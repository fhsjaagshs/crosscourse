/////////////////////////////////////////////////////////////////////////////
//
//  JFRWebSocket.m
//
//  Created by Austin and Dalton Cherry on 5/13/14.
//  Copyright (c) 2014 Vluxe. All rights reserved.
//  Modified 2016 by Nathaniel Symer
//
/////////////////////////////////////////////////////////////////////////////

#import "JFRWebSocket.h"

//get the opCode from the packet
typedef NS_ENUM(NSUInteger, JFROpCode) {
    JFROpCodeContinueFrame = 0x0,
    JFROpCodeTextFrame = 0x1,
    JFROpCodeBinaryFrame = 0x2,
    //3-7 are reserved.
    JFROpCodeConnectionClose = 0x8,
    JFROpCodePing = 0x9,
    JFROpCodePong = 0xA,
    //B-F reserved.
};

typedef NS_ENUM(NSUInteger, JFRCloseCode) {
    JFRCloseCodeNormal                 = 1000,
    JFRCloseCodeGoingAway              = 1001,
    JFRCloseCodeProtocolError          = 1002,
    JFRCloseCodeProtocolUnhandledType  = 1003,
    // 1004 reserved.
    JFRCloseCodeNoStatusReceived       = 1005,
    // 1006 reserved.
    JFRCloseCodeEncoding               = 1007,
    JFRCloseCodePolicyViolated         = 1008,
    JFRCloseCodeMessageTooBig          = 1009
};

typedef NS_ENUM(NSUInteger, JFRInternalErrorCode) {
    // 0-999 WebSocket status codes not used
    JFROutputStreamWriteError  = 1
};

// represents a response frame
@interface JFRResponse : NSObject

@property (nonatomic,assign) BOOL isFin;
@property (nonatomic,assign) JFROpCode code;
@property (nonatomic,assign) NSInteger bytesLeft;
@property (nonatomic,assign) NSInteger frameCount;
@property (nonatomic,strong) NSMutableData *buffer;

@end

@implementation JFRResponse

@end

@interface JFRWebSocket ()<NSStreamDelegate>

@property (nonatomic,strong) NSURL *url;
@property (nonatomic,strong) NSInputStream *inputStream;
@property (nonatomic,strong) NSOutputStream *outputStream;
@property (nonatomic,strong) NSOperationQueue *writeQueue;
@property (nonatomic,assign) BOOL isRunLoop;
@property (nonatomic,strong) NSMutableArray *readStack;
@property (nonatomic,strong) NSMutableArray *inputQueue;
@property (nonatomic,strong) NSData *fragBuffer;
@property (nonatomic,strong) NSMutableDictionary *headers;
@property (nonatomic,strong) NSArray *optProtocols;
@property (nonatomic,assign) BOOL isCreated;
@property (nonatomic,assign) BOOL didDisconnect;
@property (nonatomic,assign) BOOL certValidated;

@end

//Constant Header Values.
static NSString *const kHeaderConnection = @"Connection";
static NSString *const kHeaderHost       = @"Host";
static NSString *const kHeaderOrigin     = @"Origin";
static NSString *const kHeaderUpgrade    = @"Upgrade";
static NSString *const kHeaderWSAccept   = @"Sec-WebSocket-Accept";
static NSString *const kHeaderWSKey      = @"Sec-WebSocket-Key";
static NSString *const kHeaderWSProtocol = @"Sec-WebSocket-Protocol";
static NSString *const kHeaderWSVersion  = @"Sec-WebSocket-Version";

//Class Constants
static char CRLFBytes[] = {'\r', '\n', '\r', '\n'};
static int BUFFER_MAX = 4096;

// This get the correct bits out by masking the bytes of the buffer.
static const uint8_t JFRFinMask        = 0x80;
static const uint8_t JFROpCodeMask     = 0x0F;
static const uint8_t JFRRSVMask        = 0x70;
static const uint8_t JFRMaskMask       = 0x80;
static const uint8_t JFRPayloadLenMask = 0x7F;
static const size_t  JFRMaxFrameSize   = 32;

@implementation JFRWebSocket

#pragma mark - NSObject

- (instancetype)init {
    if ((self = [super init])) {
        self.certValidated = NO;
        self.voipEnabled = NO;
        self.selfSignedSSL = NO;
        self.queue = dispatch_get_main_queue();
        self.readStack = [NSMutableArray new];
        self.inputQueue = [NSMutableArray new];
        self.optProtocols = @[];
    }
    return self;
}

- (instancetype)initWithURL:(NSURL *)url protocols:(NSArray *)protocols {
    if (self = [super init]) {
        self.certValidated = NO;
        self.voipEnabled = NO;
        self.selfSignedSSL = NO;
        self.queue = dispatch_get_main_queue();
        self.url = url;
        self.readStack = [NSMutableArray new];
        self.inputQueue = [NSMutableArray new];
        self.optProtocols = protocols;
    }
    
    return self;
}

- (void)dealloc {
    [self disconnect];
}

#pragma mark - Setters

- (void)setUrl:(NSURL *)url {
    NSURLComponents *comps = [NSURLComponents componentsWithString:url.absoluteString];
    if ([comps.scheme isEqualToString:@"ws"]) comps.scheme = @"http";
    else if ([comps.scheme isEqualToString:@"wss"]) comps.scheme = @"https";
    else comps.scheme = comps.scheme.lowercaseString;
        
    if (!comps.port) {
        if ([comps.scheme isEqualToString:@"https"]) comps.port = @(443);
        else comps.port = @(80);
    }

    NSLog(@"here");
    
    _url = comps.URL;
}

#pragma mark - Exposed API

- (void)connect {
    if (self.isCreated) return;
    
    __weak typeof(self) weakSelf = self;
    dispatch_async(self.queue, ^{
        weakSelf.didDisconnect = NO;
    });

    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        weakSelf.isCreated = YES;
        NSData *serializedRequest = [weakSelf createHTTPRequest];
        [self initStreamsWithData:serializedRequest];
        weakSelf.isCreated = NO;
    });
}

- (void)disconnect {
    if (!self.isCreated) return;
    [self writeError:JFRCloseCodeNormal];
    // TODO: reset websocket client object
}

- (void)writeString:(NSString *)string {
    [self dequeueWrite:[string dataUsingEncoding:NSUTF8StringEncoding]
              withCode:JFROpCodeTextFrame];
}

- (void)writePing:(NSData *)data {
    [self dequeueWrite:data
              withCode:JFROpCodePing];
}

- (void)writeData:(NSData *)data {
    [self dequeueWrite:data withCode:JFROpCodeBinaryFrame];
}

- (void)addHeader:(NSString *)value forKey:(NSString *)key {
    if (!self.headers) {
        self.headers = [NSMutableDictionary dictionary];
    }
    [self.headers setObject:value forKey:key];
}

#pragma mark - connect's internal supporting methods

// build an HTTP Request for the WebSockets HTTP handshake
- (NSData *)createHTTPRequest {
    CFHTTPMessageRef urlRequest = CFHTTPMessageCreateRequest(kCFAllocatorDefault,
                                                             CFSTR("GET"),
                                                             (__bridge CFURLRef)self.url,
                                                             kCFHTTPVersion1_1);
    CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                     (__bridge CFStringRef)kHeaderOrigin,
                                     (__bridge CFStringRef)[NSString stringWithFormat:@"%@://%@:%@",_url.scheme,_url.host,_url.port]);
    CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                     (__bridge CFStringRef)kHeaderHost,
                                     (__bridge CFStringRef)[NSString stringWithFormat:@"%@:%@",self.url.host,_url.port]);
    CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                     (__bridge CFStringRef)kHeaderWSVersion,
                                     (__bridge CFStringRef)@"13");
    CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                     (__bridge CFStringRef)kHeaderWSKey,
                                     (__bridge CFStringRef)[self generateWebSocketKey]);
    CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                     (__bridge CFStringRef)kHeaderUpgrade,
                                     (__bridge CFStringRef)@"websocket");
    CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                     (__bridge CFStringRef)kHeaderConnection,
                                     (__bridge CFStringRef)@"Upgrade");
    if (self.optProtocols.count > 0) {
        CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                         (__bridge CFStringRef)kHeaderWSProtocol,
                                         (__bridge CFStringRef)[self.optProtocols componentsJoinedByString:@","]);
    }
   
    [self.headers enumerateKeysAndObjectsUsingBlock:^(NSString *key, NSString *obj, BOOL *stop) {
        CFHTTPMessageSetHeaderFieldValue(urlRequest,(__bridge CFStringRef)key,(__bridge CFStringRef)obj);
    }];

#if defined(DEBUG)
    NSLog(@"urlRequest = \"%@\"", urlRequest);
#endif
    NSData *serializedRequest = (__bridge_transfer NSData *)(CFHTTPMessageCopySerializedMessage(urlRequest));
    CFRelease(urlRequest);
    return serializedRequest;
}

// nondeterministic: generate a random string of 16 lowercase chars, SHA1 and base64 encoded.
- (NSString *)generateWebSocketKey {
    NSMutableString *string = [NSMutableString stringWithCapacity:16];
    for (int i = 0; i < string.length; i++) {
        [string appendFormat:@"%C",(unichar)('a' + arc4random_uniform(25))];
    }
    return [[string dataUsingEncoding:NSUTF8StringEncoding]base64EncodedStringWithOptions:0];
}

// sets up our reader/writer for the TCP stream.
- (void)initStreamsWithData:(NSData *)data {
    CFReadStreamRef readStream = NULL;
    CFWriteStreamRef writeStream = NULL;
    CFStreamCreatePairWithSocketToHost(NULL, (__bridge CFStringRef)self.url.host, _url.port.intValue, &readStream, &writeStream);
    
    self.inputStream = (__bridge_transfer NSInputStream *)readStream;
    self.inputStream.delegate = self;
    self.outputStream = (__bridge_transfer NSOutputStream *)writeStream;
    self.outputStream.delegate = self;
    if ([self.url.scheme isEqualToString:@"https"]) {
        [self.inputStream setProperty:NSStreamSocketSecurityLevelNegotiatedSSL forKey:NSStreamSocketSecurityLevelKey];
        [self.outputStream setProperty:NSStreamSocketSecurityLevelNegotiatedSSL forKey:NSStreamSocketSecurityLevelKey];
    } else {
        self.certValidated = YES; // not an https session, so no need to check SSL pinning
    }
    if(self.voipEnabled) {
        [self.inputStream setProperty:NSStreamNetworkServiceTypeVoIP forKey:NSStreamNetworkServiceType];
        [self.outputStream setProperty:NSStreamNetworkServiceTypeVoIP forKey:NSStreamNetworkServiceType];
    }
    if(self.selfSignedSSL) {
        NSString *chain = (__bridge_transfer NSString *)kCFStreamSSLValidatesCertificateChain;
        NSString *peerName = (__bridge_transfer NSString *)kCFStreamSSLValidatesCertificateChain;
        NSString *key = (__bridge_transfer NSString *)kCFStreamPropertySSLSettings;
        NSDictionary *settings = @{chain: [[NSNumber alloc] initWithBool:NO],
                                   peerName: [NSNull null]};
        [self.inputStream setProperty:settings forKey:key];
        [self.outputStream setProperty:settings forKey:key];
    }
    self.isRunLoop = YES;
    [self.inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.inputStream open];
    [self.outputStream open];
    size_t dataLen = [data length];
    [self.outputStream write:[data bytes] maxLength:dataLen];
    while (self.isRunLoop) {
        [[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]];
    }
}

#pragma mark - NSStreamDelegate

- (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)eventCode {
    if(self.security && !self.certValidated && (eventCode == NSStreamEventHasBytesAvailable || eventCode == NSStreamEventHasSpaceAvailable)) {
        SecTrustRef trust = (__bridge SecTrustRef)([aStream propertyForKey:(__bridge_transfer NSString *)kCFStreamPropertySSLPeerTrust]);
        NSString *domain = [aStream propertyForKey:(__bridge_transfer NSString *)kCFStreamSSLPeerName];
        
        if ([self.security isValid:trust domain:domain]) {
            self.certValidated = YES;
        } else {
            [self disconnectStream:[self errorWithDetail:@"Invalid SSL certificate" code:1]];
            CFRelease(trust);
            return;
        }
        CFRelease(trust);
    }
    switch (eventCode) {
        case NSStreamEventNone: break;
        case NSStreamEventOpenCompleted: break;
        case NSStreamEventHasSpaceAvailable: break;
        case NSStreamEventHasBytesAvailable: {
            if (aStream == self.inputStream) [self processInputStream];
            break;
        }
        case NSStreamEventErrorOccurred: {
            [self disconnectStream:[aStream streamError]];
            break;
        }
        case NSStreamEventEndEncountered: {
            [self disconnectStream:nil];
            break;
        }
        default: break;
    }
}

- (void)disconnectStream:(NSError *)error {
    [self.writeQueue waitUntilAllOperationsAreFinished];
    [self.inputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.outputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.outputStream close];
    [self.inputStream close];
    self.outputStream = nil;
    self.inputStream = nil;
    self.isRunLoop = NO;
    _isConnected = NO;
    self.certValidated = NO;
    [self doDisconnect:error];
}

#pragma mark - Stream Processing Methods

- (void)processInputStream {
    @autoreleasepool {
        uint8_t buffer[BUFFER_MAX];
        NSInteger length = [self.inputStream read:buffer maxLength:BUFFER_MAX];
        if (length > 0) {
            if (!self.isConnected) {
                CFIndex responseStatusCode;
                BOOL status = [self processHTTP:buffer length:length responseStatusCode:&responseStatusCode];
#if defined(DEBUG)
                if (length < BUFFER_MAX) buffer[length] = 0x00;
                else buffer[BUFFER_MAX - 1] = 0x00;
                NSLog(@"response (%ld) = \"%s\"", responseStatusCode, buffer);
#endif
                if (status == NO) {
                    [self doDisconnect:[self errorWithDetail:@"Invalid HTTP upgrade" code:1 userInfo:@{@"HTTPResponseStatusCode" : @(responseStatusCode)}]];
                }
            } else {
                BOOL process = self.inputQueue.count == 0;
                [self.inputQueue addObject:[NSData dataWithBytes:buffer length:length]];
                if (process) [self dequeueInput];
            }
        }
    }
}

- (void)dequeueInput {
    if(self.inputQueue.count > 0) {
        NSMutableData *work = [NSMutableData data];
        
        if (self.fragBuffer) {
            [work appendData:self.fragBuffer];
            self.fragBuffer = nil;
        }
        
        [work appendData:[self.inputQueue objectAtIndex:0]];
        
        [self processRawMessage:(uint8_t *)work.bytes length:work.length];
        [self.inputQueue removeObjectAtIndex:0];
        [self dequeueInput];
    }
}

#pragma mark - HTTP logic

// Finds the HTTP Packet in the TCP stream, by looking for the CRLF.
- (BOOL)processHTTP:(uint8_t*)buffer length:(NSInteger)bufferLen responseStatusCode:(CFIndex*)responseStatusCode {
    int k = 0;
    NSInteger totalSize = 0;
    for (int i = 0; i < bufferLen; i++) {
        if (buffer[i] == CRLFBytes[k]) {
            k++;
            if (k == 3) {
                totalSize = i+1;
                break;
            }
        } else {
            k = 0;
        }
    }
    
    if (totalSize > 0) {
        if ([self validateResponse:buffer length:totalSize responseStatusCode:responseStatusCode]) {
            _isConnected = YES;
            __weak typeof(self) weakSelf = self;
            dispatch_async(self.queue,^{
                if ([weakSelf.delegate respondsToSelector:@selector(websocketDidConnect:)]) {
                    [weakSelf.delegate websocketDidConnect:weakSelf];
                }
                if (weakSelf.onConnect) {
                    weakSelf.onConnect();
                }
            });
            totalSize += 1; // skip the last \n
            
            if (bufferLen > totalSize) {
                [self processRawMessage:(buffer+totalSize) length:bufferLen-totalSize];
            }
            return YES;
        }
        return NO;
    }
    return NO;
}

// Validate the HTTP rseponse is a 101, as per the RFC spec.
- (BOOL)validateResponse:(uint8_t *)buffer length:(NSInteger)bufferLen responseStatusCode:(CFIndex *)responseStatusCode {
    CFHTTPMessageRef response = CFHTTPMessageCreateEmpty(kCFAllocatorDefault, NO);
    CFHTTPMessageAppendBytes(response, buffer, bufferLen);
    *responseStatusCode = CFHTTPMessageGetResponseStatusCode(response);
    if (!((*responseStatusCode) == 101)) {
        CFRelease(response);
        return NO;
    }
    NSDictionary *headers = (__bridge_transfer NSDictionary *)(CFHTTPMessageCopyAllHeaderFields(response));
    NSString *acceptKey = headers[kHeaderWSAccept];
    CFRelease(response);
    if (acceptKey.length > 0) {
        return YES;
    }
    return NO;
}

// TODO: revitalize this
- (void)processRawMessage:(uint8_t *)buffer length:(NSInteger)bufferLen {
    JFRResponse *response = [self.readStack lastObject];
    if(response && bufferLen < 2) {
        self.fragBuffer = [NSData dataWithBytes:buffer length:bufferLen];
        return;
    }
    if (response.bytesLeft > 0) {
        NSInteger len = response.bytesLeft;
        NSInteger extra =  bufferLen - response.bytesLeft;
        if (response.bytesLeft > bufferLen) {
            len = bufferLen;
            extra = 0;
        }
        response.bytesLeft -= len;
        [response.buffer appendData:[NSData dataWithBytes:buffer length:len]];
        [self processResponse:response];
        NSInteger offset = bufferLen - extra;
        if(extra > 0) {
            [self processExtra:(buffer+offset) length:extra];
        }
        return;
    } else {
        if(bufferLen < 2) { // we need at least 2 bytes for the header
            self.fragBuffer = [NSData dataWithBytes:buffer length:bufferLen];
            return;
        }
        BOOL isFin = (JFRFinMask & buffer[0]);
        uint8_t receivedOpcode = (JFROpCodeMask & buffer[0]);
        BOOL isMasked = (JFRMaskMask & buffer[1]);
        uint8_t payloadLen = (JFRPayloadLenMask & buffer[1]);
        NSInteger offset = 2; //how many bytes do we need to skip for the header
        if ((isMasked  || (JFRRSVMask & buffer[0])) && receivedOpcode != JFROpCodePong) {
            [self doDisconnect:[self errorWithDetail:@"masked and rsv data is not currently supported" code:JFRCloseCodeProtocolError]];
            [self writeError:JFRCloseCodeProtocolError];
            return;
        }
        
        if (receivedOpcode != JFROpCodeConnectionClose &&
            receivedOpcode != JFROpCodePing &&
            receivedOpcode != JFROpCodePong &&
            receivedOpcode != JFROpCodeBinaryFrame &&
            receivedOpcode != JFROpCodeTextFrame &&
            receivedOpcode != JFROpCodeContinueFrame) {
            [self doDisconnect:[self errorWithDetail:[NSString stringWithFormat:@"unknown opcode: 0x%x",receivedOpcode] code:JFRCloseCodeProtocolError]];
            [self writeError:JFRCloseCodeProtocolError];
            return;
        }
        
        BOOL isControlFrame = (receivedOpcode == JFROpCodeConnectionClose || receivedOpcode == JFROpCodePing);
        

        if (isControlFrame && !isFin) {
            [self doDisconnect:[self errorWithDetail:@"control frames can't be fragmented" code:JFRCloseCodeProtocolError]];
            [self writeError:JFRCloseCodeProtocolError];
            return;
        }
        
        if (isControlFrame && payloadLen > 125) {
            [self doDisconnect:[self errorWithDetail:@"control frames payload is too long" code:JFRCloseCodeProtocolError]];
            [self writeError:JFRCloseCodeProtocolError];
            return;
        }
        
        if(receivedOpcode == JFROpCodeConnectionClose) {
            //the server disconnected us
            uint16_t code = JFRCloseCodeNormal;
            if(payloadLen == 1) {
                code = JFRCloseCodeProtocolError;
            }
            else if(payloadLen > 1) {
                code = CFSwapInt16BigToHost(*(uint16_t *)(buffer+offset) );
                if(code < 1000 || (code > 1003 && code < 1007) || (code > 1011 && code < 3000)) {
                    code = JFRCloseCodeProtocolError;
                }
                offset += 2;
            }
            
            if(payloadLen > 2) {
                NSInteger len = payloadLen-2;
                if(len > 0) {
                    NSData *data = [NSData dataWithBytes:(buffer+offset) length:len];
                    NSString *str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
                    if(!str) {
                        code = JFRCloseCodeProtocolError;
                    }
                }
            }
            NSLog(@"DISCONNECTED");
            // TODO: read the error message from the stream
            [self writeError:code];
            [self doDisconnect:nil];
            return;
        }
        
        NSInteger dataLength = payloadLen;
        if(payloadLen == 127) {
            dataLength = (NSInteger)CFSwapInt64BigToHost(*(uint64_t *)(buffer+offset));
            offset += sizeof(uint64_t);
        } else if(payloadLen == 126) {
            dataLength = CFSwapInt16BigToHost(*(uint16_t *)(buffer+offset) );
            offset += sizeof(uint16_t);
        }
        if(bufferLen < offset) { // we cannot process this yet, nead more header data
            self.fragBuffer = [NSData dataWithBytes:buffer length:bufferLen];
            return;
        }
        NSInteger len = dataLength;
        if (dataLength > (bufferLen-offset) || (bufferLen - offset) < dataLength) {
            len = bufferLen-offset;
        }
        NSData *data = nil;
        if (len < 0) {
            len = 0;
            data = [NSData data];
        } else {
            data = [NSData dataWithBytes:(buffer+offset) length:len];
        }
        if(receivedOpcode == JFROpCodePong) {
            NSInteger step = (offset+len);
            NSInteger extra = bufferLen-step;
            if (extra > 0) [self processRawMessage:(buffer+step) length:extra];
            return;
        }
        JFRResponse *response = [self.readStack lastObject];
        if (isControlFrame) response = nil; //don't append pings
        if (!isFin && receivedOpcode == JFROpCodeContinueFrame && !response) {
            [self doDisconnect:[self errorWithDetail:@"continue frame before a binary or text frame" code:JFRCloseCodeProtocolError]];
            [self writeError:JFRCloseCodeProtocolError];
            return;
        }
        BOOL isNew = NO;
        if (!response) {
            if(receivedOpcode == JFROpCodeContinueFrame) {
                [self doDisconnect:[self errorWithDetail:@"first frame can't be a continue frame" code:JFRCloseCodeProtocolError]];
                [self writeError:JFRCloseCodeProtocolError];
                return;
            }
            isNew = YES;
            response = [JFRResponse new];
            response.code = receivedOpcode;
            response.bytesLeft = dataLength;
            response.buffer = [NSMutableData dataWithData:data];
        } else {
            if (receivedOpcode == JFROpCodeContinueFrame) {
                response.bytesLeft = dataLength;
            } else {
                [self doDisconnect:[self errorWithDetail:@"second and beyond of fragment message must be a continue frame" code:JFRCloseCodeProtocolError]];
                [self writeError:JFRCloseCodeProtocolError];
                return;
            }
            [response.buffer appendData:data];
        }
        response.bytesLeft -= len;
        response.frameCount++;
        response.isFin = isFin;
        if (isNew) [self.readStack addObject:response];
        
        [self processResponse:response];
        
        NSInteger step = (offset+len);
        NSInteger extra = bufferLen-step;
        if(extra > 0) {
            [self processExtra:(buffer+step) length:extra];
        }
    }
}

- (void)readClosePayload:(uint8_t *)buf code:(int *)code reason:(NSString **)reason {
    
}

- (void)processExtra:(uint8_t *)buffer length:(NSInteger)bufferLen {
    if (bufferLen < 2) {
        self.fragBuffer = [NSData dataWithBytes:buffer length:bufferLen];
    } else {
        [self processRawMessage:buffer length:bufferLen];
    }
}

- (BOOL)processResponse:(JFRResponse*)response {
    if(response.isFin && response.bytesLeft <= 0) {
        NSData *data = response.buffer;
        if(response.code == JFROpCodePing) {
            [self dequeueWrite:response.buffer withCode:JFROpCodePong];
        } else if(response.code == JFROpCodeTextFrame) {
            NSString *str = [[NSString alloc] initWithData:response.buffer encoding:NSUTF8StringEncoding];
            if(!str) {
                [self writeError:JFRCloseCodeEncoding];
                return NO;
            }
            __weak typeof(self) weakSelf = self;
            dispatch_async(self.queue,^{
                if([weakSelf.delegate respondsToSelector:@selector(websocket:didReceiveMessage:)]) {
                    [weakSelf.delegate websocket:weakSelf didReceiveMessage:str];
                }
                if(weakSelf.onText) {
                    weakSelf.onText(str);
                }
            });
        } else if(response.code == JFROpCodeBinaryFrame) {
            __weak typeof(self) weakSelf = self;
            dispatch_async(self.queue,^{
                if([weakSelf.delegate respondsToSelector:@selector(websocket:didReceiveData:)]) {
                    [weakSelf.delegate websocket:weakSelf didReceiveData:data];
                }
                if(weakSelf.onData) {
                    weakSelf.onData(data);
                }
            });
        }
        [self.readStack removeLastObject];
        return YES;
    }
    return NO;
}

- (void)dequeueWrite:(NSData *)data withCode:(JFROpCode)code {
    if (!self.isConnected) return;

    if (!self.writeQueue) {
        self.writeQueue = NSOperationQueue.new;
        self.writeQueue.maxConcurrentOperationCount = 1;
    }
    
    __weak typeof(self) weakSelf = self;
    [self.writeQueue addOperationWithBlock:^{
        if (!weakSelf || !weakSelf.isConnected) return;

        typeof(weakSelf) strongSelf = weakSelf;
        uint64_t offset = 2; // how many bytes do we need to skip for the header
        uint8_t *bytes = (uint8_t *)data.bytes;
        uint64_t dataLength = data.length;
        NSMutableData *frame = [NSMutableData dataWithLength:(NSInteger)(dataLength + JFRMaxFrameSize)];
        uint8_t *buffer = (uint8_t *)frame.mutableBytes;
        buffer[0] = JFRFinMask | code;
        if (dataLength < 126) {
            buffer[1] |= dataLength;
        } else if (dataLength <= UINT16_MAX) {
            buffer[1] |= 126;
            *((uint16_t *)(buffer + offset)) = CFSwapInt16BigToHost((uint16_t)dataLength);
            offset += sizeof(uint16_t);
        } else {
            buffer[1] |= 127;
            *((uint64_t *)(buffer + offset)) = CFSwapInt64BigToHost((uint64_t)dataLength);
            offset += sizeof(uint64_t);
        }
        
        buffer[1] |= JFRMaskMask;
        uint8_t *mask_key = (buffer + offset);
        SecRandomCopyBytes(kSecRandomDefault, sizeof(uint32_t), (uint8_t *)mask_key);
        offset += sizeof(uint32_t);
        
        for (size_t i = 0; i < dataLength; i++) {
            buffer[offset] = bytes[i] ^ mask_key[i % sizeof(uint32_t)];
            offset += 1;
        }
        
        uint64_t total = 0;
        while (!(!strongSelf.isConnected || !strongSelf.outputStream)) {
            NSInteger len = [strongSelf.outputStream write:([frame bytes]+total) maxLength:(NSInteger)(offset-total)];
            if (len < 0 || len == NSNotFound) {
                NSError *error = strongSelf.outputStream.streamError;
                if(!error) {
                    error = [strongSelf errorWithDetail:@"output stream error during write" code:JFROutputStreamWriteError];
                }
                [strongSelf doDisconnect:error];
                break;
            } else {
                total += len;
            }
            if (total >= offset) {
                break;
            }
        }
    }];
}

- (void)doDisconnect:(NSError *)error {
    if(!self.didDisconnect) {
        __weak typeof(self) weakSelf = self;
        dispatch_async(self.queue, ^{
            weakSelf.didDisconnect = YES;
            [weakSelf disconnect];
            if ([weakSelf.delegate respondsToSelector:@selector(websocketDidDisconnect:error:)]) {
                [weakSelf.delegate websocketDidDisconnect:weakSelf error:error];
            }
            if (weakSelf.onDisconnect) {
                weakSelf.onDisconnect(error);
            }
        });
    }
}

#pragma mark - Error Creation

- (NSError *)errorWithDetail:(NSString *)detail code:(NSInteger)code {
    return [self errorWithDetail:detail code:code userInfo:nil];
}

- (NSError *)errorWithDetail:(NSString *)detail code:(NSInteger)code userInfo:(NSDictionary *)userInfo {
    NSMutableDictionary *details = [NSMutableDictionary dictionary];
    details[detail] = NSLocalizedDescriptionKey;
    if (userInfo) [details addEntriesFromDictionary:userInfo];
    return [NSError errorWithDomain:@"JFRWebSocket" code:code userInfo:details];
}

#pragma mark - writeError:

- (void)writeError:(uint16_t)code {
    uint16_t buffer[1];
    buffer[0] = CFSwapInt16BigToHost(code);
    [self dequeueWrite:[NSData dataWithBytes:buffer length:sizeof(uint16_t)] withCode:JFROpCodeConnectionClose];
}

@end