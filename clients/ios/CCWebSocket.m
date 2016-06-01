//
//  CCWebSocket.m
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import "CCWebSocket.h"
#import "CCFrame.h"
#import "CCDataReader.h"
#import "CCDataBuilder.h"

NSString *const CCWebSocketErrorDomain = @"CCWebSocketErrorDomain";
NSString *const CCWebSocketServerErrorDomain = @"CCWebSocketServerErrorDomain";

/*
 TODO:
 - frame write queuing
 - proper ponging
 - SSL/TLS
 */

static NSString *const kHeaderConnection = @"Connection";
static NSString *const kHeaderHost       = @"Host";
static NSString *const kHeaderOrigin     = @"Origin";
static NSString *const kHeaderUpgrade    = @"Upgrade";
static NSString *const kHeaderWSAccept   = @"Sec-WebSocket-Accept";
static NSString *const kHeaderWSKey      = @"Sec-WebSocket-Key";
static NSString *const kHeaderWSProtocol = @"Sec-WebSocket-Protocol";
static NSString *const kHeaderWSVersion  = @"Sec-WebSocket-Version";

@interface CCWebSocket () <NSStreamDelegate>

@property (nonatomic,strong) CCDataReader *inputReader;

@property (nonatomic,strong) NSInputStream *inputStream;
@property (nonatomic,strong) NSOutputStream *outputStream;

@property (nonatomic,strong) CCFrame *continuing;

@end

@implementation CCWebSocket

#pragma mark - Initialization

- (instancetype)initWithURL:(NSURL *)url protocols:(NSArray *)protocols {
    if ((self = [super init])) {
        _url = url;
        _protocols = protocols;
        _inputReader = [CCDataReader new];
    }
    return self;
}

#pragma mark - Exposed API

- (void)connect {
    if (self.state == CCWebSocketStateDisconnected) {
        _state = CCWebSocketStateHandshaking;
        __weak typeof(self) weakself = self;
        dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
            @autoreleasepool {
                [weakself initStreams];

                NSData *v = [weakself createHTTPRequest];
                [weakself.outputStream write:v.bytes maxLength:v.length];
                
                while (self.state != CCWebSocketStateDisconnected) {
                    [[NSRunLoop currentRunLoop]runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]];
                }
            }
        });
    }
}

- (void)close {
    [self closeWithError:nil];
}

- (void)writeData:(NSData *)data {
    CCFrame *f = [CCFrame frameWithType:CCFrameTypeBinary];
    f.payload = data;
    [self writeFrame:f];
}

- (void)writeString:(NSString *)string {
    CCFrame *f = [CCFrame frameWithType:CCFrameTypeText];
    f.payload = [string dataUsingEncoding:NSUTF8StringEncoding]; // TODO: double check
    [self writeFrame:f];
}

- (void)writePing:(NSData *)data {
    CCFrame *f = [CCFrame frameWithType:CCFrameTypePing];
    f.payload = data;
    [self writeFrame:f];
    // TODO: expect frame back
}

#pragma mark - URLs

- (BOOL)isSecure {
    return [self.url.scheme isEqualToString:@"https"] || [self.url.scheme isEqualToString:@"wss"];
}

#pragma mark - HTTP

// nondeterministic, non-effectful
// build an HTTP Request for the WebSockets HTTP handshake
- (NSData *)createHTTPRequest {
    NSString *scheme = _url.scheme;
    if ([scheme isEqualToString:@"wss"]) scheme = @"https";
    if ([scheme isEqualToString:@"ws"]) scheme = @"http";
    
    NSNumber *port = _url.port ?: ([self isSecure] ? @443 : @80);
    
    CFHTTPMessageRef urlRequest = CFHTTPMessageCreateRequest(kCFAllocatorDefault,
                                                             CFSTR("GET"),
                                                             (__bridge CFURLRef)self.url,
                                                             kCFHTTPVersion1_1);
    
    
    
    CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                     (__bridge CFStringRef)kHeaderOrigin,
                                     (__bridge CFStringRef)[NSString stringWithFormat:@"%@://%@:%@",scheme,_url.host,_url.port]);
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
    if (self.protocols.count > 0) {
        CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                         (__bridge CFStringRef)kHeaderWSProtocol,
                                         (__bridge CFStringRef)[self.protocols componentsJoinedByString:@","]);
    }

    NSData *serializedRequest = (__bridge_transfer NSData *)(CFHTTPMessageCopySerializedMessage(urlRequest));
    CFRelease(urlRequest);
    return serializedRequest;
}

// nondeterministic, non-effectful
// generate a random string of 16 lowercase chars, SHA1 and base64 encoded.
- (NSString *)generateWebSocketKey {
    NSMutableString *string = [NSMutableString stringWithCapacity:16];
    for (int i = 0; i < 16; i++) {
        [string appendFormat:@"%C",(unichar)('a' + arc4random_uniform(25))];
    }
    return [[string dataUsingEncoding:NSUTF8StringEncoding]base64EncodedStringWithOptions:0];
}

// TODO: read status code 400 as a bad request
//
// nondeterministic, effectful
// reads HTTP headers from the HTTP handshake response.
- (NSMutableDictionary *)readHTTPResponseHeaders:(NSError **)error {
    @try {
        NSMutableDictionary *headers = [NSMutableDictionary dictionary];
        
        [self.inputReader beginTransaction];
        
        [self.inputReader matchUTF8String:@"HTTP/1.1 101 Switching Protocols\r\n"];
        
        while (![self.inputReader lookaheadMatchUTF8String:@"\r\n"]) {
            NSString *k = [[NSString alloc]initWithData:[self.inputReader takeWhile:^BOOL(uint8_t v) {
                return v != ':';
            }] encoding:NSUTF8StringEncoding];
            
            [self.inputReader matchUTF8String:@": "];
            
            NSString *v = [[NSString alloc]initWithData:[self.inputReader takeWhile:^BOOL(uint8_t v) {
                return v != '\r';
            }] encoding:NSUTF8StringEncoding];
            
            [self.inputReader matchUTF8String:@"\r\n"];
            headers[k.lowercaseString] = v;
        }
        
        [self.inputReader commitTransaction];
        
        [self.inputReader reset];
        
        return headers;
    } @catch (NSException *e) {
        if ([e.name isEqualToString:CCDataReaderException]) {
            *error = [self errorWithCode:500 message:@"Invalid WebSocket handshake response."];
            return nil;
        } else if ([e.name isEqualToString:CCDataReaderLengthException]) {
            return nil;
        } else {
            @throw e;
        }
    }
}

#pragma mark - Frames

// deterministic, effectful
// process incoming frames from a connected server
- (void)processFrame:(CCFrame *)f {
    if ([f isMasked]) {
        [self closeWithError:[self errorWithCode:500 message:@"Frames for the client should never be masked."]];
    } else if (!f.fin && f.type != CCFrameTypeContinue) {
        self.continuing = f;
    } else {
        if (f.type == CCFrameTypeContinue) {
            if (self.continuing) {
                NSMutableData *d = [self.continuing.payload mutableCopy];
                [d appendData:f.payload];
                
                CCFrame *nf = [CCFrame frameWithType:self.continuing.type];
                nf.fin = f.fin;
                nf.rsv1 = self.continuing.rsv1;
                nf.rsv2 = self.continuing.rsv2;
                nf.rsv3 = self.continuing.rsv3;
                nf.mask = self.continuing.mask.copy;
                nf.payload = d;
                
                self.continuing = nil;
                
                if (nf.fin) {
                    [self processFrame:nf];
                } else {
                    self.continuing = nf;
                }
            } else {
                [self closeWithError:[self errorWithCode:500 message:@"Invalid continuation ordering."]];
            }
        } else if (f.type == CCFrameTypeText) {
            if (self.delegate && [self.delegate respondsToSelector:@selector(websocket:didReceiveUTF8:)]) {
                [self.delegate websocket:self didReceiveUTF8:[[NSString alloc]initWithData:f.payload encoding:NSUTF8StringEncoding]];
            }
        } else if (f.type == CCFrameTypeBinary) {
            if (self.delegate && [self.delegate respondsToSelector:@selector(websocket:didReceiveData:)]) {
                [self.delegate websocket:self didReceiveData:f.payload];
            }
        } else if (f.type == CCFrameTypeConnectionClose) {
            NSError *e = nil;
            if (f.payload.length > 0) {
                @try {
                    CCDataReader *r = [CCDataReader dataReaderWithData:f.payload.mutableCopy];
                    uint16_t code = [r read16BitUnsignedIntegerBigEndian];
                    NSString *message = [[NSString alloc]initWithData:[r takeAll] encoding:NSUTF8StringEncoding];
                    e = [self serverErrorWithCode:code message:message];
                } @catch (NSException *exception) {
                    e = [self errorWithCode:500 message:@"Invalid close frame."];
                }
            }
            
            [self closeWithError:e];
        } else if (f.type == CCFrameTypePing) {
            [self closeWithError:[self errorWithCode:500 message:@"The client should never receive ping frames."]];
        } else if (f.type == CCFrameTypePong) {
            // if there was a ping, notate that a heartbeat was valid
        }
    }
}

// nondeterministic, effectful
// tries to read a frame, if it fails due to a lack of input, then
// it returns @nil@.
- (CCFrame *)readFrame {
    @try {
        [self.inputReader beginTransaction];
        CCFrame *f = [CCFrame read:self.inputReader];
        [self.inputReader commitTransaction];
        return f;
    } @catch (NSException *e) {
        if ([e.name isEqualToString:CCDataReaderLengthException]) {
            return nil;
        } else {
            @throw e;
        }
    }
}

// deterministic, effectful
// write a frame to the server over TCP
- (void)writeFrame:(CCFrame *)frame {
    NSData *d = [frame serialize];
    [self.outputStream write:d.bytes maxLength:d.length];
}

#pragma mark - SSL

// nondeterministic, no-side-effects
// Determine if an NSStream has a valid certificate.
- (BOOL)validateCertificate:(NSStream *)stream {
    SecTrustRef trust = (__bridge SecTrustRef)([stream propertyForKey:(__bridge_transfer NSString *)kCFStreamPropertySSLPeerTrust]);
    NSString *domain = [stream propertyForKey:(__bridge_transfer NSString *)kCFStreamSSLPeerName];
    
    BOOL valid = [self.security isValid:trust domain:domain];
    CFRelease(trust);
    
    return valid;
}

#pragma mark - Errors

- (NSError *)errorWithCode:(NSInteger)code message:(NSString *)message {
    return [NSError errorWithDomain:CCWebSocketErrorDomain code:code userInfo:@{NSLocalizedDescriptionKey: message, @"websocket": self}];
}

- (NSError *)serverErrorWithCode:(NSInteger)code message:(NSString *)message {
    return [NSError errorWithDomain:CCWebSocketServerErrorDomain code:code userInfo:@{NSLocalizedDescriptionKey: message, @"websocket": self}];
}


#pragma mark - Closing

- (void)closeWithError:(NSError *)error {
    if (self.state != CCWebSocketStateDisconnected) {
        if (self.state == CCWebSocketStateConnected) {
            [self writeFrame:[CCFrame frameWithType:CCFrameTypeConnectionClose]];
            
            // TODO: wait for writing to finish
        }
        
        [self deinitStreams];
        
        _availableProtocols = nil;
        _state = CCWebSocketStateDisconnected;
        
        if (self.delegate && [self.delegate respondsToSelector:@selector(websocketDidDisconnect:error:)]) {
            [self.delegate websocketDidDisconnect:self error:error];
        }
    }
}


#pragma mark - Sockets & Streams

// deterministic, effectful
// connects to the remote socket
- (void)initStreams {
    CFReadStreamRef readStream = NULL;
    CFWriteStreamRef writeStream = NULL;
    CFStreamCreatePairWithSocketToHost(NULL, (__bridge CFStringRef)self.url.host, _url.port.intValue, &readStream, &writeStream);

    self.inputStream = (__bridge_transfer NSInputStream *)readStream;
    self.inputStream.delegate = self;
    self.outputStream = (__bridge_transfer NSOutputStream *)writeStream;
    self.outputStream.delegate = self;
    
    if ([self isSecure]) {
        [self.inputStream setProperty:NSStreamSocketSecurityLevelNegotiatedSSL forKey:NSStreamSocketSecurityLevelKey];
        [self.outputStream setProperty:NSStreamSocketSecurityLevelNegotiatedSSL forKey:NSStreamSocketSecurityLevelKey];
    }
    
    [self.inputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.outputStream scheduleInRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.inputStream open];
    [self.outputStream open];
}

// deterministic, effectful
// disconnect from remote socket
- (void)deinitStreams {
    [self.inputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.outputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
    [self.outputStream close];
    [self.inputStream close];
    self.outputStream = nil;
    self.inputStream = nil;
}

// nondeterministic, effectful
// process data in self.inputReader
- (void)processInput {
    if (self.state == CCWebSocketStateConnected) {
        while ([self.inputReader hasBytes]) {
            CCFrame *f = [self readFrame];
            if (f) [self processFrame:f];
        }
    } else if (self.state == CCWebSocketStateHandshaking) {
        NSError *error = nil;
        NSMutableDictionary *headers = [self readHTTPResponseHeaders:&error];
        
        if (headers && !error) {
            if ([headers[@"upgrade"] isEqualToString:@"websocket"] &&
                [[headers [@"connection"] lowercaseString] isEqualToString:@"upgrade"] &&
                [headers[@"sec-websocket-accept"] length] > 0) { // TODO: actually check for a match.
                if (self.protocols) {
                    _availableProtocols = [headers[@"sec-websocket-protocol"] componentsSeparatedByString:@","] ?: @[];
                }
                
                _state = CCWebSocketStateConnected;
                if (self.delegate && [self.delegate respondsToSelector:@selector(websocketDidConnect:)]) {
                    [self.delegate websocketDidConnect:self];
                }
            }
        } else if (error) {
            [self closeWithError:error];
        }
    }
}

#pragma mark - NSStreamDelegate

- (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)eventCode {
    switch (eventCode) {
        case NSStreamEventNone: break;
        case NSStreamEventOpenCompleted: break;
        case NSStreamEventHasSpaceAvailable: break;
        case NSStreamEventHasBytesAvailable: {
            if (self.security && ![self validateCertificate:aStream]) {
                [self closeWithError:[self errorWithCode:400 message:@"Invalid SSL certificate."]];
            } else if (aStream == self.inputStream) {
                [self.inputReader bufferFrom:self.inputStream];
                [self processInput];
            }
            break;
        }
        case NSStreamEventErrorOccurred: {
            [self closeWithError:aStream.streamError];
            break;
        }
        case NSStreamEventEndEncountered: {
            [self closeWithError:nil];
            break;
        }
        default: break;
    }
}

@end
