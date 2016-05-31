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

/*
 TODO:
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
@property (nonatomic,strong) CCDataBuilder *outputBuilder;

@property (nonatomic,strong) NSInputStream *inputStream;
@property (nonatomic,strong) NSOutputStream *outputStream;

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
                
                NSData *data = [weakself createHTTPRequest];
                [weakself.outputStream write:data.bytes maxLength:data.length];
                
                while (self.state != CCWebSocketStateDisconnected) {
                    [[NSRunLoop currentRunLoop]runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]];
                }
            }
        });
    }
}

- (void)disconnect {
    
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

- (BOOL)isSecure {
    return [self.url.scheme isEqualToString:@"https"];
}

#pragma mark - Writing

- (void)writeFrame:(CCFrame *)frame {
    
}

#pragma mark - URLs

#pragma mark - HTTP

// nondeterministic: build an HTTP Request for the WebSockets HTTP handshake
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
    if (self.protocols.count > 0) {
        CFHTTPMessageSetHeaderFieldValue(urlRequest,
                                         (__bridge CFStringRef)kHeaderWSProtocol,
                                         (__bridge CFStringRef)[self.protocols componentsJoinedByString:@","]);
    }

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

#pragma mark - Frames

- (void)processFrame:(CCFrame *)frame {
    // TODO: process frame
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

#pragma mark - Sockets & Streams

// sets up our reader/writer for the TCP stream.
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

// nondeterministic: process data in self.inputReader
- (void)processInput {
    if (self.state == CCWebSocketStateConnected) {
        // read frames
        while ([self.inputReader hasBytes]) {
            CCFrame *f = nil;
            @try {
                [self.inputReader beginTransaction];
                f = [CCFrame read:self.inputReader];
                [self.inputReader commitTransaction];
            } @catch (NSException *e) {
                if ([e.name isEqualToString:CCDataReaderException]) {
                    return; // double check this
                } else {
                    @throw e;
                }
            }
            if (f) [self processFrame:f];
        }
    } else if (self.state == CCWebSocketStateHandshaking) {
        // read HTTP response
        // CFIndex responseStatusCode;
        // if (![self processHTTP:buffer length:length responseStatusCode:&responseStatusCode];) {
        //    [self doDisconnect:[self errorWithDetail:@"Invalid HTTP upgrade" code:1 userInfo:@{@"HTTPResponseStatusCode" : @(responseStatusCode)}]];
        // }
    }
}

//- (void)disconnectStream:(NSError *)error {
//    [self.writeQueue waitUntilAllOperationsAreFinished];
//    [self.inputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
//    [self.outputStream removeFromRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
//    [self.outputStream close];
//    [self.inputStream close];
//    self.outputStream = nil;
//    self.inputStream = nil;
//    _isConnected = NO;
//    self.certValidated = NO;
//    [self doDisconnect:error];
//}

#pragma mark - NSStreamDelegate

- (void)stream:(NSStream *)aStream handleEvent:(NSStreamEvent)eventCode {
    switch (eventCode) {
        case NSStreamEventNone: break;
        case NSStreamEventOpenCompleted: break;
        case NSStreamEventHasSpaceAvailable: {
            if (self.security && ![self validateCertificate:aStream]) {
                //[self disconnectStream:[self errorWithDetail:@"Invalid SSL certificate" code:1]];
            } else {
                NSData *outputData = [self.outputBuilder data];
                [self.outputBuilder reset];
                [self.outputStream write:outputData.bytes maxLength:outputData.length];
            }
            break;
        }
        case NSStreamEventHasBytesAvailable: {
            if (self.security && ![self validateCertificate:aStream]) {
                //[self disconnectStream:[self errorWithDetail:@"Invalid SSL certificate" code:1]];
            } else if (aStream == self.inputStream) {
                [self.inputReader bufferFrom:self.inputStream];
                [self processInput];
            }
            break;
        }
        case NSStreamEventErrorOccurred: {
           // [self disconnectStream:aStream.streamError];
            break;
        }
        case NSStreamEventEndEncountered: {
           // [self disconnectStream:nil];
            break;
        }
        default: break;
    }
}

@end
