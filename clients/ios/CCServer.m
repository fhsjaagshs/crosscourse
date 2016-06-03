//
//  CCServer.m
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import "CCServer.h"
#import "CCWebSocket.h"
#import "CCDataReader.h"
#import "CCDataBuilder.h"

@interface CCServer () <CCWebSocketDelegate>

@property (nonatomic,strong) CCWebSocket *socket;

@end

@implementation CCServer

#pragma mark - Initialization

- (instancetype)initWithURL:(NSURL *)url {
    if ((self = [super init])) {
        _socket = [[CCWebSocket alloc]initWithURL:url protocols:@[@"crosscourse"]];
        _socket.delegate = self;
    }
    return self;
}

#pragma mark - Server Connectivity

- (void)connect {
    [_socket connect];
}

- (void)disconnect {
    [_socket close];
    _authenticatedUser = nil;
}

#pragma mark - CrossCourse Protocol Actions

- (void)authenticate:(NSUUID *)uuid {
    if (_socket.state == CCWebSocketStateConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:0];
        [b writeUUID:uuid];
        [self.socket writeData:b.data];
    }
}

- (void)startedTyping:(NSUUID *)chat {
    if (_socket.state == CCWebSocketStateConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:1];
        [b writeUUID:chat];
        [self.socket writeData:b.data];
    }
}

- (void)stopTyping:(NSUUID *)chat {
    if (_socket.state == CCWebSocketStateConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:2];
        [b writeUUID:chat];
        [self.socket writeData:b.data];
    }
}

- (void)sendMessage:(NSData *)message to:(NSUUID *)recipient kind:(uint8_t)kind {
    if (_socket.state == CCWebSocketStateConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:3];
        [b writeUUID:recipient];
        [b write8BitUnsignedInteger:kind];
        [b write32BitUnsignedIntegerBigEndian:(uint32_t)message.length];
        [b writeBytes:message];
        [self.socket writeData:b.data];
    }
}

- (void)createChat:(NSArray<NSUUID *> *)uuids {
    if (_socket.state == CCWebSocketStateConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:4];
        [b write32BitUnsignedIntegerBigEndian:(uint32_t)uuids.count];
        
        for (NSUUID *u in uuids) {
            [b writeUUID:u];
        }

        [self.socket writeData:b.data];
    }
}

#pragma mark - CrossCourseProtocol Response Parsing

- (void)evaluateResponse:(NSData *)data {
    if (data.length > 0) {
        CCDataReader *r = [CCDataReader dataReaderWithData:data.mutableCopy];
        @try {
            switch ([r read8BitUnsignedInteger]) {
                case 0: {
                    _authenticatedUser = [r readUUID];
                    if (self.onAuthenticated) {
                        self.onAuthenticated();
                    }
                    break;
                }
                case 1: {
                    if (self.onStartTyping) {
                        self.onStartTyping([r readUUID],[r readUUID]);
                    }
                    break;
                }
                case 2: {
                    if (self.onStopTyping) {
                        self.onStopTyping([r readUUID],[r readUUID]);
                    }
                    break;
                }
                case 3: {
                    if (self.onMessage) {
                        self.onMessage([r readUUID],[r readUUID],[r read8BitUnsignedInteger],[r popBytes:[r read32BitUnsignedIntegerBigEndian]]);
                    }
                    break;
                }
                case 4: {
                    if (self.onChatInclusion) {
                        self.onChatInclusion([r readUUID]);
                    }
                    break;
                }
                default: {
                    if (self.onError) {
                        self.onError([NSError errorWithDomain:@"crosscourse" code:0 userInfo:@{NSLocalizedDescriptionKey: @"Invalid opcode."}]);
                    }
                    break;
                }
            }
        } @catch (NSException *e) {
            [self.socket close];
            if (self.onError) {
                self.onError([NSError errorWithDomain:e.name code:1 userInfo:@{NSLocalizedDescriptionKey: e.reason}]);
            }
        }
    }
}

#pragma mark - CCWebSocketDelegate

- (void)websocketDidConnect:(CCWebSocket *)socket {
    if (![socket.availableProtocols containsObject:@"crosscourse"]) {
        _authenticatedUser = nil;
        [socket closeWithError:[NSError errorWithDomain:@"crosscourse" code:1 userInfo:@{NSLocalizedDescriptionKey: @"server doesn't support the crosscourse protocol."}]];
    } else {
        if (self.onConnected) {
            self.onConnected();
        }
    }
}

- (void)websocketDidDisconnect:(CCWebSocket *)socket error:(NSError *)error {
    [self disconnect];
    
    if (error) {
        if (self.onError) {
            self.onError(error);
        }
    } else {
        if (self.onDisconnect) {
            self.onDisconnect();
        }
    }
}

- (void)websocket:(CCWebSocket *)socket didReceiveData:(NSData *)data {
    [self evaluateResponse:data];
}

@end
