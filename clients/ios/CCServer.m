//
//  CCServer.m
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import "CCServer.h"
#import "JFRWebSocket.h"
#import "CCDataReader.h"
#import "CCDataBuilder.h"

@interface CCServer () <JFRWebSocketDelegate>

@property (nonatomic,strong) JFRWebSocket *socket;

@end

@implementation CCServer

#pragma mark - Initialization

- (instancetype)initWithURL:(NSURL *)url {
    if ((self = [super init])) {
        _socket = [[JFRWebSocket alloc]initWithURL:url protocols:@[@"crosscourse"]];
        _socket.delegate = self;
    }
    return self;
}

#pragma mark - Server Connectivity

- (void)connect {
    [_socket connect];
}

- (void)disconnect {
    if (_socket.isConnected) {
        [_socket disconnect];
    }
    _authenticatedUser = nil;
}

#pragma mark - CrossCourse Protocol Actions

- (void)authenticate:(NSUUID *)uuid {
    if (self.socket.isConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:0];
        [b writeUUID:uuid];
        [self.socket writeData:b.data];
    }
}

- (void)startedTyping:(NSUUID *)chat {
    if (self.socket.isConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:1];
        [b writeUUID:chat];
        [self.socket writeData:b.data];
    }
}

- (void)stopTyping:(NSUUID *)chat {
    if (self.socket.isConnected) {
        CCDataBuilder *b = CCDataBuilder.new;
        [b write8BitUnsignedInteger:2];
        [b writeUUID:chat];
        [self.socket writeData:b.data];
    }
}

- (void)sendMessage:(NSData *)message to:(NSUUID *)recipient kind:(uint8_t)kind {
    if (self.socket.isConnected) {
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
    if (self.socket.isConnected) {
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
            [self.socket disconnect];
            if (self.onError) {
                self.onError([NSError errorWithDomain:e.name code:1 userInfo:@{NSLocalizedDescriptionKey: e.reason}]);
            }
        }
    }
}

#pragma mark - JFRWebSocketDelegate

- (void)websocketDidConnect:(JFRWebSocket *)socket {
    if (self.onConnected) {
        self.onConnected();
    }
}

- (void)websocketDidDisconnect:(JFRWebSocket *)socket error:(NSError *)error {
    [self disconnect];
    if (self.onError && error) {
        self.onError(error);
    }
    
    if (self.onDisconnect) {
        self.onDisconnect();
    }
}

- (void)websocket:(JFRWebSocket *)socket didReceiveData:(NSData *)data {
    [self evaluateResponse:data];
}

@end
