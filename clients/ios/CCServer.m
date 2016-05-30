//
//  CCServer.m
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import "CCServer.h"
#import "JFRWebSocket.h"

// append an opcode to payload data.
NSData *cc_command(uint8_t opcode,NSData *d) {
    uint8_t *buf = (uint8_t *)malloc(sizeof(uint8_t)*(d.length+1));
    buf[0] = opcode;
    memcpy(buf+1, d.bytes, d.length);
    return [NSData dataWithBytesNoCopy:buf length:(d.length+1) freeWhenDone:YES];
}

// convert a NSUUID to an NSData.
NSData *uuidData(NSUUID *uuid) {
    uuid_t uuidbuf;
    [uuid getUUIDBytes:uuidbuf];
    return [NSData dataWithBytes:uuidbuf length:16];
}

NSUUID *dataUUID(NSData *data) {
    if (data.length != 16) return nil;
    return [[NSUUID alloc]initWithUUIDBytes:data.bytes];
}

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
        [self.socket writeData:cc_command(0, uuidData(uuid))];
    }
}

- (void)createChat:(NSArray<NSUUID *> *)uuids {
    if (self.socket.isConnected) {
        NSMutableData *d = [NSMutableData data];
        uint8_t count = (uint8_t)uuids.count;
        [d appendBytes:&count length:1];
        for (NSUUID *u in uuids) {
            [d appendData:uuidData(u)];
        }
        NSLog(@"sending: %@",cc_command(4,d));
        [self.socket writeData:cc_command(4,d)];
    }
}

#pragma mark - CrossCourseProtocol Response Parsing

- (void)evaluateResponse:(NSData *)data {
    if (data.length > 0) {
        uint8_t *buf = (uint8_t *)data.bytes;
        
        uint8_t opcode = buf[0];
        
        switch (opcode) {
            case 0: {
                _authenticatedUser = [[NSUUID alloc]initWithUUIDBytes:buf+1];
                if (self.onAuthenticated) {
                    self.onAuthenticated();
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
    NSLog(@"Received data: %@",data);
    [self evaluateResponse:data];
}

@end
