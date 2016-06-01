//
//  CCWebSocket.h
//  crosscourse
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JFRSecurity.h"

// Note about errors:
// all errors' codes mimic HTTP status codes.

typedef NS_ENUM(NSUInteger, CCWebSocketState) {
    CCWebSocketStateDisconnected,
    CCWebSocketStateHandshaking,
    CCWebSocketStateConnected
};

extern NSString *const CCWebSocketErrorDomain; // errors detected by client - mimic HTTP status codes
extern NSString *const CCWebSocketServerErrorDomain; // errors detected by server - no guarantees. These come from close frames.

@protocol CCWebSocketDelegate;

@interface CCWebSocket : NSObject

- (instancetype)initWithURL:(NSURL *)url protocols:(NSArray*)protocols;

- (void)connect;
- (void)close;
- (void)closeWithError:(NSError *)error;

- (void)writeData:(NSData *)data;
- (void)writeString:(NSString *)string;

@property (nonatomic,readonly) CCWebSocketState state;

@property (nonatomic,strong) JFRSecurity *security; // Use for SSL pinning.
@property (nonatomic,weak) id<CCWebSocketDelegate>delegate;

@property (nonatomic,readonly) NSURL *url;
@property (nonatomic,readonly) NSArray *protocols;
@property (nonatomic,readonly) NSArray *availableProtocols;

@end

@protocol CCWebSocketDelegate <NSObject>
@optional
- (void)websocketDidConnect:(CCWebSocket *)socket;
- (void)websocketDidDisconnect:(CCWebSocket *)socket error:(NSError *)error; // error is nil on non-erroneous disconnect
- (void)websocket:(CCWebSocket *)socket didReceiveUTF8:(NSString *)string;
- (void)websocket:(CCWebSocket *)socket didReceiveData:(NSData *)data;
@end
