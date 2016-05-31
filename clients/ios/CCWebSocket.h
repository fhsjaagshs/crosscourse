//
//  CCWebSocket.h
//  crosscourse
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "JFRSecurity.h"

typedef NS_ENUM(NSUInteger, CCWebSocketState) {
    CCWebSocketStateDisconnected,
    CCWebSocketStateHandshaking,
    CCWebSocketStateConnected
};

@protocol CCWebSocketDelegate;

@interface CCWebSocket : NSObject

- (instancetype)initWithURL:(NSURL *)url protocols:(NSArray*)protocols;

- (void)connect;
- (void)disconnect;

- (void)writeData:(NSData *)data;
- (void)writeString:(NSString *)string;

@property (nonatomic,readonly) CCWebSocketState state;

@property (nonatomic,strong) JFRSecurity *security; // Use for SSL pinning.
@property (nonatomic,weak) id<CCWebSocketDelegate>delegate;

@property (nonatomic,readonly) NSURL *url;
@property (nonatomic,readonly) NSArray *protocols;

@end

@protocol CCWebSocketDelegate <NSObject>
@optional
- (void)websocketDidConnect:(CCWebSocket *)socket;
- (void)websocketDidDisconnect:(CCWebSocket *)socket error:(NSError *)error; // error is nil on non-erroneous disconnect
- (void)websocket:(CCWebSocket *)socket didReceiveMessage:(NSString *)string;
- (void)websocket:(CCWebSocket *)socket didReceiveData:(NSData *)data;
@end
