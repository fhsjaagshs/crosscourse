//
//  CCServer.h
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import <Foundation/Foundation.h>

typedef void (^ErrorHandler)(NSError *error);
typedef void (^Handler)();
typedef void (^UserChatHandler)(NSUUID *user,NSUUID *chat);
typedef void (^ChatHandler)(NSUUID *chat);
typedef void (^MessageHandler)(NSUUID *sender,NSUUID *chat,uint8_t kind,NSData *message);

@interface CCServer : NSObject

@property (nonatomic,readonly) NSUUID *authenticatedUser;

@property (nonatomic,copy) ErrorHandler onError;
@property (nonatomic,copy) Handler onConnected;
@property (nonatomic,copy) Handler onDisconnect;
@property (nonatomic,copy) Handler onAuthenticated;
@property (nonatomic,copy) UserChatHandler onStartTyping;
@property (nonatomic,copy) UserChatHandler onStopTyping;
@property (nonatomic,copy) MessageHandler onMessage;
@property (nonatomic,copy) ChatHandler onChatInclusion;

- (instancetype)initWithURL:(NSURL *)url;

- (void)connect;
- (void)disconnect;

- (void)authenticate:(NSUUID *)uuid;
- (void)startedTyping:(NSUUID *)chat;
- (void)stopTyping:(NSUUID *)chat;
- (void)sendMessage:(NSString *)message to:(NSUUID *)recipient kind:(uint8_t)kind;
- (void)createChat:(NSArray<NSUUID *> *)uuids;

@end
