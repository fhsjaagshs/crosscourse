//
//  CCFrame.h
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "CCDataReader.h"

typedef NS_ENUM(NSUInteger, CCFrameType) {
    CCFrameTypeContinue = 0x0,
    CCFrameTypeText = 0x1,
    CCFrameTypeBinary = 0x2,
    //3-7 are reserved.
    CCFrameTypeConnectionClose = 0x8,
    CCFrameTypePing = 0x9,
    CCFrameTypePong = 0xA,
    //B-F reserved.
};

extern NSString *const CCFrameReadException;

@interface CCFrame : NSObject

@property (nonatomic,assign) BOOL fin;
@property (nonatomic,assign) BOOL rsv1;
@property (nonatomic,assign) BOOL rsv2;
@property (nonatomic,assign) BOOL rsv3;
@property (nonatomic,assign) CCFrameType type;
@property (nonatomic,strong) NSData *mask;
@property (nonatomic,strong) NSData *payload;

- (BOOL)isMasked;
- (void)demask;

- (NSData *)serialize;
+ (CCFrame *)read:(CCDataReader *)r;

- (instancetype)initWithType:(CCFrameType)type;
+ (CCFrame *)frameWithType:(CCFrameType)type;

@end
