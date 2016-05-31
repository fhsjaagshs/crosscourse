//
//  CCFrame.m
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import "CCFrame.h"
#import "CCDataBuilder.h"

NSString *const CCFrameReadException = @"CCFrameReadException";

NSData *generateMask() {
    uint32_t mask = arc4random_uniform(4294967295); // any 32-bit unsigned integer
    CCDataBuilder *b = CCDataBuilder.new;
    [b write32BitUnsignedIntegerBigEndian:mask];
    return b.data;
}

@implementation CCFrame

#pragma mark - Initialization

- (instancetype)initWithType:(CCFrameType)type {
    if ((self = [super init])) {
        _mask = generateMask();
        _payload = [NSData data];
        _fin = YES;
        _type = type;
    }
    return self;
}

+ (CCFrame *)frameWithType:(CCFrameType)type {
    return [[self alloc]initWithType:type];
}

#pragma mark - Masking

- (BOOL)isMasked {
    return _mask != nil;
}

#pragma mark - Data Representations

- (NSData *)serialize {
    CCDataBuilder *b = CCDataBuilder.new;
    // TODO: serialize
    return b.data;
}

+ (CCFrame *)read:(CCDataReader *)r {
    CCFrame *f = CCFrame.new;
    
    uint8_t b0 = [r read8BitUnsignedInteger];
    uint8_t b1 = [r read8BitUnsignedInteger];
    
    f.fin = (b0 & 0x80) == 0x80;
    f.rsv1 = (b0 & 0x40) == 0x40;
    f.rsv2 = (b0 & 0x20) == 0x20;
    f.rsv3 = (b0 & 0x10) == 0x10;
    f.type = b0 & 0x0f;
    
    if ((b1 & 0x80) == 0x80) f.mask = [r popBytes:4];
    
    uint8_t lenflag = b1 & 0x7f;
    
    if      (lenflag == 126) f.payload = [r popBytes:[r read16BitUnsignedIntegerBigEndian]];
    else if (lenflag == 127) f.payload = [r popBytes:[r read64BitUnsignedIntegerBigEndian]];
    else                     f.payload = [r popBytes:lenflag];
    return f;
}

@end
