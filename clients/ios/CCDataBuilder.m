//
//  CCDataBuilder.m
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import "CCDataBuilder.h"

@implementation CCDataBuilder {
    NSMutableData *_data;
}

- (instancetype)init {
    if ((self = [super init])) {
        _data = [NSMutableData data];
    }
    return self;
}

- (NSData *)data {
    return _data.copy;
}

- (void)reset {
    [_data setLength:0];
}

- (void)writeUUID:(NSUUID *)uuid {
    uuid_t uuidbuf;
    [uuid getUUIDBytes:uuidbuf];
    [_data appendBytes:uuidbuf length:16];
}

- (void)write32BitUnsignedIntegerBigEndian:(uint32_t)v {
    uint8_t *bytes = (uint8_t *)malloc(sizeof(uint8_t)*4);
    bytes[0] = (v >> 24);
    bytes[1] = (v >> 16);
    bytes[2] = (v >> 8);
    bytes[3] = v;
    
    [_data appendBytes:bytes length:4];
}

- (void)write8BitUnsignedInteger:(uint8_t)v {
    uint8_t *bytes = (uint8_t *)malloc(sizeof(uint8_t));
    bytes[0] = v;
    
    [_data appendBytes:bytes length:1];
}

- (void)writeBytes:(NSData *)data {
    [_data appendData:data];
}

@end
