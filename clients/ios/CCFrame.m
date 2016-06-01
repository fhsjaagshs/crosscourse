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

- (void)demask {
    if ([self isMasked]) {
        // TODO: implement me!
        //frameUnmaskedPayload :: Frame -> BL.ByteString
        //frameUnmaskedPayload (Frame _ _ _ _ _ Nothing pl) = pl
        //frameUnmaskedPayload (Frame _ _ _ _ _ (Just mask) pl) = snd $ BL.mapAccumL f 0 pl
        //where f !i !c = ((i + 1) `mod` (B.length mask), (B.index mask i) `xor` c)
    }
}

#pragma mark - Data Representations

// deterministic, stateful
// serialize @self@ into a binary WebSocket frame
- (NSData *)serialize {
    CCDataBuilder *b = CCDataBuilder.new;
    
    [b write8BitUnsignedInteger:(self.fin << 7) | (self.rsv1 << 6) | (self.rsv2 << 5) | (self.rsv3 << 4) | self.type];
    
    unsigned long len = self.payload.length;
    
    if (len < 126) {
        [b write8BitUnsignedInteger:([self isMasked] << 7) | ((uint8_t)len)];
    } else if (len < 0x10000) {
        [b write8BitUnsignedInteger:([self isMasked] << 7) | ((uint8_t)126)];
        [b write16BitUnsignedIntegerBigEndian:(uint16_t)len];
    } else {
        [b write8BitUnsignedInteger:([self isMasked] << 7) | ((uint8_t)127)];
        [b write64BitUnsignedIntegerBigEndian:(uint64_t)len];
    }
    
    if ([self isMasked]) {
        [b writeBytes:self.mask];
        
        uint8_t *buf = (uint8_t *)malloc(sizeof(uint8_t)*self.payload.length);
        
        for (size_t i = 0; i < self.payload.length; i++) {
            buf[i] = (((uint8_t *)self.payload.bytes)[i]) ^ (((uint8_t *)self.mask.bytes)[i%sizeof(uint32_t)]);
        }
        
        [b writeBytes:[NSData dataWithBytes:buf length:self.payload.length]];
    } else {
        [b writeBytes:self.payload];
    }

    return b.data;
}

// deterministic, stateful
// read a frame from a CCDataReader
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
