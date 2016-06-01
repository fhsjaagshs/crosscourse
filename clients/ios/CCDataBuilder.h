//
//  CCDataBuilder.h
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface CCDataBuilder : NSObject

- (NSData *)data;
- (void)reset;

- (void)writeUUID:(NSUUID *)uuid;
- (void)write64BitUnsignedIntegerBigEndian:(uint64_t)v;
- (void)write32BitUnsignedIntegerBigEndian:(uint32_t)v;
- (void)write16BitUnsignedIntegerBigEndian:(uint16_t)v;
- (void)write8BitUnsignedInteger:(uint8_t)v;
- (void)writeBytes:(NSData *)data;

@end
