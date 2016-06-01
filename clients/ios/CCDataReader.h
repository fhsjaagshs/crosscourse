//
//  DataReader.h
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import <Foundation/Foundation.h>

extern NSString * const CCDataReaderException;
extern NSString * const CCDataReaderLengthException;
extern NSString * const CCDataReaderTransactionException;

@interface CCDataReader : NSObject <NSCopying>

- (instancetype)initWithData:(NSMutableData *)data;
+ (CCDataReader *)dataReaderWithData:(NSMutableData *)data;

- (void)bufferFrom:(NSInputStream *)s;

- (BOOL)inTransaction;
- (void)beginTransaction;
- (void)commitTransaction;
- (void)rollbackTransaction;

- (BOOL)hasBytes;

- (NSData *)popBytes:(NSUInteger)count;
- (NSData *)takeAll;

- (void)reset;

- (void)exception:(NSString *)reason;

- (void)matchUTF8String:(NSString *)string;
- (BOOL)lookaheadMatchUTF8String:(NSString *)string;
- (NSData *)readLine;
- (NSData *)readCRLFLine;

- (NSData *)takeWhile:(BOOL(^)(uint8_t))p;

- (NSUUID *)readUUID;
- (uint8_t)read8BitUnsignedInteger;
- (uint32_t)read16BitUnsignedIntegerBigEndian;
- (uint32_t)read32BitUnsignedIntegerBigEndian;
- (uint64_t)read64BitUnsignedIntegerBigEndian;

@end
