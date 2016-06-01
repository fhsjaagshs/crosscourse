//
//  DataReader.m
//  crosscourse-demo
//
//  Created by Nathaniel Symer on 5/30/16.
//  Copyright © 2016 Nathaniel Symer. All rights reserved.
//

#import "CCDataReader.h"

// TODO: nested transactions

NSString * const CCDataReaderException = @"CCDataReaderException";
NSString * const CCDataReaderLengthException = @"CCDataReaderLengthException";
NSString * const CCDataReaderTransactionException = @"CCDataReaderTransactionException";

static NSUInteger const kChunkLen = 4092;
#define roundn(v,n) ((v) + (n) - 1) / (n) * (n)

@interface CCDataReader ()

@property (nonatomic,strong) NSMutableData *data;
@property (nonatomic,strong) NSMutableData *rollbackData;

@end

@implementation CCDataReader

#pragma mark - Initialization

- (instancetype)init {
    if ((self = [super init])) {
        _data = [NSMutableData data];
    }
    return self;
}

- (instancetype)initWithData:(NSMutableData *)data {
    if ((self = [super init])) {
        _data = data;
    }
    return self;
}

+ (CCDataReader *)dataReaderWithData:(NSMutableData *)data {
    return [[[self class]alloc]initWithData:data];
}

#pragma mark - NSCopying

- (id)copyWithZone:(nullable NSZone *)zone {
    return [[self class]dataReaderWithData:_data];
}

#pragma mark - Exceptions

- (void)exception:(NSString *)reason {
    if ([self inTransaction]) [self rollbackTransaction];
    @throw [NSException exceptionWithName:CCDataReaderException reason:reason userInfo:@{@"datareader":self}];
}

- (void)lengthException:(NSString *)reason {
    if ([self inTransaction]) [self rollbackTransaction];
    @throw [NSException exceptionWithName:CCDataReaderLengthException reason:reason userInfo:@{@"datareader":self}];
}

- (void)transactionException:(NSString *)reason {
    @throw [NSException exceptionWithName:CCDataReaderTransactionException reason:reason userInfo:@{@"datareader":self}];
}


#pragma mark - Transactions

- (BOOL)inTransaction {
    return self.rollbackData;
}

- (void)beginTransaction {
    if ([self inTransaction]) [self transactionException:@"Cannot begin new transaction: already in transaction."];
    else                      self.rollbackData = [NSMutableData data];
}

- (void)commitTransaction {
    if (![self inTransaction]) [self transactionException:@"Cannot commit: not in transaction."];
    else                       self.rollbackData = nil;
}

- (void)rollbackTransaction {
    if (![self inTransaction]) {
        [self transactionException:@"Cannot rollback: not in transaction."];
    } else {
        NSMutableData *d = self.data.copy;
        self.data = self.rollbackData;
        [self.data appendData:d];
        self.rollbackData = nil;
    }
}

#pragma mark - Streams

- (void)bufferFrom:(NSInputStream *)s {
    uint8_t *buf = (uint8_t *)malloc(sizeof(uint8_t)*kChunkLen);
    while (1) {
        NSUInteger bytesRead = [s read:buf maxLength:kChunkLen];
        if (bytesRead > 0) [_data appendBytes:buf length:bytesRead];
        if (bytesRead < kChunkLen) break;
    }
    free(buf);
}

#pragma mark - Basic Interface

- (BOOL)hasBytes {
    return _data.length > 0;
}

- (NSData *)popBytes:(NSUInteger)count {
    if (self.data.length < count) {
        [self lengthException:@"Not enough bytes."];
        return nil;
    } else {
        NSRange r = NSMakeRange(0, count);
        NSData *popped = [self.data subdataWithRange:r].copy;
        if ([self inTransaction]) [self.rollbackData appendData:popped];
        [self.data replaceBytesInRange:r withBytes:NULL length:0];
        return popped;
    }
}

- (NSData *)takeAll {
    return [self popBytes:self.data.length];
}

- (void)reset {
    [self.data setLength:0];
    self.rollbackData = nil;
}

#pragma mark - Strings

- (void)matchUTF8String:(NSString *)string {
    NSString *read = [[NSString alloc]initWithData:[self popBytes:string.length] encoding:NSUTF8StringEncoding];
    if (![string isEqualToString:read]) {
        [self exception:@"matchUTF8String: strings don't match."];
    }
}

- (BOOL)lookaheadMatchUTF8String:(NSString *)string {
    if (self.data.length < string.length) {
        [self lengthException:@"Not enough bytes."];
        return NO;
    } else {
        NSString *read = [[NSString alloc]initWithData:[self.data subdataWithRange:NSMakeRange(0,string.length)] encoding:NSUTF8StringEncoding];
        return [string isEqualToString:read];
    }
}

- (NSData *)readLine {
    return [self takeWhile:^BOOL(uint8_t v) { return v != '\n'; }];
}

- (NSData *)readCRLFLine {
    NSData *ln = [self readLine];
    return [ln subdataWithRange:NSMakeRange(0, ln.length-1)];
}

#pragma mark - Binary

- (NSData *)takeWhile:(BOOL(^)(uint8_t))p {
    NSMutableData *d = [NSMutableData data];
    
    while (1) {
        if (self.data.length == 0) {
            [self lengthException:@"Not enough bytes."];
        }
        if (p(*(uint8_t *)(self.data.bytes))) {
            [d appendData:[self popBytes:1]];
        } else {
            break;
        }
    }
    
    return d;
}

#pragma mark - UUID

- (NSUUID *)readUUID {
    NSData *data = [self popBytes:16];
    return [[NSUUID alloc]initWithUUIDBytes:data.bytes];
}

#pragma mark - Integers

- (uint8_t)read8BitUnsignedInteger {
    NSData *data = [self popBytes:1];
    return *(uint8_t *)(data.bytes);
}

- (uint32_t)read16BitUnsignedIntegerBigEndian {
    NSData *data = [self popBytes:2];
    uint8_t *bytes = (uint8_t *)data.bytes;
    return (bytes[0] << 8) + bytes[1];
}

- (uint32_t)read32BitUnsignedIntegerBigEndian {
    NSData *data = [self popBytes:4];
    uint8_t *bytes = (uint8_t *)data.bytes;
    return (bytes[0] << 24) + (bytes[1] << 16) + (bytes[2] << 8) + bytes[3];
}

- (uint64_t)read64BitUnsignedIntegerBigEndian {
    NSData *data = [self popBytes:8];
    uint8_t *bytes = (uint8_t *)data.bytes;
    return (((uint64_t)bytes[0]) << 56) +
           (((uint64_t)bytes[1]) << 48) +
           (((uint64_t)bytes[2]) << 40) +
           (((uint64_t)bytes[3]) << 32) +
           (((uint64_t)bytes[4]) << 24) +
           (((uint64_t)bytes[5]) << 16) +
           (((uint64_t)bytes[6]) << 8) +
            ((uint64_t)bytes[7]);
}

@end
