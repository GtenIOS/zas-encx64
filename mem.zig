const std = @import("std");
const Register = @import("./reg.zig").Register;
const byte_oper = @import("common").byte;
const Imm = @import("common").imm.Imm;
const OperandEncoding = @import("opnd.zig").OperandEncoding;
const Prefix = @import("./pfx.zig").Prefix;

pub const Displacement = union(enum) {
    disp8: u8,
    disp32: u32,
    const Self = @This();

    pub inline fn encode(self: Self) ![]const u8 {
        switch (self) {
            .disp8 => |ival| return byte_oper.intToLEBytes(u8, ival),
            .disp32 => |ival| return byte_oper.intToLEBytes(u32, ival),
        }
    }

    pub inline fn is8Bit(self: Self) bool {
        switch (self) {
            .disp8 => return true,
            else => return false,
        }
    }

    pub inline fn from(imm: ?Imm) !?Self {
        if (imm) |i| {
            switch (i.size()) {
                8 => return Self{ .disp8 = i.toImm8() },
                16, 32 => return Self{ .disp32 = i.toImm32() },
                0 => return Self{ .disp32 = i.toImm32() },
                else => return error.InvalidDisplacementSize,
            }
        } else return null;
    }

    pub inline fn byteValue(self: Self) !u8 {
        switch (self) {
            .disp8 => |ofst| return ofst,
            else => return error.NotAByteOffset,
        }
    }
};

pub const MOffset = union(enum) {
    mofst8: u8,
    mofst16: u16,
    mofst32: u32,
    mofst64: u64,
    mofst: u64, // Non sized variant
    const Self = @This();

    pub fn from(imm: Imm) Self {
        return Self{ .mofst = imm.toImm64() };
    }

    pub inline fn matches(self: Self, other: Self) bool {
        return if (other.size() == 0) true else other.size() == self.size();
    }

    pub fn toMOfst(mofst: Self, dest_size: u16) !Self {
        switch (dest_size) {
            8 => return Self{ .mofst8 = mofst.toImm8() },
            16 => return Self{ .mofst16 = mofst.toImm16() },
            32 => return Self{ .mofst32 = mofst.toImm32() },
            64 => return Self{ .mofst64 = mofst.toImm64() },
            else => return error.InvalidMOffsetSize,
        }
    }

    pub inline fn encode(self: Self) ![]const u8 {
        switch (self) {
            .mofst8 => |ival| return byte_oper.intToLEBytes(u8, ival),
            .mofst16 => |ival| return byte_oper.intToLEBytes(u16, ival),
            .mofst32 => |ival| return byte_oper.intToLEBytes(u32, ival),
            .mofst64 => |ival| return byte_oper.intToLEBytes(u64, ival),
            .mofst => return error.MustBeResolvedToASizedVariant,
        }
    }

    pub inline fn toMOffset64(self: Self) Self {
        switch (self) {
            .mofst8 => |ival| return Self{ .mofst64 = ival },
            .mofst16 => |ival| return Self{ .mofst64 = ival },
            .mofst32 => |ival| return Self{ .mofst64 = ival },
            .mofst64, .mofst => return self,
        }
    }

    pub inline fn toImm8(self: Self) u8 {
        switch (self) {
            .mofst8 => |ival| return ival,
            .mofst16 => |ival| return @intCast(u8, ival & 0x00ff),
            .mofst32 => |ival| return @intCast(u8, ival & 0x000000ff),
            .mofst64, .mofst => |ival| return @intCast(u8, ival & 0x00000000000000ff),
        }
    }

    pub inline fn toImm16(self: Self) u16 {
        switch (self) {
            .mofst8 => |ival| return ival,
            .mofst16 => |ival| return ival,
            .mofst32 => |ival| return @intCast(u8, ival & 0x000000ff),
            .mofst64, .mofst => |ival| return @intCast(u8, ival & 0x00000000000000ff),
        }
    }

    pub inline fn toImm32(self: Self) u32 {
        switch (self) {
            .mofst8 => |ival| return ival,
            .mofst16 => |ival| return ival,
            .mofst32 => |ival| return ival,
            .mofst64, .mofst => |ival| return @intCast(u8, ival & 0x00000000000000ff),
        }
    }

    pub inline fn toImm64(self: Self) u64 {
        switch (self) {
            .mofst8 => |ival| return ival,
            .mofst16 => |ival| return ival,
            .mofst32 => |ival| return ival,
            .mofst64, .mofst => |ival| return ival,
        }
    }

    pub inline fn size(self: Self) u16 {
        switch (self) {
            .mofst8 => return 8,
            .mofst16 => return 16,
            .mofst32 => return 32,
            .mofst64 => return 64,
            .mofst => return 0,
        }
    }
};

pub const Rel = union(enum) {
    rel8: u8,
    rel16: u16,
    rel32: u32,
    rel: u64, // Un sized variant
    const Self = @This();

    pub fn from(imm: Imm) !Self {
        switch (imm) {
            .imm8 => return Self{ .rel8 = imm.toImm8() },
            .imm16 => return Self{ .rel16 = imm.toImm16() },
            .imm32, .imm => return Self{ .rel32 = imm.toImm32() },
            .imm64 => return error.SizeTooBigForRelativeOffset,
        }
    }

    pub inline fn matches(self: Self, other: Self) bool {
        return if (other.size() == 0) true else other.size() == self.size();
    }

    pub fn toRel(rel: Self, dest_size: u16) !Self {
        switch (dest_size) {
            8 => return Self{ .rel8 = rel.toImm8() },
            16 => return Self{ .rel16 = rel.toImm16() },
            32 => return Self{ .rel32 = rel.toImm32() },
            else => return error.InvalidRelativeOffsetSize,
        }
    }

    pub inline fn encode(self: Self) ![]const u8 {
        switch (self) {
            .rel8 => |ival| return byte_oper.intToLEBytes(u8, ival),
            .rel16 => |ival| return byte_oper.intToLEBytes(u16, ival),
            .rel32 => |ival| return byte_oper.intToLEBytes(u32, ival),
            .rel => return error.MustBeResolvedToASizedVariant,
        }
    }

    pub inline fn toMOffset64(self: Self) Self {
        switch (self) {
            .rel8 => |ival| return Self{ .rel64 = ival },
            .rel16 => |ival| return Self{ .rel64 = ival },
            .rel32, .rel => |ival| return Self{ .rel64 = ival },
        }
    }

    pub inline fn toImm8(self: Self) u8 {
        switch (self) {
            .rel8 => |ival| return ival,
            .rel16 => |ival| return @intCast(u8, ival & 0x00ff),
            .rel32 => |ival| return @intCast(u8, ival & 0x000000ff),
            .rel => |ival| return @intCast(u8, ival & 0x00000000000000ff),
        }
    }

    pub inline fn toImm16(self: Self) u16 {
        switch (self) {
            .rel8 => |ival| return ival,
            .rel16 => |ival| return ival,
            .rel32 => |ival| return @intCast(u16, ival & 0x0000ffff),
            .rel => |ival| return @intCast(u16, ival & 0x000000000000ffff),
        }
    }

    pub inline fn toImm32(self: Self) u32 {
        switch (self) {
            .rel8 => |ival| return ival,
            .rel16 => |ival| return ival,
            .rel32 => |ival| return ival,
            .rel => |ival| return @intCast(u32, ival & 0x00000000ffffffff),
        }
    }

    pub inline fn toImm64(self: Self) u64 {
        switch (self) {
            .rel8 => |ival| return ival,
            .rel16 => |ival| return ival,
            .rel32 => |ival| return ival,
            .rel => |ival| return ival,
        }
    }

    pub inline fn size(self: Self) u16 {
        switch (self) {
            .rel8 => return 8,
            .rel16 => return 16,
            .rel32 => return 32,
            .rel => return 0,
        }
    }
};

const Sib = struct {
    scale: ?u2, // takes in the power, final scale value will be pow(2, scale)
    index: ?Register,
    base: ?Register,
    displacement: ?Displacement,
    const Self = @This();

    pub inline fn isValid(self: Self) bool {
        return if (self.base) |base| blk: {
            // Valid base, index register size is either 32-bits or 64-bits
            if (base.size() != 32 and base.size() != 64) break :blk false;

            if (self.index) |index| {
                break :blk index.size() == base.size();
            }
            break :blk true;
        } else if (self.displacement != null or self.index != null) {
            return true;
        } else {
            return false;
        };
    }

    pub inline fn size(self: Self) !u16 {
        if (!self.isValid()) return error.InvalidMemoryReference;

        if (self.base) |base| {
            return base.size();
        } else if (self.index) |index| {
            return index.size();
        } else {
            // TODO: - Revise following return value
            return 64;
        }
    }

    inline fn mod(self: Self) u8 {
        if (self.displacement) |disp| {
            if (self.base) |b| {
                if (b.isRip()) return 0b00; // 32-bit displacement only mode

                switch (disp) {
                    .disp8 => return 0b01,
                    .disp32 => return 0b10,
                }
            } else {
                return 0b00;
            }
        } else {
            // For SIB with base only of `rbp`, we explicitely add `0` as
            // 1-byte offset, hence the mod bits `0b01`
            if (self.base) |b| {
                if (b.isRbp()) return 0b01;
            }
            return 0b00;
        }
    }

    inline fn rm(self: Self) !u8 {
        if (self.base) |b| {
            if (b.isRip() or b.isRbp()) return 0b101;

            const base_idx = b.idx();
            if (self.index) |_| {
                return 0b100;
            } else {
                if (base_idx < 0b011) {
                    return base_idx;
                } else if (base_idx < 0b111) {
                    return base_idx;
                } else {
                    unreachable;
                }
            }
        } else {
            return 0b100;
        }
    }

    pub inline fn encode(self: Self, allocator: std.mem.Allocator, modrm_byte: ?*u8, rex_byte: ?*u8) !std.ArrayList(u8) {
        var bytes_array = std.ArrayList(u8).init(allocator);
        errdefer bytes_array.deinit();

        const mod_byte = self.mod();
        const rm_byte = try self.rm();
        const byte = mod_byte << 6 | rm_byte;
        if (modrm_byte) |modrm_b| {
            modrm_b.* |= byte;
        } else {
            return error.MissingModRmByte;
        }

        var base_byte = if (self.base) |b| b.idx() else 0b101;
        const scale_byte = if (self.scale) |s| @intCast(u8, s) else 0b00;
        var index_byte = if (self.index) |i| i.idx() else 0b100;

        // Update Rex prefix in case of 64-bit only extended register usage
        if (base_byte > 7) {
            if (rex_byte) |rex_b| {
                rex_b.* |= @enumToInt(Prefix.RexB);
            } else {
                return error.MissingRexByte;
            }
            base_byte -= 8;
        }
        if (index_byte > 7) {
            if (rex_byte) |rex_b| {
                rex_b.* |= @enumToInt(Prefix.RexX);
            } else {
                return error.MissingRexByte;
            }
            index_byte -= 8;
        }
        if (rm_byte == 0b100) { // Sib byte
            try bytes_array.append(scale_byte << 6 | index_byte << 3 | base_byte);
        }

        // Encode displacement
        if (self.displacement) |disp| {
            try bytes_array.appendSlice(try disp.encode());
            // Check if 32-bit displacement mode actually has a 32 bit displacement
            if (self.base) |b| {
                if (b.isRip() and disp.is8Bit()) {
                    // encode remaining 3-bytes offset
                    try bytes_array.appendSlice(&[3]u8{ 0, 0, 0 });
                }
            }
        } else if (self.base) |b| {
            if (b.isRbp()) {
                // Explicit 1-byte displacement
                // 'cause [rbp] is not supported
                try bytes_array.append(0);
            } else if (b.isRip()) {
                // Explicit 32-bit displacement
                // 'cause [rip] is not supported and should always have a 32 bit displacement
                try bytes_array.appendSlice(&[4]u8{ 0, 0, 0, 0 });
            }
        }

        return bytes_array;
    }
};

pub const Memory = union(enum) {
    mem8: Sib,
    mem16: Sib,
    mem32: Sib,
    mem48: Sib,
    mem64: Sib,
    mem80: Sib,
    mem94m108: Sib,
    mem14m28: Sib,
    mem128: Sib,
    mem512: Sib,
    mem: Sib, // Un sized variant
    const Self = @This();

    pub inline fn isValid(self: Self) bool {
        switch (self) {
            .mem8 => |m| return m.isValid(),
            .mem16 => |m| return m.isValid(),
            .mem32 => |m| return m.isValid(),
            .mem48 => |m| return m.isValid(),
            .mem64 => |m| return m.isValid(),
            .mem80 => |m| return m.isValid(),
            .mem94m108 => |m| return m.isValid(),
            .mem14m28 => |m| return m.isValid(),
            .mem128 => |m| return m.isValid(),
            .mem512 => |m| return m.isValid(),
            .mem => |m| return m.isValid(),
        }
    }

    pub inline fn toMem(mem: Self, dest_size: u16) !Self {
        switch (dest_size) {
            8 => return Self{ .mem8 = mem.sib() },
            16 => return Self{ .mem16 = mem.sib() },
            32 => return Self{ .mem32 = mem.sib() },
            48 => return Self{ .mem48 = mem.sib() },
            64 => return Self{ .mem64 = mem.sib() },
            80 => return Self{ .mem80 = mem.sib() },
            108 => return Self{ .mem94m108 = mem.sib() },
            28 => return Self{ .mem14m28 = mem.sib() },
            128 => return Self{ .mem128 = mem.sib() },
            512 => return Self{ .mem512 = mem.sib() },
            else => return error.InvalidMemorySize,
        }
    }
    pub inline fn size(self: Self) u16 {
        switch (self) {
            .mem8 => return 8,
            .mem16 => return 16,
            .mem32 => return 32,
            .mem48 => return 48,
            .mem64 => return 64,
            .mem80 => return 80,
            .mem94m108 => return 108,
            .mem14m28 => return 28,
            .mem128 => return 128,
            .mem512 => return 512,
            .mem => return 0,
        }
    }

    pub inline fn isMem(self: Self) bool {
        switch (self) {
            .mem => return true,
            else => return false,
        }
    }

    pub inline fn matches(self: Self, other: Self) bool {
        // Is a template
        if (other.isMem()) return true;
        return self.size() == other.size();
    }

    pub inline fn sibSize(self: Self) !u16 {
        switch (self) {
            .mem8 => |m| return m.size(),
            .mem16 => |m| return m.size(),
            .mem32 => |m| return m.size(),
            .mem48 => |m| return m.size(),
            .mem64 => |m| return m.size(),
            .mem80 => |m| return m.size(),
            .mem94m108 => |m| return m.size(),
            .mem14m28 => |m| return m.size(),
            .mem128 => |m| return m.size(),
            .mem512 => |m| return m.size(),
            .mem => |m| return m.size(),
        }
    }

    inline fn sib(self: Self) Sib {
        switch (self) {
            .mem8 => |m| return m,
            .mem16 => |m| return m,
            .mem32 => |m| return m,
            .mem48 => |m| return m,
            .mem64 => |m| return m,
            .mem80 => |m| return m,
            .mem94m108 => |m| return m,
            .mem14m28 => |m| return m,
            .mem128 => |m| return m,
            .mem512 => |m| return m,
            .mem => |m| return m,
        }
    }

    pub inline fn index(self: Self) ?Register {
        switch (self) {
            .mem8 => |m| return m.index,
            .mem16 => |m| return m.index,
            .mem32 => |m| return m.index,
            .mem48 => |m| return m.index,
            .mem64 => |m| return m.index,
            .mem80 => |m| return m.index,
            .mem94m108 => |m| return m.index,
            .mem14m28 => |m| return m.index,
            .mem128 => |m| return m.index,
            .mem512 => |m| return m.index,
            .mem => |m| return m.index,
        }
    }

    pub inline fn base(self: Self) ?Register {
        switch (self) {
            .mem8 => |m| return m.base,
            .mem16 => |m| return m.base,
            .mem32 => |m| return m.base,
            .mem48 => |m| return m.base,
            .mem64 => |m| return m.base,
            .mem80 => |m| return m.base,
            .mem94m108 => |m| return m.base,
            .mem14m28 => |m| return m.base,
            .mem128 => |m| return m.base,
            .mem512 => |m| return m.base,
            .mem => |m| return m.base,
        }
    }

    pub inline fn encode(self: Self, allocator: std.mem.Allocator, modrm_byte: ?*u8, rex_byte: ?*u8) !std.ArrayList(u8) {
        switch (self) {
            .mem8 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem16 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem32 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem48 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem64 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem80 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem94m108 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem14m28 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem128 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem512 => |m| return try m.encode(allocator, modrm_byte, rex_byte),
            .mem => return error.MustBeResolvedToASizedVariant,
        }
    }
};
