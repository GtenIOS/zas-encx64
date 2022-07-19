const std = @import("std");
const Operand = @import("./opnd.zig").Operand;
const RegGpIdx = @import("./reg.zig").RegGpIdx;
const Prefix = @import("./pfx.zig").Prefix;
const OperandEncoding = @import("./opnd.zig").OperandEncoding;
const OperatingMode = @import("common").OperatingMode;
const instrs = @import("isa.zig").instrs;
const EncodeResult = @import("./opnd.zig").EncodeResult;

pub fn Instr(comptime count: comptime_int) type {
    return struct {
        mnem: []const u8,
        opcode: []const u8,
        encoding: OperandEncoding,
        opers: [count]Operand,
        valid_op_modes: u3,
        valid_exp_prefixes: []const Prefix = &[_]Prefix{},
        operating_size: u16 = 32,
        prefix32: ?[]const Prefix, // Must be in the order they would end up encoded (LSB first)
        prefix64: ?[]const Prefix, // Must be in the order they would end up encoded (LSB first)
        const Self = @This();

        pub fn init(mnem: []const u8, opcode: []const u8, encoding: OperandEncoding, opers: [count]Operand, valid_op_modes: u3, valid_exp_prefixes: []const Prefix, operating_size: u16, prefix32: ?[]const Prefix, prefix64: ?[]const Prefix) Self {
            return Self{ .mnem = mnem, .opcode = opcode, .encoding = encoding, .opers = opers, .valid_op_modes = valid_op_modes, .valid_exp_prefixes = valid_exp_prefixes, .operating_size = operating_size, .prefix32 = prefix32, .prefix64 = prefix64 };
        }

        inline fn matches(self: Self, mnem: []const u8, opers: []const Operand, op_mode: OperatingMode) bool {
            return if (opers.len != self.opers.len or !std.mem.eql(u8, mnem, self.mnem) or !self.isOpModeValid(op_mode)) false else blk: {
                for (opers) |oper, i| {
                    if (!self.opers[i].matches(oper)) break :blk false;
                } else break :blk true;
            };
        }

        // inline fn encodeRMOperandPair(self: Self, allocator: std.mem.Allocator, oper1: Operand, oper2: Operand, rex_byte: ?*u8) !std.ArrayList(u8) {
        //     var mod: u8 = if (oper1.isReg() and oper2.isReg()) 0b11 << 6 else 0;
        //     if (mod != 0) {
        //         var reg1 = try oper1.regIdx();
        //         var reg2 = try oper2.regIdx();
        //         if (reg1 > 7) {
        //             if (rex_byte) |rex_b| {
        //                 rex_b.* |= if (self.encoding == .rm) @enumToInt(Prefix.RexR) else @enumToInt(Prefix.RexB);
        //             } else {
        //                 return error.MissingRexPrefix;
        //             }
        //             reg1 -= 8;
        //         }
        //         if (reg2 > 7) {
        //             if (rex_byte) |rex_b| {
        //                 rex_b.* |= if (self.encoding == .mr) @enumToInt(Prefix.RexR) else @enumToInt(Prefix.RexB);
        //             } else {
        //                 return error.MissingRexPrefix;
        //             }
        //             reg2 -= 8;
        //         }

        //         const reg_rm = if (self.encoding == .rm) reg1 << 3 | reg2 else reg2 << 3 | reg1;

        //         var bytes_array = std.ArrayList(u8).init(allocator);
        //         errdefer bytes_array.deinit();

        //         try bytes_array.append(mod | reg_rm);
        //         return bytes_array;
        //     } else {
        //         var reg = if (self.encoding == .rm) try oper1.regIdx() else try oper2.regIdx();
        //         const mem = if (self.encoding == .rm) try oper2.memory() else try oper1.memory();
        //         if (reg > 7) {
        //             if (rex_byte) |rex_b| {
        //                 rex_b.* |= @enumToInt(Prefix.RexR);
        //             } else {
        //                 return error.MissingRexPrefix;
        //             }
        //             reg -= 8;
        //         }
        //         const base = mem.base();
        //         const index = mem.index();
        //         var base_idx: ?u8 = null;
        //         var index_idx: ?u8 = null;
        //         if (base) |b| {
        //             base_idx = b.idx();
        //             if (base_idx.? > 7) {
        //                 if (rex_byte) |rex_b| {
        //                     rex_b.* |= @enumToInt(Prefix.RexB);
        //                 } else {
        //                     return error.MissingRexPrefix;
        //                 }
        //                 base_idx.? -= 8;
        //             }
        //         }

        //         if (index) |i| {
        //             index_idx = i.idx();
        //             if (index_idx.? > 7) {
        //                 if (rex_byte) |rex_b| {
        //                     rex_b.* |= @enumToInt(Prefix.RexX);
        //                 } else {
        //                     return error.MissingRexPrefix;
        //                 }
        //                 index_idx.? -= 8;
        //             }
        //         }

        //         var bytes_array = try mem.encode(allocator);

        //         const reg_byte = bytes_array.items[0] | reg << 3;
        //         bytes_array.items[0] = reg_byte;
        //         return bytes_array;
        //     }
        // }

        inline fn operatingModePrefixes(self: Self, op_mode: OperatingMode) ?[]const Prefix {
            switch (op_mode) {
                .Bits16 => return null,
                .Bits32 => return self.prefix32,
                .Bits64 => return self.prefix64,
            }
        }

        inline fn requiresRexW(self: Self, op_mode: OperatingMode) bool {
            return if (self.operatingModePrefixes(op_mode)) |prefix_slice| blk: {
                for (prefix_slice) |pfx| {
                    switch (pfx) {
                        .RexW => break :blk true,
                        else => break :blk false,
                    }
                } else break :blk false;
            } else false;
        }

        inline fn rexwPfxIdx(self: Self, imp_pfx_len: usize, op_mode: OperatingMode) !usize {
            if (self.operatingModePrefixes(op_mode)) |prefix| {
                for (prefix) |pfx, i| {
                    if (pfx == Prefix.RexW) return imp_pfx_len + i;
                }

                return error.MissingRexPrefix;
            } else {
                return error.MissingPrefixes;
            }
        }

        inline fn isOpModeValid(self: Self, op_mode: OperatingMode) bool {
            return (self.valid_op_modes & @enumToInt(op_mode)) != 0;
        }

        inline fn hasMemOper(opers: []const Operand) bool {
            return blk: {
                for (opers) |oper| {
                    if (oper.isMemory()) break :blk true;
                } else break :blk false;
            };
        }

        inline fn needsModRmByte(self: Self, opers: []const Operand) bool {
            if (hasMemOper(opers)) return true;
            switch (self.encoding) {
                .rm, .mr => return true,
                else => return false,
            }
        }

        inline fn encode(self: Self, allocator: std.mem.Allocator, opers: []const Operand, op_mode: OperatingMode, prefix: ?Prefix) !std.ArrayList(u8) {
            var bytes = std.ArrayList(u8).init(allocator);
            errdefer bytes.deinit();

            // Encode explicit prefix
            if (prefix) |exp_prefix| {
                for (self.valid_exp_prefixes) |val_exp_pfx| {
                    if (val_exp_pfx == exp_prefix) break;
                } else return error.PrefixNotAllowed;

                try bytes.append(@enumToInt(exp_prefix));
            }

            var rex_byte: ?*u8 = null;
            // Encode implicit prefixes
            if (self.operatingModePrefixes(op_mode)) |prefixes| {
                for (prefixes) |pfx| {
                    try bytes.append(@enumToInt(pfx));
                    if (rex_byte == null and (@enumToInt(pfx) | @enumToInt(Prefix.Rex) != 0)) rex_byte = &bytes.items[bytes.items.len - 1];
                }
            }

            // Encode primary (+ secondary/escape) opcode(s)
            try bytes.appendSlice(self.opcode);
            var po_byte: *u8 = &bytes.items[bytes.items.len - 1];

            var modrm: u8 = 0;
            var modrm_byte: ?*u8 = if (self.needsModRmByte(opers)) &modrm else null;
            var modrm_byte_idx = bytes.items.len;
            // Encode operands
            for (opers) |oper, i| {
                switch (try oper.encode(allocator, self.encoding, @intCast(u8, i), po_byte, modrm_byte, rex_byte)) {
                    .slice => |bys| try bytes.appendSlice(bys),
                    .arr => |arr| {
                        defer arr.deinit();
                        try bytes.appendSlice(arr.items);
                    },
                }
            }

            // Add ModR/M byte if required
            if (modrm_byte) |modrm_b| try bytes.insert(modrm_byte_idx, modrm_b.*);

            return bytes;
        }

        // inline fn encode(self: Self, allocator: std.mem.Allocator, opers: []const Operand, op_mode: OperatingMode, prefix: ?Prefix) !std.ArrayList(u8) {
        //     var bytes = std.ArrayList(u8).init(allocator);
        //     errdefer bytes.deinit();

        //     // Encode explicit prefix
        //     if (prefix) |imp_prefix| {
        //         try bytes.append(@enumToInt(imp_prefix));
        //     }

        //     // Encode implicit prefixes
        //     if (self.operatingModePrefixes(op_mode)) |prefixes| {
        //         for (prefixes) |pfx| {
        //             try bytes.append(@enumToInt(pfx));
        //         }
        //     }

        //     // Encode opcode(s)
        //     if (self.encoding == .ri) {
        //         try bytes.append(self.opcode[0] + try opers[0].regIdx());
        //         try bytes.appendSlice(try opers[1].encode(self.encoding));
        //     } else {
        //         try bytes.appendSlice(self.opcode);

        //         // Encode operands
        //         if (self.encoding == .d or self.encoding == .i or self.encoding == .rmo or self.encoding == .mor) {
        //             for (opers) |oper| {
        //                 try bytes.appendSlice(try oper.encode(self.encoding));
        //             }
        //         } else {
        //             if (opers.len == 2) {
        //                 var imp_pfx_len: usize = if (prefix == null) 0 else 1;

        //                 // Needs address size override prefix? Only in case of memory operand
        //                 if (opers[0].isMemory() or opers[1].isMemory()) {
        //                     const mem = if (self.encoding == .rm) try opers[1].memory() else try opers[0].memory();
        //                     const mem_sib_size = try mem.sibSize();
        //                     if (mem_sib_size == 32) {
        //                         // Add address size override prefix
        //                         try bytes.insert(imp_pfx_len, @enumToInt(Prefix.AddOvrd));
        //                         imp_pfx_len += 1;
        //                     }
        //                 }

        //                 const rm_oper_bytes = try self.encodeRMOperandPair(allocator, opers[0], opers[1], if (self.requiresRexW(op_mode)) &bytes.items[try self.rexwPfxIdx(imp_pfx_len, op_mode)] else null);
        //                 try bytes.appendSlice(rm_oper_bytes.items);
        //                 defer rm_oper_bytes.deinit();
        //             } else if (self.encoding != .po) {
        //                 std.log.info("Trying to parse {d} operands", .{opers.len});
        //                 unreachable;
        //             }
        //         }
        //     }
        //     return bytes;
        // }
    };
}

pub const Instruction = union(enum) {
    instr0: Instr(0),
    instr1: Instr(1),
    instr2: Instr(2),
    instr3: Instr(3),
    instr4: Instr(4),
    const Self = @This();

    pub inline fn mnemonic(self: Self) []const u8 {
        switch (self) {
            .instr0 => |instr| return instr.mnem,
            .instr1 => |instr| return instr.mnem,
            .instr2 => |instr| return instr.mnem,
            .instr3 => |instr| return instr.mnem,
            .instr4 => |instr| return instr.mnem,
        }
    }

    inline fn matches(self: Self, mnem: []const u8, opers: []const Operand, op_mode: OperatingMode) bool {
        switch (self) {
            .instr0 => |instr| return opers.len == 0 and std.mem.eql(u8, mnem, instr.mnem) and instr.isOpModeValid(op_mode),
            .instr1 => |instr| return instr.matches(mnem, opers, op_mode),
            .instr2 => |instr| return instr.matches(mnem, opers, op_mode),
            .instr3 => |instr| return instr.matches(mnem, opers, op_mode),
            .instr4 => |instr| return instr.matches(mnem, opers, op_mode),
        }
    }

    pub inline fn encoding(self: Self) OperandEncoding {
        switch (self) {
            .instr0 => |instr| return instr.encoding,
            .instr1 => |instr| return instr.encoding,
            .instr2 => |instr| return instr.encoding,
            .instr3 => |instr| return instr.encoding,
            .instr4 => |instr| return instr.encoding,
        }
    }

    pub inline fn operands(self: Self) []const Operand {
        switch (self) {
            .instr0 => |instr| return &instr.opers,
            .instr1 => |instr| return &instr.opers,
            .instr2 => |instr| return &instr.opers,
            .instr3 => |instr| return &instr.opers,
            .instr4 => |instr| return &instr.opers,
        }
    }

    pub inline fn encode(self: Self, allocator: std.mem.Allocator, opers: []const Operand, op_mode: OperatingMode, prefix: ?Prefix) !std.ArrayList(u8) {
        switch (self) {
            .instr0 => |instr| return instr.encode(allocator, opers, op_mode, prefix),
            .instr1 => |instr| return instr.encode(allocator, opers, op_mode, prefix),
            .instr2 => |instr| return instr.encode(allocator, opers, op_mode, prefix),
            .instr3 => |instr| return instr.encode(allocator, opers, op_mode, prefix),
            .instr4 => |instr| return instr.encode(allocator, opers, op_mode, prefix),
        }
    }

    inline fn operatingSize(self: Self) u16 {
        switch (self) {
            .instr0 => |instr| return instr.operating_size,
            .instr1 => |instr| return instr.operating_size,
            .instr2 => |instr| return instr.operating_size,
            .instr3 => |instr| return instr.operating_size,
            .instr4 => |instr| return instr.operating_size,
        }
    }

    inline fn opersCount(self: Self) u8 {
        switch (self) {
            .instr0 => return 0,
            .instr1 => return 1,
            .instr2 => return 2,
            .instr3 => return 3,
            .instr4 => return 4,
        }
    }

    pub inline fn opcode(self: Self) []const u8 {
        switch (self) {
            .instr0 => |instr| return instr.opcode,
            .instr1 => |instr| return instr.opcode,
            .instr2 => |instr| return instr.opcode,
            .instr3 => |instr| return instr.opcode,
            .instr4 => |instr| return instr.opcode,
        }
    }
};

// const x64_instrs = [_]Instruction{
//     Instruction{ .instr0 = Instr(0).init("aaa", &[_]u8{0x37}, .po, [0]Operand{}, @enumToInt(OperatingMode.Bits32), null, null) },
//     Instruction{ .instr2 = Instr(2).init("mov", &[_]u8{0xa1}, .rmo, [2]Operand{
//         .{ .reg = .reg64A },
//         .{ .mofst = .{ .mofst64 = undefined } },
//     }, @enumToInt(OperatingMode.Bits64), null, &[_]Prefix{Prefix.RexW}) },
//     Instruction{ .instr1 = Instr(1).init("aad", &[_]u8{0xd5}, .i, [1]Operand{
//         .{ .imm = .{ .imm8 = undefined } },
//     }, @enumToInt(OperatingMode.Bits64), null, null) },
//     Instruction{ .instr2 = Instr(2).init("mov", &[_]u8{0x8b}, .rm, [2]Operand{
//         .{ .reg = .{ .reg64Gp = RegGpIdx.RegSi } },
//         .{ .rm = .{ .mem = .{ .mem64 = undefined } } },
//     }, @enumToInt(OperatingMode.Bits64), null, &[_]Prefix{Prefix.RexW}) },
//     Instruction{ .instr2 = Instr(2).init("mov", &[_]u8{0x88}, .rm, [2]Operand{
//         .{ .reg = .reg32A },
//         .{ .reg = .{ .reg32Gp = RegGpIdx.RegBx } },
//     }, @enumToInt(OperatingMode.Bits32) | @enumToInt(OperatingMode.Bits64), null, null) },
//     Instruction{ .instr2 = Instr(2).init("mov", &[_]u8{0x88}, .rm, [2]Operand{
//         .{ .reg = .{ .reg32Gp = RegGpIdx.RegBx } },
//         .{ .reg = .reg32A },
//     }, @enumToInt(OperatingMode.Bits32) | @enumToInt(OperatingMode.Bits64), null, null) },
//     Instruction{ .instr2 = Instr(2).init("add", &[_]u8{0x83}, .{ .d = 2 }, [2]Operand{
//         .{ .reg = .reg32A },
//         .{ .imm = .{ .imm32 = undefined } },
//     }, @enumToInt(OperatingMode.Bits32) | @enumToInt(OperatingMode.Bits64), null, null) },
//     Instruction{ .instr2 = Instr(2).init("mov", &[_]u8{0xb8}, .ri, [2]Operand{
//         .{ .reg = .{ .reg64Gp = undefined } },
//         .{ .imm = .{ .imm64 = undefined } },
//     }, @enumToInt(OperatingMode.Bits64), null, &[_]Prefix{Prefix.RexW}) },
//     Instruction{ .instr2 = Instr(2).init("adc", &[_]u8{0x80}, .{ .d = 2 }, [2]Operand{
//         .{ .rm = .{ .mem = undefined } },
//         .{ .imm = .{ .imm8 = undefined } },
//     }, @enumToInt(OperatingMode.Bits16) | @enumToInt(OperatingMode.Bits32) | @enumToInt(OperatingMode.Bits64), null, null) },
//     Instruction{ .instr2 = Instr(2).init("adc", &[_]u8{0x10}, .mr, [2]Operand{
//         .{ .rm = .{ .mem = undefined } },
//         .{ .reg = .{ .reg8Gp = undefined } },
//     }, @enumToInt(OperatingMode.Bits16) | @enumToInt(OperatingMode.Bits32) | @enumToInt(OperatingMode.Bits64), null, null) },
//     Instruction{ .instr2 = Instr(2).init("adc", &[_]u8{0x11}, .mr, [2]Operand{
//         .{ .rm = .{ .mem = .{ .mem64 = undefined } } },
//         .{ .reg = .{ .reg64Gp = undefined } },
//     }, @enumToInt(OperatingMode.Bits64), null, &[_]Prefix{Prefix.RexW}) },
//     Instruction{ .instr2 = Instr(2).init("adc", &[_]u8{0x11}, .mr, [2]Operand{
//         .{ .rm = .{ .mem = .{ .mem16 = undefined } } },
//         .{ .reg = .{ .reg16Gp = undefined } },
//     }, @enumToInt(OperatingMode.Bits64), null, &[_]Prefix{Prefix.OpOvrd}) },
//     Instruction{ .instr2 = Instr(2).init("adc", &[_]u8{0x11}, .mr, [2]Operand{
//         .{ .rm = .{ .mem = .{ .mem32 = undefined } } },
//         .{ .reg = .{ .reg32Gp = undefined } },
//     }, @enumToInt(OperatingMode.Bits32) | @enumToInt(OperatingMode.Bits64), null, null) },
//     Instruction{ .instr2 = Instr(2).init("adc", &[_]u8{0x11}, .mr, [2]Operand{
//         .{ .rm = .{ .mem = .{ .mem16 = undefined } } },
//         .{ .reg = .{ .reg16Gp = undefined } },
//     }, @enumToInt(OperatingMode.Bits32) | @enumToInt(OperatingMode.Bits64), null, &[_]Prefix{Prefix.OpOvrd}) },
// };

pub fn findInstr(mnem: []const u8, opers: []const Operand, op_mode: OperatingMode) !?Instruction {
    // Validate request
    if (mnem[0] > 'z' or mnem[0] < 'a') return null;
    if (opers.len > 4) return null;

    // Find instruction
    return blk: {
        var found_instr: ?Instruction = null;
        for (instrs[mnem[0] - 'a'][opers.len]) |instr| {
            if (instr.matches(mnem, opers, op_mode)) {
                if (found_instr) |prev_instr| {
                    if (prev_instr.operatingSize() == instr.operatingSize() or prev_instr.encoding().isCovariant(instr.encoding())) continue;
                    for (opers) |oper| if (oper.size() == 0) return error.NeedsExplicitSizeDerivative;
                    return error.FoundMultipleVariantOfTheInstruction;
                }
                found_instr = instr;
            }
        }
        break :blk found_instr;
    };
}
