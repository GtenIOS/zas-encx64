pub const reg = @import("reg.zig");
pub const instr = @import("instr.zig");
pub const mem = @import("mem.zig");
pub const opnd = @import("opnd.zig");
pub const pfx = @import("pfx.zig");
const OperatingMode = @import("common").OperatingMode;

test "Instructions" {
    const std = @import("std");
    const Operand = opnd.Operand;
    const RegGpIdx = reg.RegGpIdx;
    const Instruction = instr.Instruction;
    var op_mode = OperatingMode.Bits64;
    var mnem = "mov";
    var oper1 = Operand{ .reg = .{ .reg32Gp = RegGpIdx.RegBx } };
    var oper2 = Operand{ .reg = .reg32A };
    var opers: []Operand = &[_]Operand{ oper1, oper2 };
    var found = if (try instr.findInstr(mnem, opers, .Bits64)) |_| true else false;
    try std.testing.expect(found);

    mnem = "add";
    oper1 = Operand{ .reg = .reg32A };
    oper2 = Operand{ .imm = .{ .imm32 = 0x2345ef } };
    opers = &[_]Operand{ oper1, oper2 };
    found = if (try instr.findInstr(mnem, opers, .Bits64)) |_| true else false;
    try std.testing.expect(found);

    // Allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    const allocator = gpa.allocator();

    defer {
        _ = gpa.deinit();
    }

    mnem = "mov";
    oper1 = Operand{ .reg = .{ .reg64Gp = RegGpIdx.RegSi } };
    oper2 = Operand{ .mem = .{ .mem64 = .{ .scale = null, .base = null, .index = null, .displacement = .{ .disp32 = 0x00000020 } } } };
    opers = &[_]Operand{ oper1, oper2 };
    const instr_movrm: ?Instruction = try instr.findInstr(mnem, opers, .Bits64);
    try std.testing.expect(instr_movrm != null);

    if (instr_movrm) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0x48, 0x8b, 0x34, 0x25, 0x20, 0x0, 0x0, 0x0 }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction `mov_rm`", .{});
            return err;
        }
    }

    oper1 = Operand{ .reg = .{ .reg64Gp = RegGpIdx.RegCx } };
    oper2 = Operand{ .imm = .{ .imm64 = 0x20 } };
    opers = &[_]Operand{ oper1, oper2 };
    const instr_movri: ?Instruction = try instr.findInstr(mnem, opers, .Bits64);
    try std.testing.expect(instr_movrm != null);

    if (instr_movri) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0x48, 0xb9, 0x20, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction `mov_rm`", .{});
            return err;
        }
    }

    oper1 = Operand{ .reg = .{ .reg64Gp = RegGpIdx.RegAx } };
    oper2 = Operand{ .mem = .{ .mem64 = .{ .scale = null, .base = null, .index = null, .displacement = .{ .disp32 = 0x000000ff } } } };
    opers = &[_]Operand{ oper1, oper2 };
    const instr_movrmo: ?Instruction = try instr.findInstr(mnem, opers, .Bits64);
    try std.testing.expect(instr_movrm != null);

    if (instr_movrmo) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0x48, 0x8b, 0x04, 0x25, 0xff, 0x00, 0x00, 0x00 }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction `mov_rm`", .{});
            return err;
        }
    }

    mnem = "aad";
    oper1 = Operand{ .imm = .{ .imm8 = 0x23 } };
    opers = &[_]Operand{oper1};
    const instr_aad: ?Instruction = try instr.findInstr(mnem, opers, .Bits32);

    try std.testing.expect(instr_aad != null);
    if (instr_aad) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0xd5, 0x23 }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction", .{});
            return err;
        }
    }

    mnem = "adc";
    oper1 = Operand{ .reg = .{ .reg8Gp = RegGpIdx.RegBx } };
    oper2 = Operand{ .imm = .{ .imm8 = 0x23 } };
    opers = &[_]Operand{ oper1, oper2 };
    const instr_adc: ?Instruction = try instr.findInstr(mnem, opers, .Bits64);

    try std.testing.expect(instr_adc != null);
    if (instr_adc) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0x80, 0xd3, 0x23 }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction", .{});
            return err;
        }
    }

    oper1 = Operand{ .reg = .{ .reg8Gp = RegGpIdx.RegBx } };
    oper2 = Operand{ .reg = .{ .reg8Gp = RegGpIdx.RegCx } };
    opers = &[_]Operand{ oper1, oper2 };
    const instr_adc8: ?Instruction = try instr.findInstr(mnem, opers, .Bits64);

    try std.testing.expect(instr_adc8 != null);
    if (instr_adc8) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0x10, 0xcb }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction", .{});
            return err;
        }
    }

    oper1 = Operand{ .reg = .{ .reg64Gp = RegGpIdx.Reg8 } };
    oper2 = Operand{ .reg = .{ .reg64Gp = RegGpIdx.Reg15 } };
    opers = &[_]Operand{ oper1, oper2 };
    const instr_adc64: ?Instruction = try instr.findInstr(mnem, opers, .Bits64);

    try std.testing.expect(instr_adc64 != null);
    if (instr_adc64) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0x4d, 0x11, 0xf8 }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction", .{});
            return err;
        }
    }

    oper1 = Operand{ .reg = .{ .reg16Gp = RegGpIdx.RegBx } };
    oper2 = Operand{ .reg = .{ .reg16Gp = RegGpIdx.RegCx } };
    opers = &[_]Operand{ oper1, oper2 };
    const instr_adc16: ?Instruction = try instr.findInstr(mnem, opers, .Bits64);

    try std.testing.expect(instr_adc16 != null);
    if (instr_adc16) |ins| {
        std.log.info("Found instr {s}", .{ins.mnemonic()});
        if (ins.encode(allocator, opers, op_mode, null)) |bytes_array| {
            try std.testing.expect(std.mem.eql(u8, bytes_array.items, &[_]u8{ 0x66, 0x11, 0xcb }));
            bytes_array.deinit();
        } else |err| {
            std.log.info("Could not encode instruction", .{});
            return err;
        }
    }
}
