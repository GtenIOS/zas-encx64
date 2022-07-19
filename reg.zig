const std = @import("std");
const OperandEncoding = @import("opnd.zig").OperandEncoding;
const Prefix = @import("pfx.zig").Prefix;

pub const RegGpIdx = enum(u4) {
    RegAx,
    RegCx,
    RegDx,
    RegBx,
    RegSp,
    RegBp,
    RegSi,
    RegDi,
    Reg8,
    Reg9,
    Reg10,
    Reg11,
    Reg12,
    Reg13,
    Reg14,
    Reg15,
};

pub const RegMMXIdx = enum(u3) {
    RegMMX0,
    RegMMX1,
    RegMMX2,
    RegMMX3,
    RegMMX4,
    RegMMX5,
    RegMMX6,
    RegMMX7,
};

pub const RegXMMIdx = enum(u4) {
    RegXMM0,
    RegXMM1,
    RegXMM2,
    RegXMM3,
    RegXMM4,
    RegXMM5,
    RegXMM6,
    RegXMM7,
    RegXMM8,
    RegXMM9,
    RegXMM10,
    RegXMM11,
    RegXMM12,
    RegXMM13,
    RegXMM14,
    RegXMM15,
};

pub const RegYMMIdx = enum(u4) {
    RegYMM0,
    RegYMM1,
    RegYMM2,
    RegYMM3,
    RegYMM4,
    RegYMM5,
    RegYMM6,
    RegYMM7,
    RegYMM8,
    RegYMM9,
    RegYMM10,
    RegYMM11,
    RegYMM12,
    RegYMM13,
    RegYMM14,
    RegYMM15,
};

pub const RegDRIdx = enum(u4) {
    RegDR0,
    RegDR1,
    RegDR2,
    RegDR3,
    RegDR4,
    RegDR5,
    RegDR6,
    RegDR7,
};

pub const RegCRIdx = enum(u4) {
    RegCR0,
    RegCR1,
    RegCR2,
    RegCR3,
    RegCR4,
    RegCR5,
    RegCR6,
    RegCR7,
};

pub const RegSTIdx = enum(u4) {
    RegST0,
    RegST1,
    RegST2,
    RegST3,
    RegST4,
    RegST5,
    RegST6,
    RegST7,
};

pub const RegSize = enum(u4) {
    R8,
    R16,
    R32,
    R64,
    R128,
    R256,
    R512,
};

pub const Register = union(enum) {
    reg8A,
    reg16A,
    reg32A,
    reg64A,
    reg8Gp: RegGpIdx,
    reg16Gp: RegGpIdx,
    reg32Gp: RegGpIdx,
    reg64Gp: RegGpIdx,
    regDR: RegDRIdx,
    regCR: RegCRIdx,
    regST: RegSTIdx,
    regMMX: RegMMXIdx,
    regXMM: RegXMMIdx,
    regYMM: RegYMMIdx,
    regES,
    regCS,
    regSS,
    regDS,
    regFS,
    regGS,
    rip: u8,
    const Self = @This();

    inline fn isReg8A(self: Self) bool {
        switch (self) {
            .reg8A => return true,
            .reg8Gp => |reg_idx| return reg_idx == .RegAx,
            else => return false,
        }
    }

    inline fn isReg16A(self: Self) bool {
        switch (self) {
            .reg16A => return true,
            .reg16Gp => |reg_idx| return reg_idx == .RegAx,
            else => return false,
        }
    }

    inline fn isReg32A(self: Self) bool {
        switch (self) {
            .reg32A => return true,
            .reg32Gp => |reg_idx| return reg_idx == .RegAx,
            else => return false,
        }
    }

    inline fn isReg64A(self: Self) bool {
        switch (self) {
            .reg64A => return true,
            .reg64Gp => |reg_idx| return reg_idx == .RegAx,
            else => return false,
        }
    }

    inline fn isReg8Gp(self: Self) bool {
        switch (self) {
            .reg8Gp => return true,
            .reg8A => return true,
            else => return false,
        }
    }

    inline fn isReg16Gp(self: Self) bool {
        switch (self) {
            .reg16Gp => return true,
            .reg16A => return true,
            else => return false,
        }
    }

    inline fn isReg32Gp(self: Self) bool {
        switch (self) {
            .reg32Gp => return true,
            .reg32A => return true,
            else => return false,
        }
    }

    inline fn isReg64Gp(self: Self) bool {
        switch (self) {
            .reg64Gp => return true,
            .reg64A => return true,
            else => return false,
        }
    }

    inline fn isRegDR(self: Self) bool {
        switch (self) {
            .regDR => return true,
            else => return false,
        }
    }

    inline fn isRegCR(self: Self) bool {
        switch (self) {
            .regCR => return true,
            else => return false,
        }
    }

    inline fn isRegST(self: Self) bool {
        switch (self) {
            .regST => return true,
            else => return false,
        }
    }

    inline fn isRegMMX(self: Self) bool {
        switch (self) {
            .regMMX => return true,
            else => return false,
        }
    }

    inline fn isRegXMM(self: Self) bool {
        switch (self) {
            .regXMM => return true,
            else => return false,
        }
    }

    inline fn isRegYMM(self: Self) bool {
        switch (self) {
            .regYMM => return true,
            else => return false,
        }
    }

    pub inline fn matches(self: Self, other: Self) bool {
        switch (self) {
            .reg8A => return other.isReg8A(),
            .reg16A => return other.isReg16A(),
            .reg32A => return other.isReg32A(),
            .reg64A => return other.isReg64A(),
            .reg8Gp => return other.isReg8Gp(),
            .reg16Gp => return other.isReg16Gp(),
            .reg32Gp => return other.isReg32Gp(),
            .reg64Gp => return other.isReg64Gp(),
            .regDR => return other.isRegDR(),
            .regCR => return other.isRegCR(),
            .regST => return other.isRegST(),
            .regMMX => return other.isRegMMX(),
            .regXMM => return other.isRegXMM(),
            .regYMM => return other.isRegYMM(),
            .regES => return other == .regES,
            .regCS => return other == .regCS,
            .regSS => return other == .regSS,
            .regDS => return other == .regDS,
            .regFS => return other == .regFS,
            .regGS => return other == .regGS,
            else => return false,
        }
    }

    pub inline fn ripSize(self: Self) ?u8 {
        switch (self) {
            .rip => |size| return size,
            else => return null,
        }
    }

    pub inline fn isRbp(self: Self) bool {
        switch (self) {
            .reg8Gp => |reg| return reg == RegGpIdx.RegBp,
            .reg16Gp => |reg| return reg == RegGpIdx.RegBp,
            .reg32Gp => |reg| return reg == RegGpIdx.RegBp,
            .reg64Gp => |reg| return reg == RegGpIdx.RegBp,
            else => return false,
        }
    }

    pub inline fn isRip(self: Self) bool {
        switch (self) {
            .rip => return true,
            else => return false,
        }
    }

    pub inline fn isSp(self: Self) bool {
        switch (self) {
            .reg8Gp => |reg| return reg == RegGpIdx.RegSp,
            .reg16Gp => |reg| return reg == RegGpIdx.RegSp,
            .reg32Gp => |reg| return reg == RegGpIdx.RegSp,
            .reg64Gp => |reg| return reg == RegGpIdx.RegSp,
            else => return false,
        }
    }

    pub inline fn size(self: Self) u16 {
        if (self.isReg8A() or self.isReg8Gp()) {
            return 8;
        } else if (self.isReg16A() or self.isReg16Gp()) {
            return 16;
        } else if (self.isReg32A() or self.isReg32Gp()) {
            return 32;
        } else if (self.ripSize()) |rsize| {
            return @intCast(u16, rsize);
        } else {
            return 64;
        }
    }

    pub inline fn idx(self: Self) u8 {
        switch (self) {
            .reg8Gp => |id| return @enumToInt(id),
            .reg16Gp => |id| return @enumToInt(id),
            .reg32Gp => |id| return @enumToInt(id),
            .reg64Gp => |id| return @enumToInt(id),
            .regDR => |id| return @enumToInt(id),
            .regCR => |id| return @enumToInt(id),
            .regST => |id| return @enumToInt(id),
            else => return 0,
        }
    }

    pub inline fn encode(self: Self, encoding: OperandEncoding, oper_idx: u8, po_byte: *u8, modrm_byte: ?*u8, rex_byte: ?*u8) ![]const u8 {
        switch (encoding) {
            .d => |val| return &[_]u8{0b11000000 | (@intCast(u8, val) << 3) | self.idx()},
            .ri => {
                po_byte.* += self.idx();
                return &[_]u8{};
            },
            .rm => {
                if (modrm_byte) |modrm_b| {
                    var r_idx = self.idx();
                    if (r_idx > 7) {
                        r_idx -= 8;
                        if (rex_byte) |rex_b| {
                            rex_b.* |= if (oper_idx == 0) @enumToInt(Prefix.RexR) else @enumToInt(Prefix.RexB);
                        } else {
                            return error.MissingRexByte;   
                        }
                    }
                    
                    if (oper_idx == 1) {
                        // Found register at rm operand, hence
                        // Set mode to Register Direct addressing mode
                        modrm_b.* |= 0b11 << 6; 
                    }

                    modrm_b.* |= if (oper_idx == 0) r_idx << 3 else r_idx;
                    return &[_]u8{};
                } else {
                    return error.MissingModRmByte;
                }
            },
            .mr => {
                if (modrm_byte) |modrm_b| {
                    var r_idx = self.idx();
                    if (r_idx > 7) {
                        r_idx -= 8;
                        if (rex_byte) |rex_b| {
                            rex_b.* |= if (oper_idx == 0) @enumToInt(Prefix.RexB) else @enumToInt(Prefix.RexR);
                        } else {
                            return error.MissingRexByte;   
                        }
                    }

                    if (oper_idx == 0) {
                        // Found register at rm operand, hence
                        // Set mode to Register Direct addressing mode
                        modrm_b.* |= 0b11 << 6; 
                    }

                    modrm_b.* |= if (oper_idx == 0) r_idx else r_idx << 3;
                    return &[_]u8{};
                } else {
                    return error.MissingModRmByte;
                }
            },
            .rmo, .mor, .po => return &[_]u8{},
            else => {
                std.log.err("{s}:{d}", .{encoding.typeStr(), oper_idx});
                return error.InvalidRegisterEncoding;
            },
        }
    }

    const RegId = struct {
        name: []const u8,
        reg: Register,
    };

    inline fn matchesReg8Gp(name: []const u8) ?Register {
        const R8 = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "al", .reg = .{ .reg8Gp = RegGpIdx.RegAx } },
                .{ .name = "bl", .reg = .{ .reg8Gp = RegGpIdx.RegBx } },
                .{ .name = "cl", .reg = .{ .reg8Gp = RegGpIdx.RegCx } },
                .{ .name = "dl", .reg = .{ .reg8Gp = RegGpIdx.RegDx } },
                .{ .name = "spl", .reg = .{ .reg8Gp = RegGpIdx.RegSp } },
                .{ .name = "bpl", .reg = .{ .reg8Gp = RegGpIdx.RegBp } },
                .{ .name = "sil", .reg = .{ .reg8Gp = RegGpIdx.RegSi } },
                .{ .name = "dil", .reg = .{ .reg8Gp = RegGpIdx.RegDi } },
                .{ .name = "r8l", .reg = .{ .reg8Gp = RegGpIdx.Reg8 } },
                .{ .name = "r9l", .reg = .{ .reg8Gp = RegGpIdx.Reg9 } },
                .{ .name = "r10l", .reg = .{ .reg8Gp = RegGpIdx.Reg10 } },
                .{ .name = "r11l", .reg = .{ .reg8Gp = RegGpIdx.Reg11 } },
                .{ .name = "r12l", .reg = .{ .reg8Gp = RegGpIdx.Reg12 } },
                .{ .name = "r13l", .reg = .{ .reg8Gp = RegGpIdx.Reg13 } },
                .{ .name = "r14l", .reg = .{ .reg8Gp = RegGpIdx.Reg14 } },
                .{ .name = "r15l", .reg = .{ .reg8Gp = RegGpIdx.Reg15 } },
                .{ .name = "ah", .reg = .{ .reg8Gp = RegGpIdx.RegAx } },
                .{ .name = "bh", .reg = .{ .reg8Gp = RegGpIdx.RegBx } },
                .{ .name = "ch", .reg = .{ .reg8Gp = RegGpIdx.RegCx } },
                .{ .name = "dh", .reg = .{ .reg8Gp = RegGpIdx.RegDx } },
            };
        };

        for (R8.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesReg16Gp(name: []const u8) ?Register {
        // TODO: Add segment registers
        const R16 = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "ax", .reg = .{ .reg16Gp = RegGpIdx.RegAx } },
                .{ .name = "bx", .reg = .{ .reg16Gp = RegGpIdx.RegBx } },
                .{ .name = "cx", .reg = .{ .reg16Gp = RegGpIdx.RegCx } },
                .{ .name = "dx", .reg = .{ .reg16Gp = RegGpIdx.RegDx } },
                .{ .name = "sp", .reg = .{ .reg16Gp = RegGpIdx.RegSp } },
                .{ .name = "bp", .reg = .{ .reg16Gp = RegGpIdx.RegBp } },
                .{ .name = "si", .reg = .{ .reg16Gp = RegGpIdx.RegSi } },
                .{ .name = "di", .reg = .{ .reg16Gp = RegGpIdx.RegDi } },
                .{ .name = "r8w", .reg = .{ .reg16Gp = RegGpIdx.Reg8 } },
                .{ .name = "r9w", .reg = .{ .reg16Gp = RegGpIdx.Reg9 } },
                .{ .name = "r10w", .reg = .{ .reg16Gp = RegGpIdx.Reg10 } },
                .{ .name = "r11w", .reg = .{ .reg16Gp = RegGpIdx.Reg11 } },
                .{ .name = "r12w", .reg = .{ .reg16Gp = RegGpIdx.Reg12 } },
                .{ .name = "r13w", .reg = .{ .reg16Gp = RegGpIdx.Reg13 } },
                .{ .name = "r14w", .reg = .{ .reg16Gp = RegGpIdx.Reg14 } },
                .{ .name = "r15w", .reg = .{ .reg16Gp = RegGpIdx.Reg15 } },
            };
        };

        for (R16.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesReg32Gp(name: []const u8) ?Register {
        // TODO: Add control and debug registers
        const R32 = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "eax", .reg = .{ .reg32Gp = RegGpIdx.RegAx } },
                .{ .name = "ebx", .reg = .{ .reg32Gp = RegGpIdx.RegBx } },
                .{ .name = "ecx", .reg = .{ .reg32Gp = RegGpIdx.RegCx } },
                .{ .name = "edx", .reg = .{ .reg32Gp = RegGpIdx.RegDx } },
                .{ .name = "esp", .reg = .{ .reg32Gp = RegGpIdx.RegSp } },
                .{ .name = "ebp", .reg = .{ .reg32Gp = RegGpIdx.RegBp } },
                .{ .name = "esi", .reg = .{ .reg32Gp = RegGpIdx.RegSi } },
                .{ .name = "edi", .reg = .{ .reg32Gp = RegGpIdx.RegDi } },
                .{ .name = "r8d", .reg = .{ .reg32Gp = RegGpIdx.Reg8 } },
                .{ .name = "r9d", .reg = .{ .reg32Gp = RegGpIdx.Reg9 } },
                .{ .name = "r10d", .reg = .{ .reg32Gp = RegGpIdx.Reg10 } },
                .{ .name = "r11d", .reg = .{ .reg32Gp = RegGpIdx.Reg11 } },
                .{ .name = "r12d", .reg = .{ .reg32Gp = RegGpIdx.Reg12 } },
                .{ .name = "r13d", .reg = .{ .reg32Gp = RegGpIdx.Reg13 } },
                .{ .name = "r14d", .reg = .{ .reg32Gp = RegGpIdx.Reg14 } },
                .{ .name = "r15d", .reg = .{ .reg32Gp = RegGpIdx.Reg15 } },
                .{ .name = "eip", .reg = .{ .rip = 32 } },
            };
        };

        for (R32.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesReg64Gp(name: []const u8) ?Register {
        const R64 = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "rax", .reg = .{ .reg64Gp = RegGpIdx.RegAx } },
                .{ .name = "rbx", .reg = .{ .reg64Gp = RegGpIdx.RegBx } },
                .{ .name = "rcx", .reg = .{ .reg64Gp = RegGpIdx.RegCx } },
                .{ .name = "rdx", .reg = .{ .reg64Gp = RegGpIdx.RegDx } },
                .{ .name = "rsp", .reg = .{ .reg64Gp = RegGpIdx.RegSp } },
                .{ .name = "rbp", .reg = .{ .reg64Gp = RegGpIdx.RegBp } },
                .{ .name = "rsi", .reg = .{ .reg64Gp = RegGpIdx.RegSi } },
                .{ .name = "rdi", .reg = .{ .reg64Gp = RegGpIdx.RegDi } },
                .{ .name = "r8", .reg = .{ .reg64Gp = RegGpIdx.Reg8 } },
                .{ .name = "r9", .reg = .{ .reg64Gp = RegGpIdx.Reg9 } },
                .{ .name = "r10", .reg = .{ .reg64Gp = RegGpIdx.Reg10 } },
                .{ .name = "r11", .reg = .{ .reg64Gp = RegGpIdx.Reg11 } },
                .{ .name = "r12", .reg = .{ .reg64Gp = RegGpIdx.Reg12 } },
                .{ .name = "r13", .reg = .{ .reg64Gp = RegGpIdx.Reg13 } },
                .{ .name = "r14", .reg = .{ .reg64Gp = RegGpIdx.Reg14 } },
                .{ .name = "r15", .reg = .{ .reg64Gp = RegGpIdx.Reg15 } },
                .{ .name = "rip", .reg = .{ .rip = 64 } },
            };
        };

        for (R64.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesRegMMX(name: []const u8) ?Register {
        const RMMX = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "mmx0", .reg = .{ .regMMX = RegMMXIdx.RegMMX0 } },
                .{ .name = "mmx1", .reg = .{ .regMMX = RegMMXIdx.RegMMX1 } },
                .{ .name = "mmx2", .reg = .{ .regMMX = RegMMXIdx.RegMMX2 } },
                .{ .name = "mmx3", .reg = .{ .regMMX = RegMMXIdx.RegMMX3 } },
                .{ .name = "mmx4", .reg = .{ .regMMX = RegMMXIdx.RegMMX4 } },
                .{ .name = "mmx5", .reg = .{ .regMMX = RegMMXIdx.RegMMX5 } },
                .{ .name = "mmx6", .reg = .{ .regMMX = RegMMXIdx.RegMMX6 } },
                .{ .name = "mmx7", .reg = .{ .regMMX = RegMMXIdx.RegMMX7 } },
            };
        };

        for (RMMX.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesRegXMM(name: []const u8) ?Register {
        const RXMM = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "xmm0", .reg = .{ .regXMM = RegXMMIdx.RegXMM0 } },
                .{ .name = "xmm1", .reg = .{ .regXMM = RegXMMIdx.RegXMM1 } },
                .{ .name = "xmm2", .reg = .{ .regXMM = RegXMMIdx.RegXMM2 } },
                .{ .name = "xmm3", .reg = .{ .regXMM = RegXMMIdx.RegXMM3 } },
                .{ .name = "xmm4", .reg = .{ .regXMM = RegXMMIdx.RegXMM4 } },
                .{ .name = "xmm5", .reg = .{ .regXMM = RegXMMIdx.RegXMM5 } },
                .{ .name = "xmm6", .reg = .{ .regXMM = RegXMMIdx.RegXMM6 } },
                .{ .name = "xmm7", .reg = .{ .regXMM = RegXMMIdx.RegXMM7 } },
                .{ .name = "xmm8", .reg = .{ .regXMM = RegXMMIdx.RegXMM8 } },
                .{ .name = "xmm9", .reg = .{ .regXMM = RegXMMIdx.RegXMM9 } },
                .{ .name = "xmm10", .reg = .{ .regXMM = RegXMMIdx.RegXMM10 } },
                .{ .name = "xmm11", .reg = .{ .regXMM = RegXMMIdx.RegXMM11 } },
                .{ .name = "xmm12", .reg = .{ .regXMM = RegXMMIdx.RegXMM12 } },
                .{ .name = "xmm13", .reg = .{ .regXMM = RegXMMIdx.RegXMM13 } },
                .{ .name = "xmm14", .reg = .{ .regXMM = RegXMMIdx.RegXMM14 } },
                .{ .name = "xmm15", .reg = .{ .regXMM = RegXMMIdx.RegXMM15 } },
            };
        };

        for (RXMM.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesRegYMM(name: []const u8) ?Register {
        const RYMM = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "ymm0", .reg = .{ .regYMM = RegYMMIdx.RegYMM0 } },
                .{ .name = "ymm1", .reg = .{ .regYMM = RegYMMIdx.RegYMM1 } },
                .{ .name = "ymm2", .reg = .{ .regYMM = RegYMMIdx.RegYMM2 } },
                .{ .name = "ymm3", .reg = .{ .regYMM = RegYMMIdx.RegYMM3 } },
                .{ .name = "ymm4", .reg = .{ .regYMM = RegYMMIdx.RegYMM4 } },
                .{ .name = "ymm5", .reg = .{ .regYMM = RegYMMIdx.RegYMM5 } },
                .{ .name = "ymm6", .reg = .{ .regYMM = RegYMMIdx.RegYMM6 } },
                .{ .name = "ymm7", .reg = .{ .regYMM = RegYMMIdx.RegYMM7 } },
                .{ .name = "ymm8", .reg = .{ .regYMM = RegYMMIdx.RegYMM8 } },
                .{ .name = "ymm9", .reg = .{ .regYMM = RegYMMIdx.RegYMM9 } },
                .{ .name = "ymm10", .reg = .{ .regYMM = RegYMMIdx.RegYMM10 } },
                .{ .name = "ymm11", .reg = .{ .regYMM = RegYMMIdx.RegYMM11 } },
                .{ .name = "ymm12", .reg = .{ .regYMM = RegYMMIdx.RegYMM12 } },
                .{ .name = "ymm13", .reg = .{ .regYMM = RegYMMIdx.RegYMM13 } },
                .{ .name = "ymm14", .reg = .{ .regYMM = RegYMMIdx.RegYMM14 } },
                .{ .name = "ymm15", .reg = .{ .regYMM = RegYMMIdx.RegYMM15 } },
            };
        };

        for (RYMM.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesRegST(name: []const u8) ?Register {
        const RST = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "st0", .reg = .{ .regST = RegSTIdx.RegST0 } },
                .{ .name = "st1", .reg = .{ .regST = RegSTIdx.RegST1 } },
                .{ .name = "st2", .reg = .{ .regST = RegSTIdx.RegST2 } },
                .{ .name = "st3", .reg = .{ .regST = RegSTIdx.RegST3 } },
                .{ .name = "st4", .reg = .{ .regST = RegSTIdx.RegST4 } },
                .{ .name = "st5", .reg = .{ .regST = RegSTIdx.RegST5 } },
                .{ .name = "st6", .reg = .{ .regST = RegSTIdx.RegST6 } },
                .{ .name = "st7", .reg = .{ .regST = RegSTIdx.RegST7 } },
            };
        };

        for (RST.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesRegDR(name: []const u8) ?Register {
        const RDR = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "dr0", .reg = .{ .regDR = RegDRIdx.RegDR0 } },
                .{ .name = "dr1", .reg = .{ .regDR = RegDRIdx.RegDR1 } },
                .{ .name = "dr2", .reg = .{ .regDR = RegDRIdx.RegDR2 } },
                .{ .name = "dr3", .reg = .{ .regDR = RegDRIdx.RegDR3 } },
                .{ .name = "dr4", .reg = .{ .regDR = RegDRIdx.RegDR4 } },
                .{ .name = "dr5", .reg = .{ .regDR = RegDRIdx.RegDR5 } },
                .{ .name = "dr6", .reg = .{ .regDR = RegDRIdx.RegDR6 } },
                .{ .name = "dr7", .reg = .{ .regDR = RegDRIdx.RegDR7 } },
            };
        };

        for (RDR.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesRegCR(name: []const u8) ?Register {
        const RCR = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "cr0", .reg = .{ .regCR = RegCRIdx.RegCR0 } },
                .{ .name = "cr1", .reg = .{ .regCR = RegCRIdx.RegCR1 } },
                .{ .name = "cr2", .reg = .{ .regCR = RegCRIdx.RegCR2 } },
                .{ .name = "cr3", .reg = .{ .regCR = RegCRIdx.RegCR3 } },
                .{ .name = "cr4", .reg = .{ .regCR = RegCRIdx.RegCR4 } },
                .{ .name = "cr5", .reg = .{ .regCR = RegCRIdx.RegCR5 } },
                .{ .name = "cr6", .reg = .{ .regCR = RegCRIdx.RegCR6 } },
                .{ .name = "cr7", .reg = .{ .regCR = RegCRIdx.RegCR7 } },
            };
        };

        for (RCR.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    inline fn matchesRegSeg(name: []const u8) ?Register {
        const RSeg = struct {
            const regs: []const RegId = &[_]RegId{
                .{ .name = "es", .reg = .regES },
                .{ .name = "cs", .reg = .regCS },
                .{ .name = "ss", .reg = .regSS },
                .{ .name = "ds", .reg = .regDS },
                .{ .name = "fs", .reg = .regFS },
                .{ .name = "gs", .reg = .regGS },
            };
        };

        for (RSeg.regs) |reg| {
            if (std.mem.eql(u8, reg.name, name)) return reg.reg;
        }

        return null;
    }

    pub fn isReg(name: []const u8) ?Register {
        if (matchesReg8Gp(name)) |reg| {
            return reg;
        } else if (matchesReg16Gp(name)) |reg| {
            return reg;
        } else if (matchesReg32Gp(name)) |reg| {
            return reg;
        } else if (matchesReg64Gp(name)) |reg| {
            return reg;
        } else if (matchesRegMMX(name)) |reg| {
            return reg;
        } else if (matchesRegXMM(name)) |reg| {
            return reg;
        } else if (matchesRegYMM(name)) |reg| {
            return reg;
        } else if (matchesRegST(name)) |reg| {
            return reg;
        } else if (matchesRegDR(name)) |reg| {
            return reg;
        } else if (matchesRegCR(name)) |reg| {
            return reg;
        } else if (matchesRegSeg(name)) |reg| {
            return reg;
        } else return null;
    }
    
    pub inline fn isSegReg(reg: Register) bool {
        switch (reg) {
            .regES....regFS => return true,
            else => return false,
        }
    }
};
