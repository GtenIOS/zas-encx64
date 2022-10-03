const std = @import("std");
const Register = @import("./reg.zig").Register;
const Imm = @import("common").imm.Imm;
const Memory = @import("./mem.zig").Memory;
const MOffset = @import("./mem.zig").MOffset;
const Rel = @import("./mem.zig").Rel;
const Pntr = @import("./pntr.zig").Pntr;

pub const RegMem = union(enum) {
    reg: Register,
    mem: Memory,
    const Self = @This();

    pub inline fn size(self: Self) u16 {
        switch (self) {
            .reg => |r| return r.size(),
            .mem => |m| return m.size(),
        }
    }

    pub inline fn matches(self: Self, other: Self) bool {
        return self.size() == other.size();
    }

    pub inline fn canBeMOffset(self: Self) bool {
        switch (self) {
            .mem => |m| return m.canBeMOffset(),
            else => return false,
        }
    }

    pub inline fn encode(self: Self, allocator: std.mem.Allocator, encoding: OperandEncoding, oper_idx: u8, po_byte: *u8, modrm_byte: ?*u8, rex_byte: ?*u8) !std.ArrayList(u8) {
        switch (self) {
            .reg => |rval| return try rval.encode(allocator, encoding, oper_idx, po_byte, modrm_byte, rex_byte),
            .mem => |mval| return try mval.encode(allocator, modrm_byte, rex_byte),
        }
    }
};

pub const Operand = union(enum) {
    reg: Register,
    mem: Memory,
    mofst: MOffset,
    rel: Rel,
    rm: RegMem,
    imm: Imm,
    pntr: Pntr,
    const Self = @This();

    // `self` must always be from the isa template
    pub inline fn matches(self: Self, other: Self) bool {
        switch (self) {
            .reg => |reg_a| {
                switch (other) {
                    .reg => |reg_b| return reg_a.matches(reg_b),
                    .rm => |rm_b| return reg_a.size() == rm_b.size(),
                    else => return false,
                }
            },
            .mem => |m_a| {
                switch (other) {
                    .mem => |m_b| return m_b.size() == 0 or m_a.size() == m_b.size(),
                    .rm => |rm_b| return rm_b.size() == 0 or m_a.size() == rm_b.size(),
                    else => return false,
                }
            },
            .rm => |rm_a| {
                switch (other) {
                    .reg => |r_b| return rm_a.size() == r_b.size(),
                    .mem => |m_b| return m_b.size() == 0 or rm_a.size() == m_b.size(),
                    .rm => |rm_b| return rm_b.size() == 0 or rm_a.size() == rm_b.size(),
                    .mofst => return true, // mofst can be implicitly converted to memory operand
                    else => return false,
                }
            },
            .imm => |imm_a| {
                switch (other) {
                    .imm => |imm_b| return imm_a.matches(imm_b),
                    else => return false,
                }
            },
            .mofst => |mofst_a| {
                switch (other) {
                    .mofst => |mofst_b| return mofst_a.matches(mofst_b),
                    else => return false,
                }
            },
            .rel => |rel_a| {
                switch (other) {
                    .rel => |rel_b| return rel_a.matches(rel_b),
                    else => return false,
                }
            },
            .pntr => |pntr_a| {
                switch (other) {
                    .pntr => |pntr_b| return pntr_a.size() == pntr_b.size(),
                    else => return false,
                }
            },
        }
    }

    pub inline fn encode(self: Self, allocator: std.mem.Allocator, encoding: OperandEncoding, oper_idx: u8, po_byte: *u8, modrm_byte: ?*u8, rex_byte: ?*u8) !std.ArrayList(u8) {
        switch (self) {
            .imm => |ival| return try ival.encode(allocator),
            .reg => |rval| return try rval.encode(allocator, encoding, oper_idx, po_byte, modrm_byte, rex_byte),
            .mem => |mval| return try mval.encode(allocator, modrm_byte, rex_byte),
            .rm => |rmval| return rmval.encode(allocator, encoding, oper_idx, po_byte, modrm_byte, rex_byte),
            .mofst => |mofst| return try mofst.encode(allocator),
            .rel => |rel| return try rel.encode(allocator),
            .pntr => |pntr| return try pntr.encode(allocator),
        }
    }

    pub inline fn isReg(self: Self) bool {
        switch (self) {
            .reg => return true,
            .rm => |regm| {
                switch (regm) {
                    .reg => return true,
                    else => return false,
                }
            },
            else => return false,
        }
    }

    pub inline fn regIdx(self: Self) !u8 {
        switch (self) {
            .reg => |r| return r.idx(),
            .rm => |rm| {
                switch (rm) {
                    .reg => |r| return r.idx(),
                    else => return error.OperandNotARegister,
                }
            },
            else => return error.OperandNotARegister,
        }
    }

    pub inline fn isMemory(self: Self) bool {
        switch (self) {
            .rm => |regm| {
                switch (regm) {
                    .mem => return true,
                    else => return false,
                }
            },
            .mem => return true,
            else => return false,
        }
    }

    pub inline fn memory(self: Self) !Memory {
        switch (self) {
            .mem => |m| return m,
            .rm => |rm| {
                switch (rm) {
                    .mem => |m| return m,
                    else => return error.OperandNotAMemory,
                }
            },
            else => return error.OperandNotAMemory,
        }
    }

    pub inline fn isMOffset(self: Self) bool {
        switch (self) {
            .mofst => return true,
            .mem => |m| return m.canBeMOffset(),
            .rm => |rm| return rm.canBeMOffset(),
            else => return false,
        }
    }

    pub inline fn isRel(self: Self) bool {
        switch (self) {
            .rel => return true,
            else => return false,
        }
    }

    pub inline fn isImm(self: Self) bool {
        switch (self) {
            .imm => return true,
            else => return false,
        }
    }

    pub inline fn immediate(self: Self) !Imm {
        switch (self) {
            .imm => |imm| return imm,
            else => return error.OperandNotAnImmediate,
        }
    }

    pub inline fn size(self: Self) u16 {
        switch (self) {
            .imm => |imm| return imm.size(),
            .reg => |reg| return reg.size(),
            .rm => |rm| return rm.size(),
            .mem => |mem| return mem.size(),
            .mofst => |mofst| return mofst.size(),
            .rel => |rel| return rel.size(),
            .pntr => |pntr| return pntr.size(),
        }
    }

    pub inline fn typeStr(self: Self) []const u8 {
        switch (self) {
            .imm => return "imm",
            .reg => return "reg",
            .rm => return "rm",
            .mem => return "mem",
            .mofst => return "mofst",
            .rel => return "rel",
            .pntr => return "pntr",
        }
    }
};

pub const OperandEncoding = union(enum) {
    po,
    d: u3,
    i,
    ri,
    rm,
    mr,
    rmo,
    mor,
    const Self = @This();
    pub inline fn isCovariant(self: Self, other: Self) bool {
        return if ((@enumToInt(self) == @enumToInt(other)) or (self == .rm and other == .mr) or (self == .mr and other == .rm) or ((self == .d and other == .ri) or (self == .ri or other == .d))) true else false;
    }

    pub inline fn typeStr(self: Self) []const u8 {
        switch (self) {
            .po => return "po",
            .d => return "d",
            .i => return "i",
            .ri => return "ri",
            .rm => return "rm",
            .mr => return "mr",
            .rmo => return "rmo",
            .mor => return "mor",
        }
    }
};
