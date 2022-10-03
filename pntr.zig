const std = @import("std");
pub const Pntr = union(enum) {
    pntr16_16: [2]u16, // Last `word` bytes will be encoded first, then the first
    pntr16_32: std.meta.Tuple(&[_]type{ u16, u32 }), // Same as above, last tuple property will be encoded first, then the first
    const Self = @This();

    pub inline fn size(self: Self) u16 {
        switch (self) {
            .pntr16_16 => return 32,
            .pntr16_32 => return 48,
        }
    }

    pub inline fn encode(self: Self, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        var bytes = std.ArrayList(u8).init(allocator);
        errdefer bytes.deinit();
        const pval_bytes = blk: {
            switch (self) {
                .pntr16_16 => |pval| {
                    break :blk &[_]u8{
                        @intCast(u8, pval[1] & 0x00ff),
                        @intCast(u8, pval[1] & 0xff00 >> 8),
                        @intCast(u8, pval[0] & 0x00ff),
                        @intCast(u8, pval[0] & 0xff00 >> 8),
                    };
                },
                .pntr16_32 => |pval| {
                    break :blk &[_]u8{
                        @intCast(u8, pval[1] & 0x000000ff),
                        @intCast(u8, pval[1] & 0x0000ff00 >> 8),
                        @intCast(u8, pval[1] & 0x00ff0000 >> 16),
                        @intCast(u8, pval[1] & 0xff000000 >> 24),
                        @intCast(u8, pval[0] & 0x00ff),
                        @intCast(u8, pval[0] & 0xff00 >> 8),
                    };
                },
            }
        };

        try bytes.appendSlice(pval_bytes);
        return bytes;
    }
};
