pub const Prefix = enum(u8) {
    Rex = 0x40,
    RexW = 0x48,
    RexR = 0x44,
    RexX = 0x42,
    RexB = 0x41,
    OpOvrd = 0x66,
    AddOvrd = 0x67,
    Lock = 0xf0,
    Repnez = 0xf2,
    Repez = 0xf3,
    CsOvrd = 0x2e,
    SsOvrd = 0x36,
    DsOvrd = 0x3e,
    EsOvrd = 0x26,
    FsOvrd = 0x64,
    GsOvrd = 0x65,
};
