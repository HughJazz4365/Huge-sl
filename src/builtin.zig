const std = @import("std");
const util = @import("util.zig");

pub const InterpolationQualifier = enum { smooth, flat, noperspective };
pub const UniformAccessQualifier = enum { private, public };
