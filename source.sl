in normal : vec3 = .{-1.2*(2^8)}

const main = entrypoint(.fragment){
	out out_col : vec4

	const exposure: f32 = -1.2 ^ 2
	var albedo: vec3 = .{0.01 + 0.3, 3.3, 1.89 - 0.3}

	albedo *= .{exposure}
	out_col = .{albedo, 1}
}

// const double = fn(x) f32{
// 	return x * 2
// }

//things to do there:
//[3 ]: global stage io
//[5 ]: @builtin scope //@ShaderStage.fragment
//[6 ]: enums
//[7 ]: entrypoint scope stage io
//[12]: mutable variables
//[16]: vector swizzle
//[17]: '1' in vector swizzle
