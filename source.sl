in normal : vec3 = -1.2*(2+8)

const main = entrypoint(.fragment){
	out out_col : vec4

	const exposure: f32 = -1.2 ^ 2
	var albedo: vec4 = .{0.01 + 0.3, 0.1, 1.89 - 0.3}

	albedo *= exposure
	out_col = .{albedo, 1}
}

// const double = fn(x) f32{
// 	return x * 2
// }
//things to do there:
//COMPTIME EXECUTION
//[1 ]: var decl
//[2 ]: vector types
//[3 ]: global stage io
//[4 ]: entrypoint type
//[5 ]: @builtin scope //@ShaderStage.fragment
//[6 ]: enums
//[7 ]: entrypoint scope stage io
//[8 ]: constant variables
//[9 ]: float literals
//[10]: int literals
//[11]: power operator
//[12]: mutable variables
//[13]: vector constructors
//[14]: type inference
//[15]: '[op]=' syntax
//[16]: vector swizzle
//[17]: '1' in vector swizzle
