in normal : vec3

const main = entrypoint(.fragment){
	out out_col : vec4

	const exposure: f32 = -1.2 ^ 2
	var albedo: vec3 = vec3{0.01 + 0.3, 3.3, 1.76 - 0.25}} 

	out_col = .{albedo * exposure, 1}
	return
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
