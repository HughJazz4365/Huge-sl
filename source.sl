in normal : vec3

const main = entrypoint(.fragment){
	out out_col : vec4

	const exposure = -1.2^2
	var albedo = .{0.01 + 0.3, 0.1, 1.89 - 0.9}

	albedo *= exposure
	out_col = albedo.xyz1
}
//needed tokens:
//_in, _out, _const, _var
//entrypoint
//indetifier
//@":", @".", @"{}"
//@"=", @"+", @"-", @"*"
//float_literal, int_literal
// vec3, vec4
//swizzle

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
