#version 430 core
#extension GL_EXT_nonuniform_qualifier : enable
layout(location = 0) out vec4 col;
layout(binding = 0, set = 0) uniform sampler2D s[];
layout(binding = 0, set = 0) uniform sampler3D r[];
void main(){
    col = texture(s[int(gl_FragCoord.z)], gl_FragCoord.xy);
    col += texture(r[int(gl_FragCoord.x)], gl_FragCoord.xyz);
}
