#version 430 core
#extension GL_EXT_nonuniform_qualifier : enable
layout(location = 0) in double mem;
layout(location = 0) out vec4 col;
layout(binding = 1) buffer B{
    float elem;
} b;
layout(binding = 0, set = 0) uniform sampler2D s[];
layout(binding = 0, set = 0) uniform sampler3D r[];
void main(){
    col = texture(s[int(0)], gl_FragCoord.xy) * b.elem;
}
