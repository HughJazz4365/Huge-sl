#version 430 core
layout(location = 0) out vec4 col;
layout(binding = 0, set = 0) uniform sampler2D s;
void main(){
    col = texture(s, vec2(1.,1.));
}
