#version 450 
layout(location = 0)out vec4 col;
layout(binding = 0)uniform sampler2D s;
layout(binding = 1)uniform UBO{
    float fff;
    } ubo;
void main(){
    col = texture(s, vec2(ubo.fff));
    gl_Position  = vec4(0.0);
}
