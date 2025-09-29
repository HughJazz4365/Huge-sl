#version 450 
layout(location = 0)out vec4 col;
layout(location = 0)in float f;

// layout(binding = 0)uniform sampler2D s;
// layout(binding = 1)uniform UBO{
    // float fff;
    // } ubo;
void main(){
    // col = texture(s, vec2(ubo.fff));
    const float mull = 43.4;
    const float f3 = f * f * f * mull;

    const float m = 38.1;
    const float f6 = f3 * f3 * m;

    col = vec4(f6) * vec4(0.4, 5.0, 5.5, -99.33);
    // gl_Position  = vec4(0.0);
}
