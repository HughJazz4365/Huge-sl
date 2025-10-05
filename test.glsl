#version 450 

layout(location = 0)in float i;
layout(location = 0)out vec4 col;
float v = 1.3;

void main(){
    v += i;
    col = vec4(v);
}
