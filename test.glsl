#version 450 
layout(location = 0)out vec4 col;
vec4 callme(vec4 v){
    if(length(v) >= 0)gl_Position += 0.1;

    return v * 2.0;
}
void main(){
    col = callme(gl_Position);
    gl_Position  = vec4(0.0);
}
