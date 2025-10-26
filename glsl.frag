#version 430 core
layout(location = 0) out vec4 col;
void main(){
    col = vec4(gl_Position.x, gl_Position.y,1.,1.);
}
