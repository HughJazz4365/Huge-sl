#version 450 
layout(location = 0)out vec4 col;
void main(){
    col = vec4(gl_VertexIndex, gl_InstanceIndex, 0.0, 0.0);
    gl_Position  = vec4(0.0);
}
