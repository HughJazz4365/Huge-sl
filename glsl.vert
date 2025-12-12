#version 430 core

layout(location = 1)out double mem;
const vec2 verts[4] = {
		{0.,0.},
		{0.,1.},
		{1.,0.},
		{1.,1.}
};
layout(location = 0)out vec2 uv;
void main(){
	vec2 kek = verts[gl_VertexIndex];
    gl_Position = vec4(kek.x, 1., 1., 1.);
    uv = gl_Position.xy;

}
