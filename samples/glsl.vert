#version 430 core

const vec2 verts[4] = {
		{0.,0.},
		{0.,1.},
		{1.,0.},
		{1.,1.}
};
layout(location = 0)out vec2 uv;

void main(){
	vec2 pos = verts[gl_VertexIndex];
    gl_Position = vec4(kek.xy, 0., 1.);
    uv = pos * .5 + vec2(.5);
}
