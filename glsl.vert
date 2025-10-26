#version 430 core

const vec2 verts[4] = {
		{0.,0.},
		{0.,1.},
		{1.,0.},
		{1.,1.}
};
void main(){
	vec2 kek = verts[gl_VertexIndex];
    gl_Position = vec4(kek.x, 1., 1., 1.);

}
