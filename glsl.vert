#version 430 core

const vec2 verts[4] = {
		{0.,0.},
		{0.,1.},
		{1.,0.},
		{1.,1.}
};
void main(){
    gl_Position = vec4(verts[gl_VertexIndex], 1., 1.);

}
