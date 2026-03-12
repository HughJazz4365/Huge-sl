#version 430 core
#extension GL_EXT_nonuniform_qualifier : enable
layout(location = 0) out vec4 col;
layout(binding = 0) buffer B{
    float elem;
} b;
void main(){
    float factor = 1.0;  
    if(b.elem > 2.0){
        factor *= 4.0;
    }
    if(b.elem < 2.0){
        factor *= 4.0;
    }
    if(b.elem >= 2.0){
        factor *= 4.0;
    }
    if(b.elem <= 2.0){
        factor *= 4.0;
    }
    if(b.elem == 2.0){
        factor *= 4.0;
    }
    if(b.elem != 2.0){
        factor *= 4.0;
    }
    
    col = vec4(factor);
}
