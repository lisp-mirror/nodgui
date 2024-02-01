#version 330 core

layout (location = 0)  in vec4 position;

uniform mat4 modelview_matrix;

uniform mat4 projection_matrix;

out float height;

mat3 normal_matrix = mat3(modelview_matrix);

void main () {
  height                 = position.y / 128.0;
  gl_Position            = projection_matrix * modelview_matrix * position;
}
