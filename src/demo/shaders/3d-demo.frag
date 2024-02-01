#version 330 core

in float height;

out vec4 color;

float lerp (in float from, in float to, in float pos) {
  float w = (pos - from) / (to - from);
 return w;
}

vec4 col_empty  = vec4(0.0);

vec4 terrain_color_by_height() {

  if (height <= 0.1){
    return vec4(0.0, 0.0, 1.0, 1.0);
  } else if (height >  0.1 &&
            height <= 0.4){
    return mix(vec4(0.0, 0.0, 1.0, 1.0), vec4(0.0, 1.0, 0.0, 1.0), height);
  }else if (height >  0.4 &&
            height <= 0.70){
    return mix(vec4(0.0, 1.0, 0.0, 1.0), vec4(1.0, 0.0, 0.0, 1.0), height);
  } else {
    return mix(vec4(1.0, 0.0, 0.0, 1.0), vec4(1.0, 1.0, 1.0, 1.0), height);
  }

}

void main () {
  color = terrain_color_by_height();
}
