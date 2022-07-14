#version 330 core

layout (location = 0) in vec3 in_position;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_tex;

out vec3 v3_normal;
out vec3 v3_position;
out vec2 v2_tex;

uniform mat4 u_MVP;
uniform mat4 u_MV;

void main()
{
  v3_normal = normalize( vec3( u_MV * vec4(in_normal, 0.0)) );
  vec4 position = u_MVP * vec4(in_position, 1.0);
  v3_position = vec3( position );
  v2_tex = in_tex;
  gl_Position = position;
}
