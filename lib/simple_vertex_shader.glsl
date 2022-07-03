#version 330 core
layout (location = 0) in vec3 vertexPosition_modelspace;
uniform mat4 MVP;
out vec4 vertexColor;
void main()
{
  gl_Position =vec4 (vertexPosition_modelspace,1.0);
  gl_Position = MVP * vec4(vertexPosition_modelspace, 1.0);
  vertexColor = vec4(vertexPosition_modelspace, 1);
}
