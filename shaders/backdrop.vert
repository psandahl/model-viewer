#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

uniform mat4 mvp;
uniform mat4 model;

out vec3 vPosition;
out vec3 vNormal;

void main()
{
  vPosition = (model * vec4(position, 1.0)).xyz;
  vNormal = (model * vec4(position, 0.0)).xyz;
  gl_Position = mvp * vec4(position, 1.0);
}
