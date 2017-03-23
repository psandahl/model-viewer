#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

uniform mat4 mvp;
uniform mat4 view;
uniform mat4 model;
uniform mat4 lightVP;

out vec3 vPosition;
out vec3 vNormal;
out vec4 vPositionLightSpace;

void main()
{
  mat4 mv = view * model;
  vPosition = (mv * vec4(position, 1.0)).xyz;
  vNormal = (mv * vec4(normal, 0.0)).xyz;
  vPositionLightSpace = lightVP * model * vec4(position, 1.0);
  gl_Position = mvp * vec4(position, 1.0);
}
