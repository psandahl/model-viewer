#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

uniform mat4 mvp;
uniform mat4 view;
uniform mat4 model;

out vec3 vPosition;
out vec3 vNormal;

void main()
{
  // Lightning calculations are in view space, so make a transformation
  // matrix of mode and view.
  mat4 mv = view * model;

  // Interpolate attributes:
  // Transform to view space.
  vPosition = (mv * vec4(position, 1.0)).xyz;
  vNormal = (mv * vec4(normal, 0.0)).xyz;

  // Transform vertex position.
  gl_Position = mvp * vec4(position, 1.0);
}
