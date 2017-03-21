#version 330 core

in vec2 vTexCoord;

uniform sampler2D shadowMap;

out vec4 color;

void main()
{
  float depthValue = texture(shadowMap, vTexCoord).r;
  color = vec4(vec3(depthValue), 1.0);
}
