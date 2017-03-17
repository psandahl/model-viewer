#version 330 core

in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

uniform vec3 lightPos;
uniform vec3 lightColor;
uniform float ambientStrength;

out vec4 color;

const vec3 staticColor = vec3(1.0, 0.0, 0.0);

vec3 calcAmbientColor()
{
  return lightColor * ambientStrength;
}

void main()
{
  color = vec4(staticColor * calcAmbientColor(), 1.0);
}
