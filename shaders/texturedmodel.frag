#version 330 core

in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

uniform vec3 lightPos;
uniform vec3 lightColor;

out vec4 color;

const float ambientStrength = 0.5;
const vec3 staticColor = vec3(1.0, 0.0, 0.0);

void main()
{
  vec3 ambientColor = lightColor * ambientStrength;

  color = vec4(staticColor * ambientColor, 1.0);
}
