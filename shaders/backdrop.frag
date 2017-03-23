#version 330 core

in vec3 vPosition;
in vec3 vNormal;

uniform vec3 lightDir;
uniform vec3 lightColor;
uniform float ambientStrength;
uniform float specularStrength;
uniform float specularShine;

out vec4 color;

const vec3 grey = vec3(192.0 / 255.0);

void main()
{
  color = vec4(grey, 1);
}
