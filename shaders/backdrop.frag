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

vec3 calcAmbientColor()
{
  return lightColor * ambientStrength;
}

vec3 calcDiffuseColor()
{
  vec3 normal = normalize(vNormal);
  float diffuse = max(dot(normal, lightDir), 0.0);
  return lightColor * diffuse;
}

void main()
{
  vec3 finalColor = grey * (calcAmbientColor() + calcDiffuseColor());
  color = vec4(finalColor, 1);
}
