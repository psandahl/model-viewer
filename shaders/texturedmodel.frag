#version 330 core

in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

uniform mat4 view;
uniform vec3 lightDir;
uniform vec3 lightColor;
uniform float ambientStrength;
uniform float specularStrength;
uniform int specularShine;

out vec4 color;

const vec3 staticColor = vec3(1.0, 0.0, 0.0);

vec3 transformLightDir()
{
  // The light direction is in model space. Transform to view space.
  return (view * vec4(lightDir, 0.0)).xyz;
}

vec3 calcAmbientColor()
{
  return lightColor * ambientStrength;
}

vec3 calcDiffuseColor()
{
  vec3 normal = normalize(vNormal);
  float diffuse = max(dot(normal, transformLightDir()), 0.0);

  return diffuse * lightColor;
}

vec3 calcSpecularColor()
{
  vec3 normal = normalize(vNormal);
  vec3 reflectDir = reflect(-transformLightDir(), normal);
  vec3 viewDir = normalize(vec3(0) - vPosition);
  float specular = pow(max(dot(viewDir, reflectDir), 0.0), specularShine);

  return specular * specularStrength * lightColor;
}

void main()
{
  vec3 fragmentColor = staticColor * (calcAmbientColor() + calcDiffuseColor() + calcSpecularColor());
  color = vec4(fragmentColor, 1.0);
}
