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
uniform sampler2D sampler;

out vec4 color;

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
  vec3 pixelColor = texture2D(sampler, vec2(vTexCoord.x, 1.0 - vTexCoord.y)).xyz;
  vec3 finalColor = pixelColor * (calcAmbientColor() + calcDiffuseColor() + calcSpecularColor());
  color = vec4(finalColor, 1.0);
}
