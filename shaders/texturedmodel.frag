#version 330 core

in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

uniform mat4 view;
uniform vec3 lightDir;
uniform vec3 lightColor;
uniform float ambientStrength;

out vec4 color;

const vec3 staticColor = vec3(1.0, 0.0, 0.0);

vec3 calcAmbientColor()
{
  return lightColor * ambientStrength;
}

vec3 calcDiffuseColor()
{
  // The light direction is in model space. Transform to view space.
  vec3 tLightDir = (view * vec4(lightDir, 0.0)).xyz;

  // Normals are transformed to view space in vertex shader. Just normalize.
  vec3 normal = normalize(vNormal);
  float diffuse = max(dot(normal, tLightDir), 0.0);

  return diffuse * lightColor;
}

void main()
{
  vec3 fragmentColor = staticColor * (calcAmbientColor() + calcDiffuseColor());
  color = vec4(fragmentColor, 1.0);
}
