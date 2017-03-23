#version 330 core

in vec3 vPosition;
in vec3 vNormal;
in vec4 vPositionLightSpace;

uniform mat4 view;
uniform vec3 lightDir;
uniform vec3 lightColor;
uniform float ambientStrength;
uniform float specularStrength;

uniform sampler2D shadowMap;

out vec4 color;

const vec3 grey = vec3(192.0 / 255.0);

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
  float specular = pow(max(dot(viewDir, reflectDir), 0.0), 32);

  return specular * specularStrength * lightColor;
}

float shadowBias()
{
  vec3 normal = normalize(vNormal);
  return max(0.05 * (1.0 - dot(normal, transformLightDir())), 0.005);
}

float calcShadow()
{
  vec3 projCoords = vPositionLightSpace.xyz / vPositionLightSpace.w;
  // Transform from [-1,1] range to [0,1] range.
  projCoords = projCoords * 0.5 + 0.5;
  float closestDepth = texture(shadowMap, projCoords.xy).r;
  float currentDepth = projCoords.z;

  // If the current position of further away than the closestDepth, we are
  // in shadow.
  return currentDepth - shadowBias() > closestDepth ? 1.0 : 0.0;
}

float calcPCFShadow()
{
  vec3 projCoords = vPositionLightSpace.xyz / vPositionLightSpace.w;
  projCoords = projCoords * 0.5 + 0.5;
  float currentDepth = projCoords.z;

  vec2 texelSize = 1.0 / textureSize(shadowMap, 0);
  float shadow = 0.0;
  float bias = shadowBias();
  for (int x = -1; x <= 1; ++x)
  {
    for (int y = -1; y <= 1; ++y)
    {
      float pcfDepth = texture(shadowMap, projCoords.xy + vec2(x, y) * texelSize).r;
      shadow += currentDepth - bias > pcfDepth ? 1.0 : 0.0;
    }
  }

  return shadow / 9.0;
}

void main()
{
  vec3 finalColor = grey *
    (calcAmbientColor() + (1.0 - calcPCFShadow()) * (calcDiffuseColor() + calcSpecularColor()));
  color = vec4(finalColor, 1.0);
}
