#version 330 core

in vec3 vPosition;
in vec3 vNormal;
in vec2 vTexCoord;

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
  vec3 normal = normalize(vNormal);
  float angle = dot(normal, lightDir);
  if (angle > 0)
  {
    return 10 * angle * lightColor;
  }
  else
  {
    // The color will be used for multiplication to create the final
    // color. So use the identity vector as color value when having no
    // diffuse effects.
    return vec3(1);
  }
}

void main()
{
  color = vec4(staticColor * calcAmbientColor() * calcDiffuseColor(), 1.0);
}
