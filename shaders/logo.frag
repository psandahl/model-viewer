#version 330 core

in vec2 vTexCoord;

uniform sampler2D logo;

out vec4 color;

void main()
{
  vec4 pixel = texture2D(logo, vec2(vTexCoord.x, 1 - vTexCoord.y));
  if (pixel.a < 0.5)
  {
    discard;
  }
  else
  {
    color = pixel;
  }
}
