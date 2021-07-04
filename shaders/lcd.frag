#version 320 es

in mediump vec2 TextureCoords;
uniform sampler2D LcdTexture;
out lowp vec4 FragColor;

void main()
{
    FragColor = texture(LcdTexture, TextureCoords).bgra;
}
