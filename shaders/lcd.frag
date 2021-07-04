#version 100

varying mediump vec2 TextureCoords;
uniform sampler2D LcdTexture;

void main()
{
    gl_FragColor = texture2D(LcdTexture, TextureCoords).bgra;
}
