#version 100

attribute vec2 VertexPosition;
attribute mediump vec2 VertexTextureCoords;
varying mediump vec2 TextureCoords;

void main()
{
    gl_Position = vec4(VertexPosition, 0.0, 1.0);
    TextureCoords = VertexTextureCoords;
}
