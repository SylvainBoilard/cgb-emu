#version 320 es

in vec2 VertexPosition;
in mediump vec2 VertexTextureCoords;
out mediump vec2 TextureCoords;

void main()
{
    gl_Position = vec4(VertexPosition, 0.0, 1.0);
    TextureCoords = VertexTextureCoords;
}
