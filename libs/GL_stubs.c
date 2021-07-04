#define GL_GLEXT_PROTOTYPES
#include <GL/gl.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/bigarray.h>

#define CAMLvoid CAMLunused_start value unit CAMLunused_end

#define Val_none Val_int(0)

const int ml_to_gl_capability[] = {
    GL_BLEND,
    GL_CULL_FACE,
    GL_DEPTH_TEST,
    GL_DITHER,
    GL_POLYGON_OFFSET_FILL,
    GL_SAMPLE_ALPHA_TO_COVERAGE,
    GL_SAMPLE_COVERAGE,
    GL_SCISSOR_TEST,
    GL_STENCIL_TEST
};

const int ml_to_gl_blend_function[] = {
    GL_ZERO,
    GL_ONE,
    GL_SRC_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_DST_COLOR,
    GL_ONE_MINUS_DST_COLOR,
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA,
    GL_DST_ALPHA,
    GL_ONE_MINUS_DST_ALPHA,
    GL_CONSTANT_COLOR,
    GL_ONE_MINUS_CONSTANT_COLOR,
    GL_CONSTANT_ALPHA,
    GL_ONE_MINUS_CONSTANT_ALPHA,
    GL_SRC_ALPHA_SATURATE
};

const int ml_to_gl_buffer_bit[] = {
    GL_COLOR_BUFFER_BIT,
    GL_DEPTH_BUFFER_BIT,
    GL_STENCIL_BUFFER_BIT
};

const int ml_to_gl_shader_kind[] = {
    GL_VERTEX_SHADER,
    GL_FRAGMENT_SHADER
};

const int ml_to_gl_attachment[] = {
    GL_COLOR_ATTACHMENT0,
    GL_DEPTH_ATTACHMENT,
    GL_STENCIL_ATTACHMENT
};

const int ml_to_gl_buffer_target[] = {
    GL_ARRAY_BUFFER,
    GL_ELEMENT_ARRAY_BUFFER
};

const int ml_to_gl_texture_target[] = {
    GL_TEXTURE_2D,
    GL_TEXTURE_CUBE_MAP_POSITIVE_X,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
    GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
    GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
};

const int ml_to_gl_texture_format[] = {
    GL_ALPHA,
    GL_LUMINANCE,
    GL_LUMINANCE_ALPHA,
    GL_RGB,
    GL_RGBA
};

const int ml_to_gl_pixel_data_type[] = {
    GL_UNSIGNED_BYTE,
    GL_UNSIGNED_SHORT_5_6_5,
    GL_UNSIGNED_SHORT_4_4_4_4,
    GL_UNSIGNED_SHORT_5_5_5_1
};

const int ml_to_gl_texture_filter[] = {
    GL_NEAREST,
    GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_LINEAR
};

const int ml_to_gl_texture_wrap[] = {
    GL_CLAMP_TO_EDGE,
    GL_MIRRORED_REPEAT,
    GL_REPEAT
};

const int ml_to_gl_texture_parameter[] = {
    GL_TEXTURE_MIN_FILTER,
    GL_TEXTURE_MAG_FILTER,
    GL_TEXTURE_WRAP_S,
    GL_TEXTURE_WRAP_T
};

const int ml_to_gl_pixel_param[] = {
    GL_PACK_ALIGNMENT,
    GL_UNPACK_ALIGNMENT
};

const int ml_to_gl_usage[] = {
    GL_STREAM_DRAW,
    GL_STATIC_DRAW,
    GL_DYNAMIC_DRAW
};

const int ml_to_gl_type[] = {
    GL_FLOAT,
    GL_UNSIGNED_SHORT
};

const int ml_to_gl_draw_mode[] = {
    GL_POINTS,
    GL_LINE_STRIP,
    GL_LINE_LOOP,
    GL_LINES,
    GL_TRIANGLE_STRIP,
    GL_TRIANGLE_FAN,
    GL_TRIANGLES
};

CAMLprim value caml_glGetError(CAMLvoid)
{
    return Val_int(glGetError());
}

CAMLprim value caml_glEnable(value cap)
{
    glEnable(ml_to_gl_capability[Int_val(cap)]);
    return Val_unit;
}

CAMLprim value caml_glDisable(value cap)
{
    glDisable(ml_to_gl_capability[Int_val(cap)]);
    return Val_unit;
}

CAMLprim value caml_glBlendFunc(value sfactor, value dfactor)
{
    glBlendFunc(ml_to_gl_blend_function[Int_val(sfactor)],
                ml_to_gl_blend_function[Int_val(dfactor)]);
    return Val_unit;
}

CAMLprim value caml_glClear(value buffers)
{
    glClear(caml_convert_flag_list(buffers, (int*)ml_to_gl_buffer_bit));
    return Val_unit;
}

CAMLprim value caml_glViewport(value x, value y, value width, value height)
{
    glViewport(Int_val(x), Int_val(y), Int_val(width), Int_val(height));
    return Val_unit;
}

CAMLprim value caml_glCreateShader(value shader_kind)
{
    return Val_int(glCreateShader(ml_to_gl_shader_kind[Int_val(shader_kind)]));
}

CAMLprim value caml_glShaderSource(value shader, value source)
{
    glShaderSource(Int_val(shader), 1, (const char**)&source, NULL);
    return Val_unit;
}

CAMLprim value caml_glCompileShader(value shader)
{
    glCompileShader(Int_val(shader));
    return Val_unit;
}

CAMLprim value caml_glGetShaderInfoLog(value shader)
{
    CAMLparam0();
    CAMLlocal2(ret, str);
    GLint length;

    glGetShaderiv(Int_val(shader), GL_INFO_LOG_LENGTH, &length);
    if (length <= 1)
        ret = Val_none;
    else
    {
        str = caml_alloc_string(length - 1);
        glGetShaderInfoLog(
            Int_val(shader), length, NULL, (char*)Bytes_val(str));
        ret = caml_alloc_small(1, 0);
        Field(ret, 0) = str;
    }
    CAMLreturn(ret);
}

CAMLprim value caml_glCreateProgram(CAMLvoid)
{
    return Val_int(glCreateProgram());
}

CAMLprim value caml_glAttachShader(value program, value shader)
{
    glAttachShader(Int_val(program), Int_val(shader));
    return Val_unit;
}

CAMLprim value caml_glLinkProgram(value program)
{
    glLinkProgram(Int_val(program));
    return Val_unit;
}

CAMLprim value caml_glGetProgramInfoLog(value program)
{
    CAMLparam0();
    CAMLlocal2(ret, str);
    GLint length;

    glGetProgramiv(Int_val(program), GL_INFO_LOG_LENGTH, &length);
    if (length <= 1)
        ret = Val_none;
    else
    {
        str = caml_alloc_string(length - 1);
        glGetProgramInfoLog(
            Int_val(program), length, NULL, (char*)Bytes_val(str));
        ret = caml_alloc_small(1, 0);
        Field(ret, 0) = str;
    }
    CAMLreturn(ret);
}

CAMLprim value caml_glUseProgram(value program)
{
    glUseProgram(Int_val(program));
    return Val_unit;
}

CAMLprim value caml_glGenFramebuffer(CAMLvoid)
{
    GLuint framebuffer;

    glGenFramebuffers(1, &framebuffer);
    return Val_int(framebuffer);
}

CAMLprim value caml_glBindFramebuffer(value framebuffer)
{
    glBindFramebuffer(GL_FRAMEBUFFER, Int_val(framebuffer));
    return Val_unit;
}

CAMLprim value caml_glFramebufferTexture2D(
    value attachment, value textarget, value texture, value level)
{
    glFramebufferTexture2D(
        GL_FRAMEBUFFER, ml_to_gl_attachment[Int_val(attachment)],
        ml_to_gl_texture_target[Int_val(textarget)], Int_val(texture),
        Int_val(level));
    return Val_unit;
}

CAMLprim value caml_glGenBuffer(CAMLvoid)
{
    GLuint buffer;

    glGenBuffers(1, &buffer);
    return Val_int(buffer);
}

CAMLprim value caml_glDeleteBuffer(value ml_buffer)
{
    GLuint buffer = Int_val(ml_buffer);

    glDeleteBuffers(1, &buffer);
    return Val_unit;
}

CAMLprim value caml_glBindBuffer(value target, value buffer)
{
    glBindBuffer(ml_to_gl_buffer_target[Int_val(target)], Int_val(buffer));
    return Val_unit;
}

CAMLprim value caml_glBufferData(value target, value data, value usage)
{
    struct caml_ba_array* ba_data = Caml_ba_array_val(data);

    glBufferData(
        ml_to_gl_buffer_target[Int_val(target)], caml_ba_byte_size(ba_data),
        ba_data->data, ml_to_gl_usage[Int_val(usage)]);
    return Val_unit;
}

CAMLprim value caml_glActiveTexture(value index)
{
    glActiveTexture(GL_TEXTURE0 + Int_val(index));
    return Val_unit;
}

CAMLprim value caml_glGenTexture(CAMLvoid)
{
    GLuint texture;

    glGenTextures(1, &texture);
    return Val_int(texture);
}

CAMLprim value caml_glBindTexture(value target, value texture)
{
    glBindTexture(ml_to_gl_texture_target[Int_val(target)], Int_val(texture));
    return Val_unit;
}

CAMLprim value caml_glTexImage2D(
    value target, value level, value internal_format, value width, value height,
    value border, value format, value ptype)
{
    glTexImage2D(
        ml_to_gl_texture_target[Int_val(target)], Int_val(level),
        ml_to_gl_texture_format[Int_val(internal_format)],
        Int_val(width), Int_val(height), Int_val(border),
        ml_to_gl_texture_format[Int_val(format)],
        ml_to_gl_pixel_data_type[Int_val(ptype)], NULL);
    return Val_unit;
}

CAMLprim value caml_glTexImage2D_byte(value* val_array, int val_count)
{
    (void)val_count;
    caml_glTexImage2D(val_array[0], val_array[1], val_array[2], val_array[3],
                      val_array[4], val_array[5], val_array[6], val_array[7]);
    return Val_unit;
}

CAMLprim value caml_glTexSubImage2D(
    value target, value level, value x_offset, value y_offset, value format,
    value ptype, value data)
{
    struct caml_ba_array* ba = Caml_ba_array_val(data);

    glTexSubImage2D(
        ml_to_gl_texture_target[Int_val(target)], Int_val(level),
        Int_val(x_offset), Int_val(y_offset), ba->dim[1], ba->dim[0],
        ml_to_gl_texture_format[Int_val(format)],
        ml_to_gl_pixel_data_type[Int_val(ptype)], ba->data);
    return Val_unit;
}

CAMLprim value caml_glTexSubImage2D_byte(value* val_array, int val_count)
{
    (void)val_count;
    caml_glTexSubImage2D(
        val_array[0], val_array[1], val_array[2], val_array[3], val_array[4],
        val_array[5], val_array[6]);
    return Val_unit;
}

CAMLprim value caml_glTexParameter(value target, value pname, value param)
{
    const GLint gl_param = (Int_val(pname) <= 1)
        ? ml_to_gl_texture_filter[Int_val(param)]
        : ml_to_gl_texture_wrap[Int_val(param)];

    glTexParameteri(ml_to_gl_texture_target[Int_val(target)],
                    ml_to_gl_texture_parameter[Int_val(pname)], gl_param);
    return Val_unit;
}

CAMLprim value caml_glPixelStorei(value pname, value param)
{
    glPixelStorei(ml_to_gl_pixel_param[Int_val(pname)], Int_val(param));
    return Val_unit;
}

CAMLprim value caml_glGetAttribLocation(value program, value name)
{
    return Val_int(glGetAttribLocation(Int_val(program), String_val(name)));
}

CAMLprim value caml_glGetUniformLocation(value program, value name)
{
    return Val_int(glGetUniformLocation(Int_val(program), String_val(name)));
}

CAMLprim value caml_glEnableVertexAttribArray(value location)
{
    glEnableVertexAttribArray(Int_val(location));
    return Val_unit;
}

CAMLprim value caml_glDisableVertexAttribArray(value location)
{
    glDisableVertexAttribArray(Int_val(location));
    return Val_unit;
}

CAMLprim value caml_glVertexAttribPointer(
    value location, value size, value type, value normalized, value stride,
    value pointer)
{
    glVertexAttribPointer(
        Int_val(location), Int_val(size), ml_to_gl_type[Int_val(type)],
        Bool_val(normalized), Int_val(stride), (void*)Long_val(pointer));
    return Val_unit;
}

CAMLprim value caml_glVertexAttribPointer_byte(value* val_array, int val_count)
{
    (void)val_count;
    caml_glVertexAttribPointer(val_array[0], val_array[1], val_array[2],
                               val_array[3], val_array[4], val_array[5]);
    return Val_unit;
}

CAMLprim value caml_glUniform1i(value location, value v)
{
    glUniform1i(Int_val(location), Int_val(v));
    return Val_unit;
}

CAMLprim value caml_glUniform1f(value location, value v)
{
    glUniform1f(Int_val(location), Double_val(v));
    return Val_unit;
}

CAMLprim value caml_glUniform2f(value location, value v1, value v2)
{
    glUniform2f(Int_val(location), Double_val(v1), Double_val(v2));
    return Val_unit;
}

CAMLprim value caml_glUniform3f(value location, value v1, value v2, value v3)
{
    glUniform3f(
        Int_val(location), Double_val(v1), Double_val(v2), Double_val(v3));
    return Val_unit;
}

CAMLprim value caml_glUniform4f(
    value location, value v1, value v2, value v3, value v4)
{
    glUniform4f(Int_val(location), Double_val(v1), Double_val(v2),
                Double_val(v3), Double_val(v4));
    return Val_unit;
}

CAMLprim value caml_glDrawArrays(value mode, value first, value count)
{
    glDrawArrays(
        ml_to_gl_draw_mode[Int_val(mode)], Int_val(first), Int_val(count));
    return Val_unit;
}

CAMLprim value caml_glDrawElements(
    value mode, value count, value type, value indices)
{
    glDrawElements(ml_to_gl_draw_mode[Int_val(mode)], Int_val(count),
                   ml_to_gl_type[Int_val(type)], (void*)Long_val(indices));
    return Val_unit;
}
