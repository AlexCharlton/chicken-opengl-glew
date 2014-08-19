(module opengl-glew *
(import chicken scheme bind foreign srfi-4)

#>
#include <stdlib.h>
#include <stdio.h>
#include <GL/glew.h>
static void showInfoLog(GLuint object){
    GLint logLength;
    char *log;
    glGetShaderiv(object, GL_INFO_LOG_LENGTH, &logLength);
    log = malloc(logLength);
    glGetShaderInfoLog(object, logLength, NULL, log);
    fprintf(stderr, "%s\n", log);
    free(log);
}
<#

(bind-rename/pattern "^glew" "")
(bind-rename/pattern "^GL_([A-Z_].+)$" "+\\1+")
(bind-rename/pattern "([^_])([123])D" "\\1-\\2d")
(bind-rename/pattern "^gl" "")
(bind-rename/pattern "^Is(.*)$" "\\1?")

(bind-options default-renaming: ""
              export-constants: #t)

(bind-file "gl.h")

(bind*
 #<<END
unsigned int makeShader(unsigned int type, const char *source){
    GLuint shader;
    GLint shaderOk;
    shader = glCreateShader(type);
    glShaderSource(shader, 1, (const GLchar**)&source, NULL);
    glCompileShader(shader);
    glGetShaderiv(shader, GL_COMPILE_STATUS, &shaderOk);
    if (!shaderOk) {
       fprintf(stderr, "Failed to compile shader:\n\n");
       int i = 0, line = 2;
       fprintf(stderr, "   1|  ", line);
       while (source[i]){
         fputc(source[i], stderr);
         if(source[i] == '\n'){
           fprintf(stderr, "%4d|  ", line);
           line++;
         }
         i++;
       }
       fprintf(stderr, "\n\n");
       showInfoLog(shader);
       glDeleteShader(shader);
       return 0;
    }
    return shader;
}
END
)

(bind*
 #<<END
void checkError(){
    switch (glGetError()){
    case 0: return;
    case GL_INVALID_ENUM: fprintf(stderr, "GL error: Invalid enum\n"); break;
    case GL_INVALID_VALUE: fprintf(stderr, "GL error: Invalid value\n"); break;
    case GL_INVALID_OPERATION: fprintf(stderr, "GL error: Invalid operation\n"); break;
    case GL_STACK_OVERFLOW: fprintf(stderr, "GL error: Stack overflow\n"); break;
    case GL_STACK_UNDERFLOW: fprintf(stderr, "GL error: Stack underflow\n"); break;
    case GL_OUT_OF_MEMORY: fprintf(stderr, "GL error: Out of memory\n"); break;
    case GL_TABLE_TOO_LARGE: fprintf(stderr, "GL error: Table too large\n"); break;
    default: fprintf(stderr, "GL error: Unknown\n");
    }
}
END
)

(define (make-program shaders #!optional [program (create-program)])
  (let loop ([shaders shaders])
    (if (not (null? shaders))
        (begin (attach-shader program (car shaders))
               (loop (cdr shaders)))))
  (link-program program)
  ((foreign-lambda* unsigned-integer ((unsigned-integer program))
     "GLint programOk;
      glGetProgramiv(program, GL_LINK_STATUS, &programOk);
      if (!programOk) {
          fprintf(stderr, \"Failed to link shader program:\\n\\n\");
          showInfoLog(program);
          glDeleteProgram(program);
          C_return(0);
      }
      C_return(program);")
   program))

(bind "bool glewIsSupported(char *name);")

(bind* #<<END
void init(){
  glewExperimental = GL_TRUE;
  GLenum err = glewInit();
  if (GLEW_OK != err){
    fprintf(stderr, "Error: %s\n", glewGetErrorString(err));
    exit(1);
  }
}
END
)

(define (gen-buffer)
  (let ([vec (make-u32vector 1)])
    (gen-buffers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-buffer x)
  (let ([vec (u32vector x)])
    (delete-buffers 1 vec)))

(define (gen-framebuffer)
  (let ([vec (make-u32vector 1)])
    (gen-framebuffers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-framebuffer x)
  (let ([vec (u32vector x)])
    (delete-framebuffers 1 vec)))

(define (gen-program-pipeline)
  (let ([vec (make-u32vector 1)])
    (gen-program-pipelines 1 vec)
    (u32vector-ref vec 0)))

(define (delete-program-pipeline x)
  (let ([vec (u32vector x)])
    (delete-program-pipelines 1 vec)))

(define (gen-query)
  (let ([vec (make-u32vector 1)])
    (gen-queries 1 vec)
    (u32vector-ref vec 0)))

(define (delete-query x)
  (let ([vec (u32vector x)])
    (delete-queries 1 vec)))

(define (gen-renderbuffer)
  (let ([vec (make-u32vector 1)])
    (gen-renderbuffers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-renderbuffer x)
  (let ([vec (u32vector x)])
    (delete-renderbuffers 1 vec)))

(define (gen-sampler)
  (let ([vec (make-u32vector 1)])
    (gen-samplers 1 vec)
    (u32vector-ref vec 0)))

(define (delete-sampler x)
  (let ([vec (u32vector x)])
    (delete-samplers 1 vec)))

(define (gen-texture)
  (let ([vec (make-u32vector 1)])
    (gen-textures 1 vec)
    (u32vector-ref vec 0)))

(define (delete-texture x)
  (let ([vec (u32vector x)])
    (delete-textures 1 vec)))

(define (gen-transform-feedback)
  (let ([vec (make-u32vector 1)])
    (gen-transform-feedbacks 1 vec)
    (u32vector-ref vec 0)))

(define (delete-transform-feedback x)
  (let ([vec (u32vector x)])
    (delete-transform-feedbacks 1 vec)))

(define (gen-vertex-array)
  (let ([vec (make-u32vector 1)])
    (gen-vertex-arrays 1 vec)
    (u32vector-ref vec 0)))

(define (delete-vertex-array x)
  (let ([vec (u32vector x)])
    (delete-vertex-arrays 1 vec)))

(register-feature! #:opengl-glew)

) ; module end
