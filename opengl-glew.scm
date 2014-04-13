(module opengl-glew *
(import chicken scheme bind foreign)

#>
#include <stdlib.h>
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
	fprintf(stderr, "Failed to compile %s:\n", source);
	showInfoLog(shader);
	glDeleteShader(shader);
	return 0;
    }
    return shader;
}
END
)

(define (make-program shaders)
  (let ([program (create-program)])
    (let loop ([shaders shaders])
      (if (not (null? shaders))
          (begin (attach-shader program (car shaders))
                 (loop (cdr shaders)))))
    (link-program program)
    ((foreign-lambda* unsigned-integer ((unsigned-integer program))
   "GLint programOk;
    glGetProgramiv(program, GL_LINK_STATUS, &programOk);
    if (!programOk) {
        fprintf(stderr, \"Failed to link shader program:\\n\");
        showInfoLog(program);
        glDeleteProgram(program);
        C_return(0);
    }
    C_return(program);")
     program)))

(bind "bool glewIsSupported(char *name);")

(bind* #<<END
void init(){
  GLenum err = glewInit();
  if (GLEW_OK != err){
    fprintf(stderr, "Error: %s\n", glewGetErrorString(err));
    exit(1);
  }
}
END
)

) ; module end
