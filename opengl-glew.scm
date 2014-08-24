(module opengl-glew *
(import chicken scheme foreign)
(use bind)

(foreign-declare "#include <GL/glew.h>")

(bind-rename/pattern "^glew" "")
(bind-rename/pattern "^GL_([A-Z_].+)$" "+\\1+")
(bind-rename/pattern "([^_])([123])D" "\\1-\\2d")
(bind-rename/pattern "^gl" "")
(bind-rename/pattern "^Is(.*)$" "\\1?")

(bind-options default-renaming: ""
              export-constants: #t)

(bind-file "gl.h")

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

(register-feature! #:opengl-glew)

) ; module end
