(module opengl-glew *
(import chicken scheme foreign)
(use bind)

(cond-expand
  (gles (foreign-declare "#include <GLES3/gl3.h>"))
  (else (foreign-declare "#include <GL/glew.h>")))

(bind-rename/pattern "^glew" "")
(bind-rename/pattern "^GL_([A-Z_].+)$" "+\\1+")
(bind-rename/pattern "([^_])([123])D" "\\1-\\2d")
(bind-rename/pattern "^gl" "")
(bind-rename/pattern "^Is(.*)$" "\\1?")

(bind-options default-renaming: ""
              export-constants: #t)

(bind-file "gl.h")

(define (is-supported? str)
  (cond-expand
    (gles (error "is-supported? is not suported with GL ES"))
    (else ((foreign-lambda bool "glewIsSupported" c-string) str))))

(let ((pointer->string (foreign-lambda* c-string ((c-pointer p))
                         "C_return((char*) p);"))
      (%get-string get-string)
      (%get-stringi get-stringi))
  (set! get-string
    (lambda (n) (pointer->string (%get-string n))))
  (set! get-stringi
    (lambda (n i) (pointer->string (%get-stringi n i)))))

(define init
  (cond-expand
   (gles (lambda () #f))
   (else (foreign-lambda* void ()
                     "glewExperimental = GL_TRUE;
  GLenum err = glewInit();
  if (GLEW_OK != err){
    fprintf(stderr, \"Error: %s\\n\", glewGetErrorString(err));
    exit(1);
  }"))))

(register-feature! #:opengl-glew)

) ; module end
