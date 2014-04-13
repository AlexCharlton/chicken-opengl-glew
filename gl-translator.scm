(import chicken scheme irregex)

(define *replacements*
  '(((: "/*" (*? any) "*/") . "")
    ("GLAPI " . "")
    ("#if.*\n" . "")
    ("#ifndef.*\n" . "")
    ("#elif.*\n" . "")
    ("#else\n" . "")
    ("#endif.*\n" . "")
    ("extern.*\n" . "")
    ("ptrdiff_t" . "signed int")
    ("APIENTRY" . "")
    ("\n}\n" . "")
    ("#include.*\n" . "")
    ("#define GL_VERSION.*\n" . "")
    ("#define [^G].*\n" . "")
    ((: "*const*") . "**")
    ((: newline (*? (or alpha numeric ("()*") space)) "PFNGL" (*? any) eol) . "")
    ("#endif .*\n" . "")
    ("typedef .*_t;\n" . "")
    ("typedef uint64_t GLuint64;\ntypedef int64_t GLint64;" .
     "typedef unsigned long int GLuint64;\ntypedef long int GLint64;")
    ("typedef unsigned char GLboolean;" . "typedef bool GLboolean;" )
    ("\n\n" . "\n")))

(define (gl-translate file)
  (call-with-output-file "gl.h"
    (lambda (output)
      (call-with-input-file file
        (lambda (input)
          (let ([h (read-string #f input)])
            (let loop ([str h] [replacements *replacements*])
              (if (null? replacements)
                  (write-string str #f output)
                  (loop (irregex-replace/all (caar replacements) str
                                             (cdar replacements))
                        (cdr replacements))))))))))
