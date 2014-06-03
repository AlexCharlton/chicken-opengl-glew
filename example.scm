(import chicken scheme)
(use (prefix glfw3 glfw:) (prefix opengl-glew gl:) gl-math gl-utils)

(define *vertex* 
#<<END
#version 330
in vec2 vertex;
in vec3 color;
out vec3 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(vertex, 0.0, 1.0);
   c = color;
}
END
)

(define *fragment*
#<<END
#version 330
in vec3 c;
out vec4 fragColor;
void main(){
  fragColor = vec4(c, 1.0);
}
END
)

(define vertex-data (f32vector -1 -1 1 0 0
                               1 -1 0 1 0
                               1 1 0 0 1
                               -1 1 1 0 1))

(define index-data (u16vector 0 1 2
                              0 2 3))

(define vao (make-parameter #f))

(define program (make-parameter #f))

(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define view-matrix
  (look-at 1 0 3
           0 0 0
           0 1 0))

(define model-matrix (mat4-identity
                      #t ; Matrix should be in a non-GC'd area
                      ))

(define (render)
  (gl:use-program (program))
  (gl:uniform-matrix4fv (gl:get-uniform-location (program) "MVP")
                        1 #f
                        (m* projection-matrix
                            (m* view-matrix model-matrix)))
  (gl:bind-vertex-array (vao))
  (gl:draw-elements-base-vertex gl:+triangles+ 6 (type->gl-type ushort:) #f 0)

  (gl:check-error)
  (gl:bind-vertex-array 0))

(glfw:with-window (640 480 "Example" resizable: #f
                       context-version-major: 3
                       context-version-minor: 3)
  (gl:init)

  (print (gl:supported? "GL_ARB_framebuffer_object"))

  (set! *vertex* (gl:make-shader gl:+vertex-shader+ *vertex*))
  (set! *fragment* (gl:make-shader gl:+fragment-shader+ *fragment*))

  (program (gl:make-program (list *vertex* *fragment*)))

  (vao (make-vao vertex-data index-data
                 `((,(gl:get-attrib-location (program) "vertex") float: 2)
                   (,(gl:get-attrib-location (program) "color") float: 3))))
  (let loop ()
     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (render)
     (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum
     (unless (glfw:window-should-close (glfw:window))
       (loop))))
