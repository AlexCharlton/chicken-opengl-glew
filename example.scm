(import chicken scheme)
(use (prefix glfw3 glfw:) (prefix opengl-glew gl:))

(glfw:init)
(define *window* (glfw:create-window 640 480 "Example" #f #f))
(glfw:make-context-current *window*)
(gl:init)

(print (gl:supported? "GL_ARB_framebuffer_object"))

(define *vertex* (gl:make-shader gl:+vertex-shader+
#<<END
#version 330
in vec2 vertex;
in vec3 color;
out vec3 c;
uniform mat4 viewMatrix;

void main(){
   gl_Position = (viewMatrix * vec4(vertex, 0.0, 1.0));
   c = color;
}
END
))

(define *fragment* (gl:make-shader gl:+fragment-shader+
#<<END
#version 330
in vec3 c;
out vec4 fragColor;
void main(){
  fragColor = vec4(c, 1.0);
}
END
))

;If this is not zero, then everything is working
(print
 (gl:make-program (list *vertex* *fragment*))) 

(glfw:destroy-window *window*)
(glfw:terminate)
