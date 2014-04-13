# opengl-glew Chicken Scheme bindings

Bindings to OpenGL with GLEW extension loading.

Chicken's other [opengl](http://wiki.call-cc.org/eggref/4/opengl) bindings are based on old fixed function pipeline OpenGL. These bindings are designed to be always up-to-date with the most recent [core header file](http://www.opengl.org/registry/api/GL/glcorearb.h). Additionally, bindings to [GLEW](http://glew.sourceforge.net/) are provided for extension management.

## Requirements
- Make
- Bind

## Documentation
All functions and constants from the OpenGL [core header file](http://www.opengl.org/registry/api/GL/glcorearb.h) are exported. Scheme style names are provided (underscores and camelCase replaced with hyphens), the `gl` prefix is removed from names, `is` functions are given question marks, and constants are bookended by `+`s (e.g. `tex-image2d`, `enabled?`, `+arb-viewport-array+`).

Additionally, two GLEW functions are exposed:

- `(init)`: Required to initialize GLEW/OpenGL. An OpenGL context must be created before this is called.
- `(is-supported? string) -> boolean`: Query whether the given OpenGL extension is supported.

Finally, the following helper functions are provided:

-`(make-shader shader-type shader-source) -> int`: Creates and compiles a shader object given the shader's type and source string.
- `(make-program shader-list) -> int`: Creates and links a program object, given a list of shader objects.


## Example
This example depends on the [glfw3](http://wiki.call-cc.org/eggref/4/glfw3) egg for window and context creation.

``` Scheme
(import chicken scheme)
(use (prefix glfw3 glfw:) (prefix opengl-glew gl:))

(glfw:init)
(define *window* (glfw:create-window 640 480 "Example" #f #f))
(glfw:make-context-current *window*)
(gl:init)

(print (gl:is-supported "GL_ARB_framebuffer_object"))

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
```

## Author
Alex Charlton

## Licence
Copyright (c) 2014, Alexander Charlton
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
