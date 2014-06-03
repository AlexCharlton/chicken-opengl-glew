(module gl-utils
  (load-ply
   make-vao
   load-ply-vao
   type->gl-type
   u8vector
   s8vector
   u16vector
   s16vector
   u32vector
   s32vector
   f32vector
   f64vector
   list->u8vector
   list->s8vector
   list->u16vector
   list->s16vector
   list->u32vector
   list->s32vector
   list->f32vector
   list->f64vector
   make-u8vector
   make-s8vector
   make-u16vector
   make-s16vector
   make-u32vector
   make-s32vector
   make-f32vector
   make-f64vector)

(import chicken scheme foreign data-structures files ports extras srfi-1 srfi-13 lolevel)
(use (prefix opengl-glew gl:) z3 matchable srfi-42 miscmacros (prefix srfi-4 v:))

(reexport (except srfi-4
                  u8vector
                  s8vector
                  u16vector
                  s16vector
                  u32vector
                  s32vector
                  f32vector
                  f64vector
                  list->u8vector
                  list->s8vector
                  list->u16vector
                  list->s16vector
                  list->u32vector
                  list->s32vector
                  list->f32vector
                  list->f64vector
                  make-u8vector
                  make-s8vector
                  make-u16vector
                  make-s16vector
                  make-u32vector
                  make-s32vector
                  make-f32vector
                  make-f64vector))

;;; Vector initialization
;; Stolen from srfi-4.scm
(define-syntax list->NNNvector 
  (ir-macro-transformer 
   (lambda (x i c)
     (let* ((tag (##sys#strip-syntax (cadr x)))
	    (tagstr (symbol->string tag))
	    (name (string->symbol (string-append "list->" tagstr)))
	    (make (string->symbol (string-append "v:make-" tagstr)))
	    (set (string->symbol (string-append "v:" tagstr "-set!"))))
       `(define ,name
          (lambda (lst)
            (##sys#check-list lst ',tag)
            (let* ((n (##core#inline "C_i_length" lst))
                   (v (,make n 0 #t)) )
              (do ((p lst (##core#inline "C_slot" p 1))
                   (i 0 (##core#inline "C_fixnum_plus" i 1)))
                  ((##core#inline "C_eqp" p '()) v)
                (if (and (##core#inline "C_blockp" p) (##core#inline "C_pairp" p))
                    (,set v i (##core#inline "C_slot" p 0))
                    (##sys#error-not-a-proper-list lst))))))))))

(list->NNNvector u8vector)
(list->NNNvector s8vector)
(list->NNNvector u16vector)
(list->NNNvector s16vector)
(list->NNNvector u32vector)
(list->NNNvector s32vector)
(list->NNNvector f32vector)
(list->NNNvector f64vector)

(define u8vector
  (lambda xs (list->u8vector xs)))

(define s8vector
  (lambda xs (list->s8vector xs)))

(define u16vector
  (lambda xs (list->u16vector xs)))

(define s16vector
  (lambda xs (list->s16vector xs)))

(define u32vector
  (lambda xs (list->u32vector xs)))

(define s32vector
  (lambda xs (list->s32vector xs)))

(define f32vector
  (lambda xs (list->f32vector xs)))

(define f64vector
  (lambda xs (list->f64vector xs)))

(define-syntax make-NNNvector 
  (ir-macro-transformer 
   (lambda (x i c)
     (let* ((tag (##sys#strip-syntax (cadr x)))
	    (tagstr (symbol->string tag))
	    (name (string->symbol (string-append "make-" tagstr)))
	    (make (string->symbol (string-append "v:make-" tagstr))))
       `(define (,name n #!optional (value 0) (gc #t) (finalize #t))
          (,make n value gc finalize))))))

(make-NNNvector u8vector)
(make-NNNvector s8vector)
(make-NNNvector u16vector)
(make-NNNvector s16vector)
(make-NNNvector u32vector)
(make-NNNvector s32vector)
(make-NNNvector f32vector)
(make-NNNvector f64vector)

;;;; Blob utils
;; Setting
(define blob-set-u8!
  (foreign-lambda* void
         ((nonnull-scheme-pointer bv) (unsigned-integer32 x) (int off))
    "((uint8_t *)bv)[off] = (uint8_t)(x & 0xff);"))

(define blob-set-s8!
  (foreign-lambda* void
         ((nonnull-scheme-pointer bv) (integer32 x) (int off))
    "((char *)bv)[off] = (char)(x & 0xff);"))

(define blob-set-u16!
  (foreign-lambda* void
         ((nonnull-scheme-pointer bv) (unsigned-integer32 x) (int off))
    "*(uint16_t *)(&((char *)bv)[off]) = (uint16_t)(x & 0xffff);"))

(define blob-set-s16!
  (foreign-lambda* void
        ((nonnull-scheme-pointer bv) (integer32 x) (int off))
    "*(int16_t *)(&((char *)bv)[off]) = (int16_t)(x & 0xffff);"))

(define blob-set-u32!
  (foreign-lambda* void
         ((nonnull-scheme-pointer bv) (unsigned-integer32 x) (int off))
    "*(uint32_t *)(&((char *)bv)[off]) = x;"))

(define blob-set-s32!
  (foreign-lambda* void
        ((nonnull-scheme-pointer bv) (integer32 x) (int off))
    "*(int32_t *)(&((char *)bv)[off]) = x;"))

(define blob-set-s64!
  (foreign-lambda* void
         ((nonnull-scheme-pointer bv) (unsigned-integer64 x) (int off))
    "*(uint64_t *)(&((char *)bv)[off]) = x;"))

(define blob-set-s64!
  (foreign-lambda* void
        ((nonnull-scheme-pointer bv) (integer64 x) (int off))
    "*(int64_t *)(&((char *)bv)[off]) = x;"))

(define blob-set-f32!
  (foreign-lambda* void
        ((nonnull-scheme-pointer bv) (float x) (int off))
    "*(float *)(&((char *)bv)[off]) = x;"))

(define blob-set-f64!
  (foreign-lambda* void
         ((nonnull-scheme-pointer bv) (double x) (int off))
    "*(double *)(&((char *)bv)[off]) = x;"))

;; Referencing
(define blob-ref-u8
  (foreign-lambda* unsigned-byte
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(((uint8_t *)bv)[off]);"))

(define blob-ref-s8
  (foreign-lambda* byte
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(((char *)bv)[off]);"))

(define blob-ref-u16
  (foreign-lambda* unsigned-short
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(uint16_t *)(&(((char *)bv)[off])));"))

(define blob-ref-s16
  (foreign-lambda* short
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(int16_t *)(&(((char *)bv)[off])));"))

(define blob-ref-u32
  (foreign-lambda* unsigned-int
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(uint32_t *)(&(((char *)bv)[off])));"))

(define blob-ref-s32
  (foreign-lambda* int
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(int32_t *)(&(((char *)bv)[off])));"))

(define blob-ref-u64
  (foreign-lambda* unsigned-integer64
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(uint64_t *)(&(((char *)bv)[off])));"))

(define blob-ref-s64
  (foreign-lambda* integer64
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(int64_t *)(&(((char *)bv)[off])));"))

(define blob-ref-f32
  (foreign-lambda* float
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(float *)(&(((char *)bv)[off])));"))

(define blob-ref-f64
  (foreign-lambda* double
         ((nonnull-scheme-pointer bv) (int off))
    "C_return(*(double *)(&(((char *)bv)[off])));"))

;;; Ply
(define data-format (make-parameter #f))

(define (type->bytes type)
  (case type
    [(char: int8: byte: uchar: uint8: unsigned-byte:) 1]
    [(short: int16: ushort: uint16: unsigned-short:) 2]
    [(int: int32: integer: integer32: uint: uint32: unsigned-int: unsigned-int32:
	   unsigned-integer: unsigned-integer32: float: float32:)
     4]
    [(double: float64:) 8]))

(define (type->gl-type type)
  (case type
    [(char: int8: byte:) gl:+byte+]
    [(uchar: uint8: unsigned-byte:) gl:+unsigned-byte+]
    [(short: int16:) gl:+short+]
    [(ushort: uint16: unsigned-short:) gl:+unsigned-short+]
    [(int: int32: integer: integer32:) gl:+int+]
    [(uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     gl:+unsigned-int+]
    [(float: float32:) gl:+float+]
    [(double: float64:) gl:+double+]))

(define (type->unsigned-type type)
  (case type
    [(char: int8: byte: uchar: uint8: unsigned-byte:) gl:+unsigned-byte+]
    [(short: int16: ushort: uint16: unsigned-short:) gl:+unsigned-short+]
    [(int: int32: integer: integer32: uint: uint32: unsigned-int: unsigned-int32:
           unsigned-integer: unsigned-integer32:)
     gl:+unsigned-int+]))

(define (type->setter type)
  (case type
    [(char: int8: byte:) blob-set-u8!]
    [(uchar: uint8: unsigned-byte:) blob-set-s8!]
    [(short: int16:) blob-set-s16!]
    [(ushort: uint16: unsigned-short:) blob-set-u16!]
    [(int: int32: integer: integer32:) blob-set-s32!]
    [(uint: uint32: unsigned-int: unsigned-int32:
	    unsigned-integer: unsigned-integer32:)
     blob-set-u32!]
    [(float: float32:) blob-set-f32!]
    [(double: float64:) blob-set-f64!]))

(define (bytes-per-element els)
  (foldl (lambda (count el)
           (+ count (type->bytes (second el))))
         0 els))

;; For ((name type) ...), return ((name b0 b1 b2) ... )
(define (get-bytes els)
  (let loop ([els els] [result '()] [n 0])
    (if (null? els)
        result
        (let ([bytes (type->bytes (cadar els))])
          (loop (cdr els)
                (cons (cons (caar els)
                            (list-ec (: i bytes) (+ n i)))
                      result)
                (+ n bytes))))))

(define (endian-swap?)
  (or (and (member little-endian: (features))
        (equal? (data-format) binary-big-endian:))
     (and (member big-endian: (features))
        (equal? (data-format) binary-little-endian:))))

(define (binary el spec)
  (match-let ([(name n . vars) el])
    (if* (assoc name spec)
         (let* ([var-bytes (get-bytes vars)]
                [mapping (concatenate
                          (list-ec (: el (second it))
                                   ((if (endian-swap?)
                                        reverse
                                        identity)
                                    (cdr (assoc el var-bytes)))))]
                [stride (length mapping)]
                [buffer (make-u8vector (* n stride))]
                [blob (u8vector->blob/shared buffer)])
           (dotimes (vertex n)
             (let ([bytes (list-ec (: _ (bytes-per-element vars))
                                   (read-byte))])
               (for-each (lambda (m i)
                           (blob-set-u8! blob (list-ref bytes m)
                                         (+ (* vertex stride) i)))
                         mapping
                         (iota stride))))
           buffer)
         (dotimes (_ (* n (bytes-per-element vars)))
           (read-byte)))))

(define (ascii el spec)
  (match-let ([(name n . vars) el])
    (if* (assoc name spec)
         (let* ([var/type/bytes/i (map (lambda (v i)
                                         (list (car v)
                                               (second v)
                                               (type->bytes (second v))
                                               i))
                                       vars
                                       (iota (length vars)))]
                [elements (map (lambda (el) (assoc el var/type/bytes/i))
                               (second it))]
                [mapping (map fourth elements)]
                [offsets (reverse
                          (let loop ([lst (map third elements)]
                                     [accum '()]
                                     [count 0])
                            (if (null? lst)
                                accum
                                (loop (cdr lst)
                                      (cons count accum)
                                      (+ count (car lst))))))]
                [setters (map (lambda (el) (type->setter (second el)))
                              elements)]
                [stride (fold + 0 (map third elements))]
                [buffer (make-u8vector (* n stride))]
                [blob (u8vector->blob/shared buffer)])
           (dotimes (vertex n)
             (let ([values (list-ec (: _ (length vars)) (read))])
               (for-each (lambda (mapping offset setter)
                           (setter blob (list-ref values mapping)
                                   (+ (* vertex stride) offset)))
                         mapping
                         offsets
                         setters)))
           buffer)
         (dotimes (_ (* n (length vars)))
           (read)))))

(define n-face-vertices (make-parameter 0))

(define (binary-list el spec)
  (match-let ([(name n (var ('list: list-type type))) el])
    (when (> (type->bytes list-type) 1)
      (error 'load-ply "Face list type must be one byte" list-type))
    (if* (assoc name spec)
         (let* ([blob #f]
                [buffer #f]
                [n-bytes (type->bytes type)]
                [mapping ((if (endian-swap?)
                              reverse
                              identity)
                          (iota n-bytes))])
           (dotimes (face n)
             (let [(n-verts (read-byte))]
               (if (= (n-face-vertices) 0)
                   (begin
                     (n-face-vertices n-verts)
                     (set! buffer (make-u8vector (* n n-verts n-bytes)))
                     (set! blob (u8vector->blob/shared buffer)))
                   (when (not (= (n-face-vertices) n-verts))
                     (error 'load-ply "Number of elements must be constant in face list")))
               (dotimes (vertex n-verts)
                 (let ([bytes (list-ec (: b n-bytes)
                                       (read-byte))])
                   (for-each
                    (lambda (byte i)
                      (blob-set-u8! blob byte
                                    (+ (* vertex n-bytes)
                                       (* face n-verts n-bytes)
                                       i)))
                    bytes
                    mapping)))))
           buffer)
         (dotimes (_ n)
           (let [(n-verts (read-byte))]
             (if (= (n-face-vertices) 0)
                 (n-face-vertices n-verts)
                 (when (not (= (n-face-vertices) n-verts))
                   (error 'load-ply "Number of elements must be constant in face list")))
             (dotimes (_ (* n-verts (type->bytes type)))
               (read-byte)))))))

(define (ascii-list el spec)
  (match-let ([(name n (var ('list: list-type type))) el])
    (if* (assoc name spec)
         (let* ([blob #f]
                [buffer #f]
                [n-bytes (type->bytes type)]
                [setter (type->setter type)])
           (dotimes (face n)
             (let ([n-verts (read)])
               (if (= (n-face-vertices) 0)
                   (begin
                     (n-face-vertices n-verts)
                     (set! buffer (make-u8vector (* n n-verts n-bytes)))
                     (set! blob (u8vector->blob/shared buffer)))
                   (when (not (= (n-face-vertices) n-verts))
                     (error 'load-ply "Number of elements must be constant in face list")))
               (dotimes (vertex n-verts)
                 (setter blob (read) (+ (* vertex n-bytes)
                                        (* face n-verts n-bytes))))))
           buffer)
         (dotimes (_ n)
           (let [(n-verts (read))]
             (if (= (n-face-vertices) 0)
                 (n-face-vertices n-verts)
                 (when (not (= (n-face-vertices) n-verts))
                   (error 'load-ply "Number of elements must be constant in face list")))
             (dotimes (_ n-verts) (read)))))))

(define (get-buffers elements spec)
  (let ([buffers (list-ec (: el elements)
                   (cons (first el)
                         (if (list? (car (cdaddr el)))
                             (if (equal? (data-format) ascii:)
                                 (ascii-list el spec)
                                 (binary-list el spec))
                             (if (equal? (data-format) ascii:)
                                 (ascii el spec)
                                 (binary el spec)))))])
    ; Return buffers in order of spec
    (list-ec (: s spec)
             (cdr (assoc (car s) buffers)))))

(define (check-spec spec elements)
  (define (err el) (error 'load-ply "buffer-spec does not match ply" el))
  (for-each (lambda (s)
              (if* (assoc (first s) elements)
                   (for-each (lambda (var)
                               (unless (assoc var (cddr it))
                                 (err var)))
                             (if (list? (second s))
                                 (second s)
                                 (list (second s))))
                   (err (first s))))
            spec))

(define (load-ply file buffer-spec)
  (define (err) (error 'load-ply "Not a valid PLY file:" file))
  (define (parse-elements)
    (reverse
     (map reverse
          (let loop ([elements '()])
            (match (string-tokenize (read-line))
              [("comment" . rest) (loop elements)]
              [("element" name n)
               (loop (cons (list (string->number n) (string->keyword name))
                           elements))]
              [("property" "list" list-type type name)
               (loop (cons (cons (list (string->symbol name)
                                       (list list:
                                             (string->keyword list-type)
                                             (string->keyword type)))
                                 (car elements))
                           (cdr elements)))]
              [("property" type name)
               (loop (cons (cons (list (string->symbol name) (string->keyword type))
                                 (car elements))
                           (cdr elements)))]
              [("end_header") elements]
              [other (err)])))))
  (with-input-from-port (if (equal? (pathname-extension file) "gz")
                            (z3:open-compressed-input-file file)
                            (open-input-file file))
    (lambda ()
      (match (read-line)
        ["ply" #t]
        [else (err)])
       (match (string-tokenize (read-line))
         [("format" "ascii" version) (data-format #:ascii)]
         [("format" "binary_little_endian" version)
          (data-format #:binary-little-endian)]
         [("format" "binary_big_endian" version)
          (data-format #:binary-big-endian)]
         [else (err)])
       (let ([elements (parse-elements)])
         (check-spec buffer-spec elements)
         (values (get-buffers elements buffer-spec)
                 elements)))))

;;; VAO creation
(define-syntax XXX->pointer
  (ir-macro-transformer
   (lambda (e r c)
     (let* ([type (strip-syntax (cadr e))]
            [name (string->symbol (string-append (symbol->string type) "->pointer"))])
       `(define ,name
          (foreign-lambda* c-pointer ((,type v))
            "C_return(v);"))))))

(XXX->pointer blob)
(XXX->pointer u8vector)
(XXX->pointer s8vector)
(XXX->pointer u16vector)
(XXX->pointer s16vector)
(XXX->pointer u32vector)
(XXX->pointer s32vector)
(XXX->pointer f32vector)
(XXX->pointer f64vector)

(define (->pointer v)
  (cond
   [(blob? v) (blob->pointer v)]
   [(u8vector? v) (u8vector->pointer v)]
   [(s8vector? v) (s8vector->pointer v)]
   [(u16vector? v) (u16vector->pointer v)]
   [(s16vector? v) (s16vector->pointer v)]
   [(u32vector? v) (u32vector->pointer v)]
   [(s32vector? v) (s32vector->pointer v)]
   [(f32vector? v) (f32vector->pointer v)]
   [(f64vector? v) (f64vector->pointer v)]
   [else (error 'make-vao "Not a blob or vector" v)]))

(define (size v)
  (cond
   [(blob? v) (blob-size v)]
   [(u8vector? v) (u8vector-length v)]
   [(s8vector? v) (s8vector-length v)]
   [(u16vector? v) (* (u16vector-length v) 2)]
   [(s16vector? v) (* (s16vector-length v) 2)]
   [(u32vector? v) (* (u32vector-length v) 4)]
   [(s32vector? v) (* (s32vector-length v) 4)]
   [(f32vector? v) (* (f32vector-length v) 4)]
   [(f64vector? v) (* (f64vector-length v) 8)]
   [else (error 'make-vao "Not a blob or vector" v)]))

(define (make-vao vertex-data index-data attributes
		  #!optional [usage gl:+static-draw+])
  (define offset 0)
  (define stride (fold (lambda (attr str)
			 (match attr
			   [(_ type n . _) (+ (* n (type->bytes type)) str)]
			   [attr (error 'make-vao
					"Expected (location type n #:key [normalize?]), got"
					attr)]))
		       0 attributes))
  (define (vertex-attrib location type n #!optional [normalize? #f])
    (gl:vertex-attrib-pointer location n (type->gl-type type)
			      normalize?
			      stride (address->pointer offset))
    (inc! offset (* n (type->bytes type)))
    (gl:enable-vertex-attrib-array location))
  (let ([vao (gl:gen-vertex-array)]
	[vert-buffer (gl:gen-buffer)]
	[index-buffer (gl:gen-buffer)])
    (gl:bind-vertex-array vao)
    (gl:bind-buffer gl:+array-buffer+ vert-buffer)
    (gl:buffer-data gl:+array-buffer+ (size vertex-data)
		    (->pointer vertex-data) usage)
    (for-each (match-lambda
	       [(location type n) (vertex-attrib location type n)]
	       [(location type n 'normalize?: normalize)
                (vertex-attrib location type n normalize)]
	       [attr (error 'make-vao "Expected (location type n #!key [normalize?]), got"
			    attr)])
	      attributes)
    (gl:bind-buffer gl:+element-array-buffer+ index-buffer)
    (gl:buffer-data gl:+element-array-buffer+ (size index-data)
		    (->pointer index-data) usage)
    (gl:bind-vertex-array 0)
    (gl:delete-buffer vert-buffer)
    (gl:delete-buffer index-buffer)
    vao))

(define (verts->primitive verts)
  (case verts
    [(1) gl:+points+]
    [(2) gl:+lines+]
    [(3) gl:+triangles+]
    [(4) gl:+quads+]
    [else (error 'load-ply-vao "Unsupported number of vertices" verts)]))

(define (load-ply-vao ply #!key vertex face)
  (define (attribute-vars attribute elements)
    (let* ([vertices (cddr (assoc vertex: elements))]
           [type (let loop ([els (cddr attribute)]
                            [type (second (assoc (cadr attribute) vertices))])
                   (if (null? els)
                       type
                       (if (equal? type (second (assoc (car els) vertices)))
                           (loop (cdr els) type)
                           (error 'load-ply-vao "Properties of the same attribute must have the same type"
                                  attribute))))])
      (list (car attribute)
            type
            (length (cdr attribute)))))
  (let ([buffer-spec `((vertex: ,(flatten (map cdr vertex)))
                       (face: ,face))])
    (let-values ([(buffers elements) (load-ply ply buffer-spec)])
      (unless (and (assoc face: elements)
                 (assoc vertex: elements))
        (error 'load-ply-vao "Ply must contain vertex and face elements" ply))
      (let* ([face (cdr (assoc face: elements))]
             [n-verts (* (car face) (n-face-vertices))]
             [primitive-type (verts->primitive (n-face-vertices))]
             [element-type (type->unsigned-type (third (cadadr face)))]
             [attributes (map (lambda (v) (attribute-vars v elements))
                              vertex)])
        (values (make-vao (car buffers) (cadr buffers) attributes)
                (car buffers)
                (cadr buffers)
                n-verts
                primitive-type
                element-type)))))

); end module
