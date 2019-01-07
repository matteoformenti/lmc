;; FIND OUT HOW MANY CONSECUITVE NUMBERS ARE PRESENT IN A LIST AND COMPRESS THEM IN A STRUCT

(defstruct run value length)

;;  Get a list of consecutive occurencies of the first element of the list
(defun find-run (to-process processed)
    (cond   ((null to-process) 
                (values processed nil))
            ((null processed)
                (find-run (cdr to-process) (list (car to-process))))
            ((eq (car to-process) (car processed))
                (find-run (cdr to-process) (cons (car to-process) processed)))
            (t (values processed to-process)) ))

;;  Construct a list of run struct
(defun rle-encode (lst) 
    (when lst
        (multiple-value-bind (run what-remains) (find-run lst nil)
            (cons   (make-run :value (car run) :length (length run))
                    (rle-encode what-remains)))))

;; Construct a list based on a list of run structs
(defun rle-decode (lst)
    (when lst
        (cons (decompress-struct(car lst)) (rle-decode(cdr lst)))))

;;  Build a list of length str-length of str-value characters
(defun decompress-struct (str) 
    (cond   ((= 0 (run-length str)) nil)
            (t (cons (run-value str) (decompress-struct (make-run :length (- (run-length str) 1) :value (run-value str)))))
))

(defmacro my-if (a b c) 
    `(cond (,a ,b) (t ,c)))