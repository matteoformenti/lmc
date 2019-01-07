(defun lmc-load (filename) 
  (let ((sanitized (read-file filename)))
    (let ((lbls (search-labels sanitized)))
    (build-memory sanitized lbls))))
; Read and sanitize input file
(defun read-file (filename)
  (with-open-file (file filename :direction :input)
    (labels ((read-helper ()
	      (let ((line (read-line file nil nil)))
		      (when line (let ((parsed (parse-line line)))
              (if parsed (cons parsed (read-helper)) (read-helper)))))))  
      (read-helper))))

; Returns a list of (label_name value)
(defun search-labels (lines &optional (row 0))
  (let ((possible-label (first (car lines))))
    (if (is-label possible-label) 
      (cons (cons possible-label (cons row nil)) 
        (if (> (length lines) 1) (search-labels (cdr lines) (+ 1 row)) nil))
      (if (> (length lines) 1)
        (search-labels (cdr lines) (+ 1 row)) 
        nil))))

;
(defun parse-line (line) 
  (reformat-line line))

;
(defun reformat-line (line) 
  (str-split (remove-comments (string-downcase line))))

; Remove comments from a line
(defun remove-comments (line) 
  (let ((pos (search "//" line))) (subseq line 0 pos)))

; Splits a line at each space ignoring multiple spaces
(defun str-split (l) 
  (let ((pos (search " " l))) 
    (cond ((not pos) (if (= 0 (length l)) nil (list l)))
          ((= pos 0) (str-split (subseq l (+ 1 pos))))
          ((> pos 0) (cons (subseq l 0 pos)
                          (str-split (subseq l (+ 1 pos))))))))

; Checks if a word is a valid label (not numeric and not reserved)
(defun is-label (word) 
  (and 
   (not (ignore-errors (parse-integer word)))
    (not (position word `("add" "sub" "sta" "lda" "bra" "brz" "brp" "dat" "inp" "out" "hlt") :test #'string=))))
; Build line to machine code
(defun build-instr (instr labels &optional (param nil)) 
  (cond 
    ((null param) 
      (cond ((equal instr "hlt") 0)
        ((equal instr "inp") 901)
        ((equal instr "out") 902)
        ((equal instr "dat") 0)
        (t (error "Instruction ~A requires a parameter~%" instr))))
    (t
      ; Resolve label if present 
      (let ((param (if (is-label param) 
            (find-label param labels) (parse-integer param))))
          (cond 
            ((equal instr "add") (+ 100 param))
            ((equal instr "sub") (+ 200 param))
            ((equal instr "sta") (+ 300 param))
            ((equal instr "lda") (+ 500 param))
            ((equal instr "bra") (+ 600 param))
            ((equal instr "brz") (+ 700 param))
            ((equal instr "brp") (+ 800 param))
            ((equal instr "dat") param))))
    (t (error "Invalid line ~W~%" instr ))))
; Build memory
(defun build-memory (mem labels) 
  (if (null mem) nil 
      (let ((instr (car mem)))
      ; remove first element of the instruction list if label
      (cond 
        ; If the first element is not a label
        ((not (is-label (first instr)))
          (cond ((= (length instr) 1) 
              (cons (build-instr (first instr) labels) 
                    (build-memory (cdr mem) labels)))
            ((= (length instr) 2)
              (cons (build-instr (first instr) labels (second instr))
                    (build-memory (cdr mem) labels)))
            (t (error "Invalid instruction ~W~%" instr)))) 
        ; If the first element is a label
        ((= 2 (length instr)) 
          (cons (build-instr (second instr) labels) 
                (build-memory (cdr mem) labels)))
        ((= 3 (length instr))
          (cons (build-instr (second instr) labels (third instr))
                (build-memory (cdr mem) labels)))))))
; Return the value associated with a label
(defun find-label (name labels) 
  (if (numberp name) nil (labels ((find-label-rec (name rest)
    (cond ((null rest) (error "Label ~A isn't defined" name))
          ((string= name (first (car rest))) (second (car rest)))
          (t (find-label-rec name (cdr rest))) )))
  (find-label-rec name labels))))

(defun lmc-run (filneame input) 
  ())