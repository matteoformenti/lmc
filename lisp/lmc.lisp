(defun lmc-load (filename) 
  (let* ((sanitized (read-file filename))
        (lbls (search-labels sanitized))
        (mem (build-memory sanitized lbls)))
    (if (> (length mem) 100) 
      (error "Too many intructions")
      (append mem (make-list (- 100 (length mem)) :initial-element '0)))))
; Read and sanitize input file
(defun read-file (filename)
  (with-open-file (file filename :direction :input)
    (labels ((read-helper ()
	      (let ((line (read-line file nil nil)))
		      (when line (let ((parsed (str-split 
                (remove-comments (string-downcase line)))))
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
        ((equal instr "dat") 000)
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

; Compile and run
(defun lmc-run (filename input)
    (execution-loop (list 'state :acc 0 :pc 0 :mem (lmc-load filename)
      :in input :out '() :flag 'noflag)))
; Execution loop
(defun execution-loop (state)
  (if (equal (first state) 'state)
    (execution-loop (one-instruction state)) (nth 10 state)))
; Run a single instruction
(defun one-instruction (state)
  (labels ((get-e (el) (nth (+ 1 (position el state)) state))
    (set-e (el val) (setf (nth (+ 1 (position el state)) state) val))
    (pc () (get-e :pc))
    (get-c (index) (nth index (get-e :mem)))
    (set-c (index val) (setf (nth index (get-e :mem)) val))
    (increment-pc () (set-e :pc (+ 1 (get-e :pc))))
    (eval-flag () 
      (cond ((> (get-e :acc) 999) (progn 
          (set-e :acc (- (get-e :acc) 1000))
          (set-e :flag 'flag)
        ))
        ((< (get-e :acc) 0) (progn 
          (set-e :acc (+ (get-e :acc) 1000))
          (set-e :flag 'flag)
        )))) )
  (progn
    (let* ((opcode (if (= 0 (get-c (pc))) 0 (floor (/ (get-c (pc)) 100))))
          (arg (- (get-c (pc)) (* 100 opcode))))
    (cond 
      ((= opcode 0) (progn (setf (nth 0 state) 'haltedstate) state))
      ; Add to acc
      ((= opcode 1) (progn
        (set-e :acc (+ (get-e :acc) (get-c arg)))
        (eval-flag)
        (increment-pc)
        state))
      ; Subtract from acc
      ((= opcode 2) (progn
        (set-e :acc (- (get-e :acc) (get-c arg)))
        (eval-flag)
        (increment-pc)
        state))
      ; Store in memory
      ((= opcode 3) (progn
        (set-c arg (get-e :acc))
        (increment-pc)
        state))
      ; Load from memory
      ((= opcode 5) (progn
        (set-e :acc (get-c arg))
        (increment-pc)
        state))
      ; Branch
      ((= opcode 6) (progn (set-e :pc arg) state))
      ; Branch if zero
      ((= opcode 7) (progn (if (and (= 0 (get-e :acc)) 
          (equal 'noflag (get-e :flag)))
            (set-e :pc arg) 
            (increment-pc))
        state))
      ; Branch if positive
      ((= opcode 8) (progn (if (equal 'noflag (get-e :flag))
          (set-e :pc arg) 
          (increment-pc))
        state))
      ; Move first input to acc
      ((= (get-c (pc)) 901) (if (null (get-e :in))
        (error "Empty input list")
        (progn
          (set-e :acc (car (get-e :in)))
          (set-e :in (cdr (get-e :in)))
          (increment-pc)
          state)))
      ; Copy acc to output
      ((= (get-c (pc)) 902) (progn
        (set-e :out (append (get-e :out) (list (get-e :acc))))
        (increment-pc)
        state))
      (t (error "Invalid instruction ~W~%" (get-c (pc))))
    )))))