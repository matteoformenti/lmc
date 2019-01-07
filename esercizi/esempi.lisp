;; Vediamo come leggere un file in Common Lisp

(defun read-all-lines ()
  (with-open-file (f "example.txt" :direction :input)
    (read-all-lines-helper f)))

(defun read-all-lines-helper (stream)
  (let ((line (read-line stream nil nil))) ;; leggi la linea ritornando nil in caso di fine file
    (when line (cons line (read-all-lines-helper stream)))))

;; Ridefiniamo usando una funzione definita localmente per la lettura delle linee

(defun read-all-lines2 ()
  (with-open-file (f "example.txt" :direction :input)
    (labels ((read-helper ()
	       (let ((line (read-line f nil nil)))
		 (when line (cons line (read-helper))))))
      (read-helper))))

;; Ricerca di un valore in una stringa (funziona anche per sottosequenze)
;; (search "test" "this is a test") => 10 (posizione del match)

;; Esercizio

(defun read-matching (match)
  (with-open-file (f "example.txt" :direction :input)
    (labels ((read-helper ()
	       (let ((line (read-line f nil nil)))
		 (when line
		   (if (search match line)
		       (cons line (read-helper))
		       (read-helper))))))
      (read-helper))))

;; Parsing di un valore salvato in una stringa

(defun convert-string (str)
  (when (string/=  str "")
    (multiple-value-bind (value num-chars) (read-from-string str nil)
      (when value
	(cons value (convert-string (subseq str num-chars)))))))

;; Format to write to files
;; ~A (auto)
;; ~D (decimal number)
;; ~F (floating point)
;; ~R (number as words)
;; ~% (newline)

(defun write-one-per-line (lst)
  (when lst
    (format t "~R~%" (car lst))
    (write-one-per-line (cdr lst))))
  
;; vediamo ora come scrivere su file

(defun write-one-per-line-on-file (lst filename)
  (with-open-file (f filename :direction :output)
    (labels ((helper (lst)
	       (when lst
		 (format f "~R~%" (car lst))
		 (helper (cdr lst)))))
      (helper lst))))
		     
;; Vediamo ora come implementare una calcolatrice RPN

(defun rpn-calc (stack)
  (format t "stack:~A~%" stack)
  (format t "> ")
  (let ((op (read)))
    (cond ((numberp op) (rpn-calc (cons op stack)))
	  ((eq op '+) (rpn-calc (cons
				 (+ (car stack) (cadr stack))
				 (cddr stack))))
	  ((eq op '-) (rpn-calc (cons
				 (- (car stack) (cadr stack))
				 (cddr stack))))
	  ((eq op '*) (rpn-calc (cons
				 (* (car stack) (cadr stack))
				 (cddr stack))))
	  ((eq op '/) (rpn-calc (cons
				 (/ (car stack) (cadr stack))
				 (cddr stack))))
	  ((eq op 'quit) (format t "Quit~%"))
	  (t nil))))

;; Proviamo ad aggiungere la stampa degli argomenti ad una funzione

(defun add-logging (func)
  (lambda (&rest args)
    (print args)
    (apply func args)))

;; Esempio di applicazione

(defun double-values (list)
  (mapcar (add-logging (lambda (x) (* x 2))) list))

;; Rendiamo più leggibile con gli argomenti opzionali

(defun add-logging2 (func &optional text)
  (lambda (&rest args)
    (if (null text)
	(format t "~A~%" args)
	(format t "~A: ~A~%" text args))
    (apply func args)))

(defun double-values2 (list)
  (mapcar (add-logging2 (lambda (x) (* x 2)) "Chiamo (lambda (x) (* x 2)) con argomenti") list))

;; Esercizio: definire "add-logging3" in cui è possibile vedere anche
;; il valore di ritorno della funzione chiamata e che possa stampare anche un testo
;; a scelta prima di stampare l'output

(defun add-logging3 (func &optional text-before text-after)
  (lambda (&rest args)
    (if (null text-before)
	(format t "~A~%" args)
	(format t "~A: ~A~%" text-before args))
    (let ((result (apply func args)))
      (if (null text-after)
	  (format t "~A~%" result)
	  (format t "~A: ~A~%" text-after result))
      result)))

(defun double-values3 (list)
  (mapcar (add-logging3 (lambda (x) (* x 2))
			"Chiamo (lambda (x) (* x 2)) con argomenti"
			"Ottenendo come risultato")
	  list))