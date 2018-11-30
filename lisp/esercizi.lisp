(defun fact (n)
  (if (= n 0) 1 (* n (fact (- n 1)))))

(defun fact-iter (n acc)
  (if (= n 0) acc (fact-iter  (- n 1) (* n acc))))

(defun listlength (lst)
  (if (null lst) 0 (+ 1 (listlength (cdr lst)))))

(defun ric-listlength (lst acc)
  (if (null lst) acc (ric-listlength (cdr lst) (+ 1 acc))))

(defun double-list (lst)
  (let ((nuova-testa (unless (null lst) (* 2 (car lst))))
        (coda (unless (null lst) (cdr lst))))
  (if (null lst) nil (cons nuova-testa (double-list coda)))))

(defun prendi-positivi (lst)
  (cond ((null lst) nil)
        ((>= (car lst) 0) (cons (car lst) (prendi-positivi (cdr lst))))
        ((< (car lst) 0)  (prendi-positivi(cdr lst)))))

(defun applica-parzialmente (func arg)
  (lambda (x) (funcall func arg x)))

(defun apply-to-list (lst func)
    (if (null lst) nil
        (cons (funcall func (car lst)) (apply-to-list (cdr lst) func))))
