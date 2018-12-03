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

(defstruct nodo
  contenuto
  sinistra
  destra)

(defun inserisci-bst (nodo val)
  (cond ((null nodo) (make-nodo :contenuto val))
        ((< (nodo-contenuto nodo) val)
          (make-nodo  :contenuto  (nodo-contenuto nodo)
                      :sinistra   (nodo-sinistra nodo)
                      :destra     (inserisci-bst (nodo-destra nodo) val)))
        ((= (nodo-contenuto nodo) val) nodo)
        ((> (nodo-contenuto nodo) val)
          (make-nodo  :contenuto  (nodo-contenuto nodo)
                      :sinistra   (inserisci-bst (nodo-sinistra nodo) val)
                      :destra     (nodo-destra nodo)))
    ))

(defun cerca-bst (nodo val)
  (cond ((null nodo) nil)
        ((= (nodo-contenuto nodo) val) T)
        ((> (nodo-contenuto nodo) val) (cerca-bst (nodo-sinistra nodo) val))
        ((< (nodo-contenuto nodo) val) (cerca-bst (nodo-destra nodo) val))
    ))

(defun inverti (lst)
  (if (atom lst) lst (cons (inverti (cdr lst)) (car lst))))

(defun circulte (lst mode)
  (cond ((null lst) nil)
        ((atom lst) lst)
        ((eq mode 'left) (append (cdr lst) (list (car lst))))
        ((eq mode 'right) (cons (last_in_list lst) (remove_last lst)))))


(defun last_in_list (lst)
  (if (null (cdr lst)) (car lst) (last_in_list (cdr lst))))

(defun remove_last (lst)
  (cond ((null lst) nil)
        ((atom (cdr lst)) nil)
        (T (cons (car lst) (remove_last (cdr lst))))))
