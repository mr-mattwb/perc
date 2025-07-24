

(defmacro name () `'(sam judy janet))

(defmacro owner () `'(sam judy))

(defvar license '(sam judy joe jack sally))

(defun purchase (n) 
    (defmacro owner ()
        (let ((olds (owner)))
            `(cons ',n ',olds))))
