

(defmacro name () `'(sam judy janet))

(defmacro owner () `'(sam judy))

(defvar license '(sam judy joe jack sally))

(defmacro purchase (n) 
    `(defmacro owner () (cons ,n `,`license)))

