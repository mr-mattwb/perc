
(defvar *infile* "perc-5s.wav")
(defvar *outfile* "out-~as.wav")
(defvar *iterator* 5)
(defvar *iterations* 1)
(defvar *trim-param* "trim 0 ~a")
(defvar *command* "/usr/bin/sox")
(defvar *cmd-out* *standard-output*)

(defun add-infile (cmd nxt)
    (if (> nxt 0) 
        (add-infile (cons *infile* cmd) (- nxt 1))
        cmd))

(defmacro create-args (secs outfile) 
    `(reverse (cons ,outfile (add-infile '() (/ ,secs *iterator*)))))

(defmacro run-cmd (cmd secs outf)
    `(sb-ext:run-program ,cmd (create-args ,secs ,outf) :output *cmd-out*))




