
(defclass envar ()
    ((name :initarg :name :accessor name)
     (switches :initarg :switches :accessor switches)
     (desc :initarg :desc :accessor desc)
     (default :initarg :default :accessor default))
    )

(defmethod get-env (e)
  (if (uiop:getenvp (name e))
    (uiop:getenv (name e))
    (progn 
        (setf (uiop:getenv (name e)) (default e))
        (uiop:getenv (name e)))))

(defmethod put-env (e v)
  (setf (uiop:getenv (name e)) v))

(defun getFile (fname) 
  (with-open-file (fin fname :direction :input)
    (let ((buf (make-array (file-length fin) :element-type 'unsigned-byte)))
      (read-sequence buf fin)
      buf)))

(defun putFile (fname buf)
  (with-open-file (fout fname :direction :output :if-exists :overwrite)
    (write-sequence buf fout)))
