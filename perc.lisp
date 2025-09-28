
(defclass envar ()
  ((name :accessor name 
         :initarg :name)
   (desc :accessor desc
         :initarg :desc)
   (default :accessor default
            :initarg :default)))

(defmethod getvar ((e envar)) 
    (if (null (uiop:getenv (name e))) (default e) (uiop:getenv (name e))))

(defvar *build-command* (make-instance 'envar
                                       :name "commands.build"
                                       :default "/usr/bin/sox" 
                                       :desc "command to convert sound files"))
(defvar *play-command* (make-instance 'envar 
                                      :name "commands.play" 
                                      :default "/usr/bin/play" 
                                      :desc "Command to play a sound file"))
(defvar *dur-command* (make-instance 'envar
                                     :name "commands.duration"
                                     :default "/usr/bin/soxi -D"
                                     :desc "Command to get sound fie duration in seconds"))
(defvar *out-file* (make-instance 'envar
                                  :name "percolate.outfile"
                                  :default "out.wav"
                                  :desc "Output filename"))
(defvar *seconds* (make-instance 'envar
                                 :name "percolate.seconds"
                                 :default 20
                                 :desc "Seconds of percolation"))
(defvar *play* (make-instance 'envar
                              :name "percolate.play"
                              :default nil
                              :desc "Play the output file"))
(defvar *file-ext* (make-instance 'envar
                                  :name "percolate.extension"
                                  :default ".wav"
                                  :desc "File extension for output file"))
(defvar *perc-file* (make-instance 'envar
                                   :name "percolate.soundfile"
                                   :default ""
                                   :desc "Percolator sound file"))

(defmethod play-file (player fname)
    (uiop:run-program (concatenate 'string (getvar player) " " fname)))

(defmethod duration-file (cmd fname)
    (read-from-string
        (uiop:run-program (concatenate 'string (getvar cmd) " " fname) :output :string)))

(defun builder (seconds percfile)
  (labels ((aux (loops cmd)
                (if (<= loops 0) (concatenate 'string cmd " " (getvar *out-file*))
                  (aux (- loops seconds) (concatenate 'string cmd " " percfile)))))
    (aux (getvar *seconds*) (getvar *build-command*))))

