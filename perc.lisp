

(defclass EnvParam ()
    ((default :initarg :default)
     (name :type string :initarg :name)
     (descr :type string :initarg :descr)
     (switch :type string :initarg :switch)))

(defclass Env (EnvParam)
    ())

(defmethod get-env ((env Env))
    (let ((gval (uiop:getenv (slot-value env 'name))))
        (if (null gval)
            (slot-value env 'default)
            gval)))

(defvar buildCommand (make-instance 'Env
    :name "BUILDCOMMAND"
    :default "/usr/bin/sox"
    :switch "--build-command"
    :descr "Conversion command"))
(defvar durCommand (make-instance 'Env
    :name "DURCOMMAND"
    :default "/usr/bin/soxi -D"
    :switch "--dur-command"
    :descr "Duration of the sound file in seconds"))
(defvar playCommand (make-instance 'Env
    :name "PLAYCOMMAND"
    :default "/usr/bin/play"
    :switch "--play-command"
    :descr "Play the sound file"))
(defvar percFile (make-instance 'Env
    :name "PERCFILE"
    :default "perc-5s.wav"
    :switch "--perc-file"
    :descr "Percolation file"))
(defvar outFile (make-instance 'Env
    :name "OUTFILE"
    :default "out.wav"
    :switch "--out-file"
    :descr "Output filename"))
(defvar seconds (make-instance 'Env
    :name "SECONDS"
    :default 30
    :switch "--seconds"
    :descr "Second of percolation to play"))

(defmethod file-duration (wavfile)
    (let* ((cmd (concatenate 'string (get-env durCommand) " " (get-env percFile)))
           (rsp (uiop:run-program cmd :output :string)))
        (parse-integer rsp :junk-allowed t)))

