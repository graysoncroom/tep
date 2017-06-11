#!/bin/clisp

(defparameter *usage* '("Commands:"
                        ":wq   || save and quit"
                        ":q    || quit without saving"
                        ":help || display this help menu"))
(defun read-file (name)
  "Reads file 'name' and returns contents of file as a 
   list of strings where each string represents a new line.
   If no file exists, one named 'name' will be created.
   Contents of file is returned."
  (with-open-file (input 
                    name
                    :direction :input
                    :if-does-not-exist :create)
    (when input
      (loop :for line = (read-line input nil)
            :while line
            :collect line))))

(defun write-file (name new-data)
  "Overwrites file 'name' and replaces it with 'new-data'."
  (with-open-file (output
                    name
                    :direction :output
                    :if-does-not-exist :create
                    :if-exists :supersede)
    (when output
      (format output "~{~a~%~}" new-data))))

(defun flush-screen ()
  (dotimes (x 50)
    (format t "~%")))

(defun init (name &optional flushp)
  "User will be prompted to type text until a 'quit-keyword' command is entered"
  (if flushp
      (flush-screen))
  (let ((buffer (read-file name)))
    (format t "~{~a~%~}" buffer) 
    (labels ((input-builder ()
               (let ((user-input (read-line))
                     (quit-keywords '(":wq" ":q")))
                 (cond
                   ((member user-input quit-keywords :test #'string=) 
                    (cons user-input nil))
                   ((string= user-input ":help")
                    (format t "~{~a~%~}" *usage*)
                    (input-builder))
                   (t
                    (cons user-input (input-builder)))))))
      (let* ((user-typed-text (input-builder))
             (input-to-save (reverse (cdr (reverse user-typed-text))))
             (command (car (reverse user-typed-text))))
        (cond
          ((string= command ":wq")
           (write-file name (append buffer input-to-save)))
          ((string= command ":q") nil))))))

(init (car *args*))
