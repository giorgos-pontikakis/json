(in-package :json)

(defun escape-char-json (char)
  (case char
    (#\Backspace "\\b")
    (#\Tab "\\t")
    (#\Newline "\\n")
    (#\Page "\\f")
    (#\Return "\\r")
    (#\\ "\\\\")
    (#\/ "\\/")
    (#\" "\\\"")
    (otherwise char)))

(defun escape-string-json (string)
  (let* ((test #'(lambda (char)
                   (find char
                         '(#\Backspace #\Tab #\Newline #\Page #\Return #\Newline #\\ #\/ #\"))))
         (first-pos (position-if test string)))
    (if (not first-pos)
        string
        (with-output-to-string (s)
          (loop for old-pos = 0 then (1+ pos)
                for pos = first-pos then (position-if test string :start old-pos)
                for char = (and pos (char string pos))
                while pos
                do
                   (write-sequence string s :start old-pos :end pos)
                   (princ (escape-char-json char) s)
                finally (write-sequence string s :start old-pos))))))


;;; ----------------------------------------------------------------------
;;; Lisp to JSON
;;; ----------------------------------------------------------------------
(defgeneric write-json (datum &optional stream)
  (:documentation "Write the JSON equivalent of a Lisp expression"))

(defmethod write-json ((datum integer) &optional (stream *standard-output*))
  "A Lisp integer is a JSON number"
  (format stream "~a" datum))

(defmethod write-json ((datum float) &optional (stream *standard-output*))
  "A Lisp float is a JSON number"
  (format stream "~f" datum))

(defmethod write-json ((datum ratio) &optional (stream *standard-output*))
  "A Lisp float is a JSON number"
  (format stream "~f" (coerce datum 'double-float)))

(defmethod write-json ((datum string) &optional (stream *standard-output*))
  "A Lisp string is a JSON string"
  (princ #\" stream)
  (princ (escape-string-json datum) stream)
  (princ #\" stream))

(defmethod write-json ((datum symbol) &optional (stream *standard-output*))
  "A Lisp symbol is a JSON string"
  (princ #\" stream)
  (princ datum stream)
  (princ #\" stream))

(defmethod write-json ((datum (eql t)) &optional (stream *standard-output*))
  "Lisp T is JSON true"
  (princ "true" stream))

(defmethod write-json ((datum (eql nil)) &optional (stream *standard-output*))
  "Lisp NIL is JSON false"
  (princ "false" stream))

(defmethod write-json ((datum character) &optional (stream *standard-output*))
  "A Lisp character is a JSON string of unary length"
  (princ (escape-char-json datum) stream))

(defmethod write-json ((datum cons) &optional (stream *standard-output*))
  "A Lisp pair object is a JSON pair"
  (write-json (car datum) stream)
  (write-json #\: stream)
  (write-json (cdr datum) stream))

(defmethod write-json ((datum vector) &optional (stream *standard-output*))
  "A Lisp vector is a JSON array"
  (princ #\[ stream)
  (loop for value across datum
        for delimiter = "" then #\,
        do
           (princ delimiter stream)
           (write-json value stream))
  (princ #\] stream))

(defmethod write-json ((datum hash-table) &optional (stream *standard-output*))
  "A Lisp hash-table is a JSON object"
  (princ #\{ stream)
  (loop for key being the hash-keys in datum using (hash-value value)
        for delimiter = "" then #\,
        do
           (princ delimiter stream)
           (write-json key stream)
           (princ #\: stream)
           (write-json value stream))
  (princ #\} stream))
