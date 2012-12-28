(in-package :json-tests)

(defun write-json-string (obj)
  (with-output-to-string (s)
    (write-json obj s)))

(defsuite* (json-encoder))

(deftest json-numbers ()
  (is (string= "0" (write-json-string 0)))
  (is (string= "-13" (write-json-string -13)))
  (is (string= "13.02" (write-json-string 13.02)))
  (is (string= "13.02" (write-json-string 13.02D0)))
  (is (string= "13.02" (write-json-string 13.02L0)))
  (is (string= "13.02" (write-json-string 13.02S0)))
  (is (string= "-13.02" (write-json-string -13.02E0))))

(deftest json-objects ()
  (let ((ht (make-hash-table)))
    (loop for key in '(:a :b :c)
          for val in '(1 2 3.2)
          do (setf (gethash key ht) val))
    (is (string= "{\"A\":1,\"B\":2,\"C\":3.2}"
                 (write-json-string ht)))
    (is (string= "{}" (write-json-string (make-hash-table))))))

(deftest json-vectors ()
  (let ((vec (vector 1 "al	pha" #\- (make-hash-table)))) ;; string contains a tab character
    (is (string= (write-json-string vec)
                 "[1,\"al\\\tpha\",-,{}]"))
    (is (string= "[]" (write-json-string #())))))

(deftest json-all-tests ()
  (json-objects)
  (json-numbers)
  (json-vectors))
