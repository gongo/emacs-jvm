(defconst jvm--builtin-class-table (make-hash-table :test #'equal))

(defstruct jvm--builtin-class
  (constructor (lambda ()))
  (fields (make-hash-table :test #'equal))
  (methods (make-hash-table :test #'equal))
  (static-methods (make-hash-table :test #'equal))
  )

(puthash "java/lang/Integer"
         (let ((class (make-jvm--builtin-class)))
           (puthash "valueOf"
                    (lambda (x)
                      x)
                    (jvm--builtin-class-static-methods class))
           (puthash "parseInt"
                    (lambda (x)
                      (string-to-number x))
                    (jvm--builtin-class-static-methods class))
           class)
         jvm--builtin-class-table)

(puthash "java/io/PrintStream"
         (let ((class (make-jvm--builtin-class)))
           (puthash "println"
                    (lambda (x)
                      (message x))
                    (jvm--builtin-class-methods class))
           (puthash "printf"
                    (lambda (format &optional args)
                      (apply #'message format (append args nil)))
                    (jvm--builtin-class-methods class))
           class)
         jvm--builtin-class-table)

(puthash "java/lang/System"
         (let ((class (make-jvm--builtin-class)))
           (puthash "out"
                    (gethash "java/io/PrintStream" jvm--builtin-class-table)
                    (jvm--builtin-class-fields class))
           class)
         jvm--builtin-class-table)

(provide 'jvm-class)
