(defconst jvm--util-signature-type-map
  #s(hash-table
     data (
           ?B "Byte"
           ?C "Char"
           ?D "Double"
           ?F "Float"
           ?I "Int"
           ?J "Long"
           ?S "Short"
           ?V "Void"
           ?Z "Boolean"
           ?L "class"
           )))

(defun jvm--util-parse-signature (signature &optional current-index)
  (when (null current-index) (setq current-index 0))
  (let ((data nil)
        (deep-array 0))
    (while (< current-index (length signature))
      (let ((s (elt signature current-index)))
        (case s
          ((or ?B ?C ?D ?F ?I ?J ?S ?V ?Z)
           (setq data (append data
                              `((
                                 (type . ,(gethash s jvm--util-signature-type-map))
                                 (deep-array . ,deep-array)
                                 ))))
           (setq deep-array 0))
          (?L
           (incf current-index)
           (let ((build ""))
             (while (not (char-equal ?\; (elt signature current-index)))
               (setq build (concat build (list (elt signature current-index))))
               (incf current-index))
             (setq data (append data
                                `((
                                   (type . ,build)
                                   (deep-array . ,deep-array)
                                   ))))
             (setq deep-array 0)))
          (?\[
           (incf deep-array)
           (incf current-index)
           (while (char-equal ?\[ (elt signature current-index))
             (incf deep-array)
             (incf current-index))

           ;; Don't increment at the end of while body
           (decf current-index))
          (?\(
           (incf current-index)
           (let ((build "")
                 arguments)
             (while (and
                     (< current-index (length signature))
                     (not (char-equal ?\) (elt signature current-index))))
               (setq build (concat build (list (elt signature current-index))))
               (incf current-index))
             (setq arguments (if (string= build "")
                                 nil
                               (jvm--util-parse-signature build)))
             (setq data (append data
                                `(
                                  (arguments . ,arguments)
                                  (arguments-count . ,(length arguments))
                                  )))))
          ))
      (incf current-index))
    data))

(provide 'jvm-util)
