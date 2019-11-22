(require 'let-alist)

(require 'jvm-util)
(require 'jvm-class)

(defstruct jvm--instruction
  (func          nil :read-only t)
  (operand-count nil :read-only t)
  )

(defconst jvm--instructions (make-vector #xFF nil))

(aset jvm--instructions #x02 (make-jvm--instruction
                              :func #'jvm--instruction-iconst-m1
                              :operand-count 0))

(aset jvm--instructions #x03 (make-jvm--instruction
                              :func #'jvm--instruction-iconst-0
                              :operand-count 0))

(aset jvm--instructions #x04 (make-jvm--instruction
                              :func #'jvm--instruction-iconst-1
                              :operand-count 0))

(aset jvm--instructions #x05 (make-jvm--instruction
                              :func #'jvm--instruction-iconst-2
                              :operand-count 0))

(aset jvm--instructions #x06 (make-jvm--instruction
                              :func #'jvm--instruction-iconst-3
                              :operand-count 0))

(aset jvm--instructions #x07 (make-jvm--instruction
                              :func #'jvm--instruction-iconst-4
                              :operand-count 0))

(aset jvm--instructions #x08 (make-jvm--instruction
                              :func #'jvm--instruction-iconst-5
                              :operand-count 0))

(aset jvm--instructions #x12 (make-jvm--instruction
                              :func #'jvm--instruction-ldc
                              :operand-count 1))

(aset jvm--instructions #x1A (make-jvm--instruction
                              :func #'jvm--instruction-iload-0
                              :operand-count 0))

(aset jvm--instructions #x1B (make-jvm--instruction
                              :func #'jvm--instruction-iload-1
                              :operand-count 0))

(aset jvm--instructions #x1C (make-jvm--instruction
                              :func #'jvm--instruction-iload-2
                              :operand-count 0))

(aset jvm--instructions #x1D (make-jvm--instruction
                              :func #'jvm--instruction-iload-3
                              :operand-count 0))

(aset jvm--instructions #x2A (make-jvm--instruction
                              :func #'jvm--instruction-aload-0
                              :operand-count 0))

(aset jvm--instructions #x2B (make-jvm--instruction
                              :func #'jvm--instruction-aload-1
                              :operand-count 0))

(aset jvm--instructions #x2C (make-jvm--instruction
                              :func #'jvm--instruction-aload-2
                              :operand-count 0))

(aset jvm--instructions #x2D (make-jvm--instruction
                              :func #'jvm--instruction-aload-3
                              :operand-count 0))

(aset jvm--instructions #x32 (make-jvm--instruction
                              :func #'jvm--instruction-aaload
                              :operand-count 0))

(aset jvm--instructions #x3B (make-jvm--instruction
                              :func #'jvm--instruction-istore-0
                              :operand-count 0))

(aset jvm--instructions #x3C (make-jvm--instruction
                              :func #'jvm--instruction-istore-1
                              :operand-count 0))

(aset jvm--instructions #x3D (make-jvm--instruction
                              :func #'jvm--instruction-istore-2
                              :operand-count 0))

(aset jvm--instructions #x3E (make-jvm--instruction
                              :func #'jvm--instruction-istore-3
                              :operand-count 0))

(aset jvm--instructions #x53 (make-jvm--instruction
                              :func #'jvm--instruction-aastore
                              :operand-count 0))

(aset jvm--instructions #x57 (make-jvm--instruction
                              :func #'jvm--instruction-pop
                              :operand-count 0))

(aset jvm--instructions #x59 (make-jvm--instruction
                              :func #'jvm--instruction-dup
                              :operand-count 0))

(aset jvm--instructions #x60 (make-jvm--instruction
                              :func #'jvm--instruction-iadd
                              :operand-count 0))

(aset jvm--instructions #x68 (make-jvm--instruction
                              :func #'jvm--instruction-imul
                              :operand-count 0))

(aset jvm--instructions #xB1 (make-jvm--instruction
                              :func #'jvm--instruction-return
                              :operand-count 0))

(aset jvm--instructions #xB2 (make-jvm--instruction
                              :func #'jvm--instruction-getstatic
                              :operand-count 2))

(aset jvm--instructions #xB6 (make-jvm--instruction
                              :func #'jvm--instruction-invokevirtual
                              :operand-count 2))

(aset jvm--instructions #xB8 (make-jvm--instruction
                              :func #'jvm--instruction-invokestatic
                              :operand-count 2))

(aset jvm--instructions #xBD (make-jvm--instruction
                              :func #'jvm--instruction-anewarray
                              :operand-count 2))

(defun jvm--instruction-iconst-m1 (frame)
  (ring-insert (let-alist frame .operand-stack) -1))

(defun jvm--instruction-iconst-0 (frame)
  (ring-insert (let-alist frame .operand-stack) 0))

(defun jvm--instruction-iconst-1 (frame)
  (ring-insert (let-alist frame .operand-stack) 1))

(defun jvm--instruction-iconst-2 (frame)
  (ring-insert (let-alist frame .operand-stack) 2))

(defun jvm--instruction-iconst-3 (frame)
  (ring-insert (let-alist frame .operand-stack) 3))

(defun jvm--instruction-iconst-4 (frame)
  (ring-insert (let-alist frame .operand-stack) 4))

(defun jvm--instruction-iconst-5 (frame)
  (ring-insert (let-alist frame .operand-stack) 5))

(defun jvm--instruction-aaload (frame)
  (let-alist frame
    (let ((index (ring-remove .operand-stack 0))
          (arrayref (ring-remove .operand-stack 0)))
      (ring-insert .operand-stack (aref arrayref index)))))

(defun jvm--instruction-istore-0 (frame)
  (let-alist frame
    (aset .local-variables 0 (ring-remove .operand-stack 0))))

(defun jvm--instruction-istore-1 (frame)
  (let-alist frame
    (aset .local-variables 1 (ring-remove .operand-stack 0))))

(defun jvm--instruction-istore-2 (frame)
  (let-alist frame
    (aset .local-variables 2 (ring-remove .operand-stack 0))))

(defun jvm--instruction-istore-3 (frame)
  (let-alist frame
    (aset .local-variables 3 (ring-remove .operand-stack 0))))

(defun jvm--instruction-ldc (frame index)
  (let-alist frame
    (let* ((constant-pool .constant-pool)
           (value (elt constant-pool index)))
      (ring-insert .operand-stack
                   (let-alist value
                     (case .tag
                       (8 (concat (let-alist (elt constant-pool .string-index) .bytes)))
                       (t value)))
                   ))))

(defun jvm--instruction-iload-0 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 0))))

(defun jvm--instruction-iload-1 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 1))))

(defun jvm--instruction-iload-2 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 2))))

(defun jvm--instruction-iload-3 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 3))))

(defun jvm--instruction-aload-0 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 0))))

(defun jvm--instruction-aload-1 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 1))))

(defun jvm--instruction-aload-2 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 2))))

(defun jvm--instruction-aload-3 (frame)
  (let-alist frame
    (ring-insert .operand-stack (aref .local-variables 3))))

(defun jvm--instruction-getstatic (frame indexbyte1 indexbyte2)
  (let ((index (logior (lsh indexbyte1 8) indexbyte2)))
    (let-alist frame
      (ring-insert .operand-stack (elt .constant-pool index)))))

(defun jvm--instruction-aastore (frame)
  (let-alist frame
    (let ((value (ring-remove .operand-stack 0))
          (index (ring-remove .operand-stack 0))
          (arrayref (ring-remove .operand-stack 0)))
      (aset arrayref index value))))

(defun jvm--instruction-pop (frame)
  (let-alist frame
    (ring-remove .operand-stack 0)))

(defun jvm--instruction-dup (frame)
  (let-alist frame
    (let ((v (ring-remove .operand-stack 0)))
      (ring-insert .operand-stack v)
      (ring-insert .operand-stack v))))

(defun jvm--instruction-iadd (frame)
  (let-alist frame
    (let ((v1 (ring-remove .operand-stack 0))
          (v2 (ring-remove .operand-stack 0)))
      (ring-insert .operand-stack (+ v1 v2)))))

(defun jvm--instruction-imul (frame)
  (let-alist frame
    (let ((v1 (ring-remove .operand-stack 0))
          (v2 (ring-remove .operand-stack 0)))
      (ring-insert .operand-stack (* v1 v2)))))

(defun jvm--instruction-return (frame)
  ;; Nothing to do
  )

(defun jvm--instruction-invokevirtual (frame indexbyte1 indexbyte2)
  (let ((constant-pool (cdr (assoc 'constant-pool frame)))
        (operand-stack (cdr (assoc 'operand-stack frame)))
        (pool-index (logior (lsh indexbyte1 8) indexbyte2))
        (callee-info nil)
        (method-name nil)
        (method-arg-descriptor nil)
        (method-arg-count nil)
        (method-args nil)
        (invoker-class nil)
        )

    (setq callee-info (elt constant-pool (let-alist (elt constant-pool pool-index) .name-and-type-index)))
    (setq method-name (concat (let-alist (elt constant-pool (let-alist callee-info .name-index)) .bytes)))
    (setq method-arg-descriptor (concat (let-alist (elt constant-pool (let-alist callee-info .descriptor-index)) .bytes)))
    (setq method-arg-count (let-alist (jvm--util-parse-signature method-arg-descriptor) .arguments-count))
    (setq method-args (make-vector method-arg-count nil))

    (dotimes (i method-arg-count)
      (aset method-args (- method-arg-count i 1) (ring-remove operand-stack 0)))

    (setq callee (ring-remove operand-stack 0))

    (setq callee-class (concat (let-alist (elt constant-pool (let-alist (elt constant-pool (let-alist callee .class-index)) .name-index)) .bytes)))
    (setq callee-field (concat (let-alist (elt constant-pool (let-alist (elt constant-pool (let-alist callee .name-and-type-index)) .name-index)) .bytes)))

    (setq hoge (gethash callee-field (jvm--builtin-class-fields (gethash callee-class jvm--builtin-class-table))))
    (setq func (gethash method-name (jvm--builtin-class-methods hoge)))

    (ring-insert operand-stack (apply func (append method-args nil)))
    ))

(defun jvm--instruction-invokestatic (frame indexbyte1 indexbyte2)
  (let ((constant-pool (cdr (assoc 'constant-pool frame)))
        (operand-stack (cdr (assoc 'operand-stack frame)))
        (pool-index (logior (lsh indexbyte1 8) indexbyte2))
        (callee-info nil)
        (method-name nil)
        (method-arg-descriptor nil)
        (method-arg-count nil)
        (method-args nil)
        (invoker-class nil)
        )

    (setq class-info (elt constant-pool (let-alist (elt constant-pool pool-index) .class-index)))
    (setq class-name (concat (let-alist (elt constant-pool (let-alist class-info .name-index)) .bytes)))
    (setq callee-info (elt constant-pool (let-alist (elt constant-pool pool-index) .name-and-type-index)))
    (setq method-name (concat (let-alist (elt constant-pool (let-alist callee-info .name-index)) .bytes)))
    (setq method-arg-descriptor (concat (let-alist (elt constant-pool (let-alist callee-info .descriptor-index)) .bytes)))
    (setq method-arg-count (let-alist (jvm--util-parse-signature method-arg-descriptor) .arguments-count))
    (setq method-args (make-vector method-arg-count nil))

    (dotimes (i method-arg-count)
      (aset method-args (- method-arg-count i 1) (ring-remove operand-stack 0)))

    (setq hoge (gethash class-name jvm--builtin-class-table))
    (setq func (gethash method-name (jvm--builtin-class-static-methods hoge)))

    (ring-insert operand-stack (apply func (append method-args nil)))
    ))

(defun jvm--instruction-anewarray (frame _indexbyte1 _indexbyte2)
  (let-alist frame
    (let ((count (ring-remove .operand-stack 0)))
      (ring-insert .operand-stack (make-vector count nil)))))

(provide 'jvm-instruction)
