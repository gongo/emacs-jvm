;; -*- lexical-binding: t -*-

(require 'bindat)
(require 'let-alist)
(require 'cl)
(require 'cl-lib)

(require 'jvm-instruction)

;;
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4
;;
(defconst jvm--classfile-spec-cp-info
  '((tag u8)
    (union (tag)
           (7 ;; CONSTANT_Class_info
            (name-index u16))
           (9 ;; CONSTANT_Fieldref_info
            (class-index u16)
            (name-and-type-index u16))
           (10 ;; CONSTANT_Methodref_info
            (class-index u16)
            (name-and-type-index u16))
           (11 ;; CONSTANT_InterfaceMethod_info
            (class-index u16)
            (name-and-type-index u16))
           (8 ;; CONSTANT_String_info
            (string-index u16))
           (3 ;; CONSTANT_Integer_info
            (bytes u32))
           (4 ;; CONSTANT_Float_info
            (bytes u32))
           (5 ;; CONSTANT_Long_info
            (high-bytes u32)
            (low-bytes u32))
           (6 ;; CONSTANT_Double_info
            (high-bytes u32)
            (low-bytes u32))
           (12 ;; CONSTANT_NameAndType_info
            (name-index u16)
            (descriptor-index u16))
           (1 ;; CONSTANT_Utf8_info
            (length u16)
            (bytes vec (length) u8))
           (15 ;; CONSTANT_MethodHandle_info
            (reference-kind u8)
            (reference-index u16))
           (16 ;; CONSTANT_MethodType_info
            (descriptor-index u16))
           (18 ;; CONSTANT_InvokeDynamic_info
            (bootstrap-method-attr-index u16)
            (name-and-type-index u16))
           )))

;;
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.1
;;
(defconst jvm--classfile-spec-classfile
  '(
    (magic u32)
    (minor-version u16)
    (major-version u16)
    (constant-pool-count u16)
    (constant-pool repeat (eval (- last 1)) (struct jvm--classfile-spec-cp-info))
    (access-flags u16)
    (this-class u16)
    (super-class u16)
    (interfaces-count u16)
    (interfaces repeat (interfaces-count) u16)
    (fields-count u16)
    (fields repeat (fields-count) (struct jvm--classfile-spec-field-info))
    (methods-count u16)
    (methods repeat (methods-count) (struct jvm--classfile-spec-method-info))
    (attributes-count u16)
    (attributes repeat (attributes-count) (struct jvm--classfile-spec-attribute-info))
    ))

;;
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.5
;;
(defconst jvm--classfile-spec-field-info
  '(
    (access-flags u16)
    (name-index u16)
    (descriptor-index u16)
    (attributes-count u16)
    (attributes repeat (attributes-count) (struct jvm--classfile-spec-attribute-info))
    ))

;;
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.6
;;
(defconst jvm--classfile-spec-method-info
  '(
    (access-flags u16)
    (name-index u16)
    (descriptor-index u16)
    (attributes-count u16)
    (attributes repeat (attributes-count) (struct jvm--classfile-spec-attribute-info))
    ))

;;
;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7
;;
(defconst jvm--classfile-spec-attribute-info
  '(
    (attribute-name-index u16)
    (attribute-length u32)
    (info vec (attribute-length) u8)
    ))

(defconst jvm--classfile-spec-code-attribute
  '(
    (max-stack u16)
    (max-locals u16)
    (code-length u32)
    (codes vec (code-length) u8)
    (exception-table-length u16)
    (exception-tables repeat (exception-table-length) (struct jvm--classfile-spec-exception-table))
    (attributes-count u16)
    (attributes repeat (attributes-count) (struct jvm--classfile-spec-attribute-info))
    ))

(defconst jvm--classfile-spec-exception-table
  '(
    (start-pc u16)
    (end-pc u16)
    (handler-pc u16)
    (catch-type u16)
    ))

(defconst jvm--classfile-spec-sourcefile-attribute
  '(
    (sourcefile-index u16)
    ))

(defstruct jvm--classfile
  magic
  minor-version
  major-version
  constant-pool
  access-flags
  this-class
  super-class
  interfaces
  fields
  methods
  attributes
  (this-class-name (let ((name-index (let-alist (elt constant-pool this-class) .name-index)))
                     (concat
                      (let-alist (elt constant-pool name-index) .bytes)))
                   :read-only t)
  )

(defstruct jvm--classfile-field
  (access-flags      nil :read-only t)
  (name-index        nil :read-only t)
  (descriptor-index  nil :read-only t)
  (attributes        nil :read-only t)
  (constant-pool     nil :read-only t)
  (name (concat (let-alist (elt constant-pool name-index) .bytes)) :read-only t)
  (descriptor (concat (let-alist (elt constant-pool descriptor-index) .bytes)) :read-only t)
  )

(defstruct jvm--classfile-method
  (access-flags      nil :read-only t)
  (name-index        nil :read-only t)
  (descriptor-index  nil :read-only t)
  (attributes        nil :read-only t)
  (constant-pool     nil :read-only t)
  (name (concat (let-alist (elt constant-pool name-index) .bytes)) :read-only t)
  (descriptor (concat (let-alist (elt constant-pool descriptor-index) .bytes)) :read-only t)
  (operand-stack (let ((code-attribute (cl-find-if (lambda (attr) (jvm--classfile-code-attribute-p attr)) attributes)))
                   (make-ring (if code-attribute
                                  (jvm--classfile-code-attribute-max-stack code-attribute)
                                0))))
  (local-variables (let ((code-attribute (cl-find-if (lambda (attr) (jvm--classfile-code-attribute-p attr)) attributes)))
                     (make-vector (if code-attribute
                                      (jvm--classfile-code-attribute-max-stack code-attribute)
                                    0)
                                  nil)))
  )

(defstruct jvm--classfile-code-attribute
  max-stack
  max-locals
  codes
  exception-tables
  attributes
  )

(defun jvm--classfile-load-fields (fields constant-pool)
  (map 'vector
       (lambda (field)
         (let-alist field
           (make-jvm--classfile-field
            :access-flags .access-flags
            :name-index .name-index
            :descriptor-index .descriptor-index
            :attributes (jvm--classfile-load-attributes .attributes constant-pool)
            :constant-pool constant-pool
           )))
       fields))

(defun jvm--classfile-load-methods (methods constant-pool)
  (map 'vector
       (lambda (method)
         (let-alist method
           (make-jvm--classfile-method
            :access-flags .access-flags
            :name-index .name-index
            :descriptor-index .descriptor-index
            :attributes (jvm--classfile-load-attributes .attributes constant-pool)
            :constant-pool constant-pool
            )))
       methods))

(defun jvm--classfile-load-attributes (attributes constant-pool)
  (map 'vector
       (lambda (attr)
         (let* ((constant (elt constant-pool (cdr (assq 'attribute-name-index attr))))
                (name (concat (cdr (assq 'bytes constant))))
                (info (cdr (assq 'info attr)))
                )
           (cond ((equal name "Code")
                  (let ((decoded (bindat-unpack jvm--classfile-spec-code-attribute info)))
                    (make-jvm--classfile-code-attribute
                     :max-stack        (bindat-get-field decoded 'max-stack)
                     :max-locals       (bindat-get-field decoded 'max-locals)
                     :codes            (bindat-get-field decoded 'codes)
                     :exception-tables (bindat-get-field decoded 'exception-tables)
                     :attributes       (jvm--classfile-load-attributes (bindat-get-field decoded 'attributes) constant-pool)
                     )))
                 )))
       attributes))

(defun jvm--classfile-load (filepath)
  (let* ((data (with-temp-buffer
                 (insert-file-contents-literally filepath)
                 (buffer-substring-no-properties (point-min) (point-max))))
         (decoded (bindat-unpack jvm--classfile-spec-classfile (encode-coding-string data 'raw-text)))
         ;; The constant_pool table is indexed from 1 to constant_pool_count-1.
         (constant-pool (vconcat [nil] (bindat-get-field decoded 'constant-pool)))
         )
    (make-jvm--classfile
     :magic         (bindat-get-field decoded 'magic)
     :minor-version (bindat-get-field decoded 'minor-version)
     :major-version (bindat-get-field decoded 'major-version)
     :constant-pool constant-pool
     :access-flags  (bindat-get-field decoded 'access-flags)
     :this-class    (bindat-get-field decoded 'this-class)
     :super-class   (bindat-get-field decoded 'super-class)
     :interfaces    (bindat-get-field decoded 'interfaces)
     :fields        (jvm--classfile-load-fields (bindat-get-field decoded 'fields) constant-pool)
     :methods       (jvm--classfile-load-methods (bindat-get-field decoded 'methods) constant-pool)
     :attributes    (jvm--classfile-load-attributes (bindat-get-field decoded 'attributes) constant-pool)
     )))

(defun jvm--classfile-find-method (classfile method-name)
  (cl-find-if
   (lambda (method)
     (equal (jvm--classfile-method-name method) method-name))
   (jvm--classfile-methods classfile)))

(defun jvm--classfile-run-method (method &rest args)
  (unless (jvm--classfile-method-p method)
    (signal 'wrong-type-argument (list 'jvm--classfile-method-p method)))

  (let* ((code-attribute (cl-find-if
                          (lambda (attr)
                            (jvm--classfile-code-attribute-p attr))
                          (jvm--classfile-method-attributes method)))
         (opcode-and-operands (when code-attribute (jvm--classfile-code-attribute-codes code-attribute)))
         (current-code-index 0)
         (current-frame (list (cons 'constant-pool   (jvm--classfile-method-constant-pool method))
                              (cons 'operand-stack   (jvm--classfile-method-operand-stack method))
                              (cons 'local-variables (jvm--classfile-method-local-variables method))))
         )

    (aset (let-alist current-frame .local-variables) 0 (vconcat args))

    (while (not (eq (length opcode-and-operands) current-code-index))
      (let* ((opcode (elt opcode-and-operands current-code-index))
             (instruction (aref jvm--instructions opcode))
             func
             operand-count
             )
        (when (null instruction)
          (error (format "Unknown opcode: %x" opcode)))
        (setq func (jvm--instruction-func instruction))
        (setq operand-count (jvm--instruction-operand-count instruction))
        (setq current-code-index (1+ current-code-index))
        (apply func
               current-frame
               (append (substring opcode-and-operands current-code-index (+ current-code-index operand-count)) nil))
        (setq current-code-index (+ current-code-index operand-count))))
    ))

(provide 'jvm-classfile)
