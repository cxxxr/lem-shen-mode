(defpackage :lem-shen-mode
  (:use :cl :lem :lem.language-mode)
  (:export :shen-mode))
(in-package :lem-shen-mode)

(defvar *shen-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\% #\: #\@
                   #\^ #\{ #\} #\~ #\# #\|)
   :paren-alist '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\")
   :escape-chars '()
   :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
   :line-comment-string "\\\\"
   :block-comment-pairs '(("\\*" . "*\\"))))

(ppcre:define-parse-tree-synonym symbol
  (:greedy-repetition 1 nil
   (:inverted-char-class #\( #\) :whitespace-char-class #\")))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defun wrap-symbol-names (&rest names)
  `(:sequence
    (:register
     (:group :case-insensitive-p
      ,(let ((args (apply #'word-length-sort names)))
         (if (null (rest args)) (first args) `(:alternation ,@args)))))
    (:alternation
     (:greedy-repetition 1 nil :whitespace-char-class)
     :whitespace-char-class :end-anchor #\( #\))))

(defun make-tmlanguage-shen ()
  (let ((patterns (make-tm-patterns
                   (make-tm-region
                    `(:sequence "\\\\")
                    "$"
                    :name 'syntax-comment-attribute)
                   (make-tm-region
                    `(:sequence "\"")
                    `(:sequence "\"")
                    :name 'syntax-string-attribute)
                   (make-tm-region
                    `(:sequence "\\*")
                    `(:sequence "*\\")
                    :name 'syntax-comment-attribute)
                   (make-tm-match
                    "(?:^|[\\s()\\[\\]])([+-]*\\d+\\.?\\d*)"
                    :captures (vector nil (make-tm-name 'syntax-constant-attribute)))
                   (make-tm-match
                    "(?:^|[\\s()\\[\\]])([+-]*\\d+\\.?\\d*[^\\s;()\\[\\]]+)"
                    :captures (vector nil (make-tm-name 'syntax-warning-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names "define" "defmacro" "defprolog" "package" "datatype")
                      (:register symbol))
                    :captures (vector nil
                                      (make-tm-name 'syntax-keyword-attribute)
                                      (make-tm-name 'syntax-function-name-attribute)))
                   (make-tm-match
                    `(:sequence
                      "("
                      ,(wrap-symbol-names
                        "and" "or" "if" "do" "lambda" "freeze" "let"
                        "cond" "cases" "trap-error" "where"
                        "defun" "/." "defcc"))
                    :captures (vector nil (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    "[\\s()\\[\\]{}]([A-Z][^\\s()\\[\\];{}]*)(?=$|[\\s;()\\[\\]{}])"
                    :captures (vector nil (make-tm-name 'syntax-variable-attribute)))
                   (make-tm-match
                    "[\\s()\\[\\]{}](->|<-|-->|<--|==>|<==|:=|__+)(?=$|[\\s;()\\[\\]{}])"
                    :captures (vector nil (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    "[\\s()\\[\\]{}](=|==|<|>|<=|>=|\\+|-|\\*|/)(?=$|[\\s;()\\[\\]{}])"
                    :captures (vector nil (make-tm-name 'syntax-keyword-attribute)))
                   (make-tm-match
                    "\\[\\]|\\(\\)"
                    :name 'syntax-constant-attribute))))
    (make-tmlanguage :patterns patterns)))

(define-major-mode shen-mode language-mode
    (:name "Shen"
     :keymap *shen-mode-keymap*
     :syntax-table *shen-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'beginning-of-defun-function) 'lem-lisp-mode::lisp-beginning-of-defun)
  (setf (variable-value 'end-of-defun-function) 'lem-lisp-mode::lisp-end-of-defun)
  (setf (variable-value 'calc-indent-function) 'calc-indent)
  (setf (variable-value 'line-comment) "\\\\")
  (setf (variable-value 'insertion-line-comment) "\\\\ ")
  (set-syntax-parser *shen-syntax-table* (make-tmlanguage-shen)))

(defun calc-indent (point)
  (lem-lisp-syntax:calc-indent point))

(pushnew (cons ".shen$" 'shen-mode) *auto-mode-alist* :test #'equal)
