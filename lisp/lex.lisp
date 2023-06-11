;;;; lex.lisp

(in-package #:bcpl)

(defparameter *dollar-character* nil)
(defparameter *quote-character* nil)

;;; Lexical tokens:
;; $(           {
;; $)           }
;; TRUE
;; FALSE
;; "string"	''		*' *" ** *N *S *T *B *P *<whitespace>*
;; 'character'	$
;; #octal
;; #Xhex
;; identifier [A-Z][A-Z0-9.]*
;; @
;; !
;; *
;; /
;; REM
;; +
;; -
;; =
;; ¬= 		~=
;; <=
;; >=
;; <
;; >
;; <<
;; >>
;; &
;; |
;; EQV
;; NEQV
;; ¬		~
;; (		[
;; )		]
;; ->
;; TABLE
;; ,
;; VALOF
;; ;
;; MANIFEST
;; STATIC
;; :
;; GLOBAL
;; VEC
;; BE
;; LET
;; AND
;; BREAK
;; LOOP
;; ENDCASE
;; RETURN
;; FINISH
;; GOTO
;; RESULTIS
;; SWITCHON
;; INTO
;; REPEAT
;; REPEATUNTIL
;; REPEATWHILE
;; UNTIL
;; DO
;; WHILE
;; FOR
;; TO
;; BY
;; TEST
;; THEN
;; ELSE
;; IF
;; UNLESS
;; CASE
;; DEFAULT

;;; GET is not visible as a token.

(defconstant +eol+
  (code-char #o37))

(defconstant +newline+
  '(#\Newline #\Return #\Linefeed #.+eol+))

(defconstant +whitespace+
  `(#\Space #\Tab ,@+newline+))

(defconstant +letters+
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(defconstant +oct-digits+
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

(defconstant +digits+
  (append +oct-digits+ '(#\8 #\9)))

(defconstant +hex-digits+
  (append +digits+ '(#\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)))

(defun skip-comment (f)
  (loop
    (case (read-char f nil)
      (#.(cons nil +newline+)
       (return-from skip-comment)))))

(defun lex-/ (f)
  (case (peek-char nil f nil)
    (#\/ (skip-comment f) (lex f))
    ((nil) '/)
    (t '/)))

(defun lex-comma (f)
  (case (peek-char nil f nil)
    (#\, (read-char f) 'bcpl::|,,|)
    ((nil) '|,|)
    (t '|,|)))

(defun lex-minus (f)
  (case (peek-char nil f nil)
    (#\> (read-char f) 'bcpl::|->|)
    ((nil) '-)
    (t '-)))

(defun lex-> (f)
  (case (peek-char nil f nil)
    (#\> (read-char f) 'bcpl::|>>|)
    (#\= (read-char f) 'bcpl::|>=|)
    ((nil) '>)
    (t '>)))

(defun lex-< (f)
  (case (peek-char nil f nil)
    (#\< (read-char f) 'bcpl::|<<|)
    (#\= (read-char f) 'bcpl::|<=|)
    ((nil) '<)
    (t '<)))

(defun lex-~ (f)
  (case (peek-char nil f nil)
    (#\= (read-char f) 'bcpl::|~=|)
    ((nil) '~)
    (t '~)))

(defun lex-$ (f)
  (let ((c (read-char f)))
    (if *dollar-character*
        (character c)
        (ecase c
          (#\( 'bcpl::|{|)
          (#\) 'bcpl::|}|)))))

(defun lex-colon (f)
  (case (peek-char nil f nil)
    (#\= (read-char f) 'bcpl::|:=|)
    ((nil) '|:|)
    (t '|:|)))

(defun lex-% (f)
  (intern (concatenate 'string "%" (string (read-char f))) '#:bcpl))

(defun lex-oct (f c)
  (let ((s (string c)))
    (loop
      (case (peek-char nil f nil)
        (#.+oct-digits+
         (setq s (concatenate 'string s (string (read-char f)))))
        (t (return-from lex-oct (values (parse-integer s :radix 8))))))))

(defun lex-hex (f)
  (let ((s ""))
    (loop
      (case (peek-char nil f nil)
        (#.+hex-digits+
         (setq s (concatenate 'string s (string (read-char f)))))
        (t (return-from lex-hex (values (parse-integer s :radix 16))))))))

(defun lex-hash (f)
  (let ((c (read-char f)))
    (ecase c
      ((#\X #\x) (lex-hex f))
      (#.+oct-digits+ (lex-oct f c)))))

(defun lex-number (f c)
  (let ((s (string c)))
    (loop
      (case (peek-char nil f nil)
        (#.+digits+ (setq s (concatenate 'string s (string (read-char f)))))
        (t (return-from lex-number (values (parse-integer s))))))))
         
(defun escaped-char (f s)
  (let ((c (read-char f)))
    (concatenate 'string s
                 (string
                  (ecase c
                    (#.+whitespace+
                     (loop
                       (when (eql (read-char nil) #\*)
                         (return-from escaped-char s))))
                    ((#\N #\n) #\Newline)
                    ((#\T #\t) #\Tab)
                    ((#\S #\s) #\Space)
                    ((#\B #\b) #\Backspace)
                    ((#\P #\p) #\Page)
                    ((#\R #\r) #\Rubout)
                    ((#\P #\p) #\Page)
                    ((#\C #\c) #\Return)
                    ((#\L #\l) #\Linefeed)
                    (#\* #\*)
                    (#\" #\")
                    (#\' #\'))))))

(defun lex-string (f)
  (let ((s "") c)
    (loop
      (case (setq c (read-char f))
        (nil (error "End of file inside string literal."))
        (#\* (setq s (escaped-char f s)))
        (#\" (return-from lex-string (values s)))
        (t (setq s (concatenate 'string s (string c))))))))
         
(defun lex-quote (f)
  (let ((s "") c)
    (loop
      (case (setq c (read-char f))
        (nil (error "End of file inside single quote literal."))
        (#\' (return-from lex-quote
               (if *quote-character*
                   (character s)
                   (values s))))
        (t (setq s (concatenate 'string s (string c))))))))
         
(defun lex-identifier (f c)
  (let ((s (string c)))
    (loop
      (case (peek-char nil f nil)
        (#.(append +letters+ +digits+ '(#\.))
         (setq s (concatenate 'string s (string (read-char f)))))
        (t
         (return-from lex-identifier (values (intern s '#:bcpl))))))))

(defun lex (f)
  (let ((c (read-char f nil)))
    (ecase c
      (#.+whitespace+ (lex f))
      (#\@ 'bcpl::|@|)
      (#\! 'bcpl::|!|)
      (#\* 'bcpl::|*|)
      (#\= 'bcpl::|=|)
      (#\& 'bcpl::|&|)
      (#\| 'bcpl::|\||)
      (#\( 'bcpl::|(|)
      (#\) 'bcpl::|)|)
      (#\+ 'bcpl::|+|)
      (#\; 'bcpl::|;|)
      (#\{ 'bcpl::|{|)
      (#\} 'bcpl::|}|)
      (#\\ 'bcpl::|\\|)
      (#\_ 'bcpl::|_|)
      (#\, (lex-comma f))
      (#\/ (lex-/ f))
      (#\- (lex-minus f))
      (#\> (lex-> f))
      (#\< (lex-< f))
      (#\~ (lex-~ f))
      (#\$ (lex-$ f))
      (#\# (lex-hash f))
      (#\" (lex-string f))
      (#\' (lex-quote f))
      (#\: (lex-colon f))
      (#\% (lex-% f))
      (#.+digits+ (lex-number f c))
      (#.+letters+ (lex-identifier f c))
      ((nil) :eof))))
