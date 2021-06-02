;; Compile lispkit source code using the lispkit compiler
;; running on the lispkit interpreter on Common Lisp.
;; The executable can be generated on SBCL by calling save-lk-compile.

;; Usage: lk-compile <source file name>

;; The code is written to a file with a name derived from
;; the source file name by replacing the extension with 'lkc'.


(load "lispkit.lisp")

(defparameter +lispkit-compile-expression+
  '(LETREC COMPILE
	    (COMPILE LAMBDA (E)
	      (COMP E (QUOTE NIL) (QUOTE (4 21))))
	    (COMP LAMBDA (E N C)
	      (IF (ATOM E)
	          (CONS (QUOTE 1) (CONS (LOCATION E N) C))
	      (IF (EQ (CAR E) (QUOTE QUOTE))
	          (CONS (QUOTE 2) (CONS (CAR (CDR E)) C))
	      (IF (EQ (CAR E) (QUOTE ADD))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 15) C)))
	      (IF (EQ (CAR E) (QUOTE SUB))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 16) C)))
	      (IF (EQ (CAR E) (QUOTE MUL))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 17) C)))
	      (IF (EQ (CAR E) (QUOTE DIV))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 18) C)))
	      (IF (EQ (CAR E) (QUOTE REM))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 19) C)))
	      (IF (EQ (CAR E) (QUOTE LEQ))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 20) C)))
	      (IF (EQ (CAR E) (QUOTE EQ))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 14) C)))
	      (IF (EQ (CAR E) (QUOTE CAR))
	          (COMP (CAR (CDR E)) N (CONS (QUOTE 10) C))
	      (IF (EQ (CAR E) (QUOTE CDR))
	          (COMP (CAR (CDR E)) N (CONS (QUOTE 11) C))
	      (IF (EQ (CAR E) (QUOTE ATOM))
	          (COMP (CAR (CDR E)) N (CONS (QUOTE 12) C))
	      (IF (EQ (CAR E) (QUOTE CONS))
	          (COMP (CAR (CDR (CDR E))) N 
	                (COMP (CAR (CDR E)) N (CONS (QUOTE 13) C)))
	      (IF (EQ (CAR E) (QUOTE RPLACA))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 41) C)))
	      (IF (EQ (CAR E) (QUOTE RPLACD))
	          (COMP (CAR (CDR E)) N (COMP (CAR (CDR (CDR E))) N (CONS (QUOTE 42) C)))
	      (IF (EQ (CAR E) (QUOTE OR))
	          (LET (CONS (QUOTE 31) (CONS OR1 (CONS OR2 C)))
	            (OR1 COMP (CAR (CDR E)) N (QUOTE (9)))
	            (OR2 COMP (CAR (CDR (CDR E))) N (QUOTE (9))))
	      (IF (EQ (CAR E) (QUOTE NONE))
	          (CONS (QUOTE 32) C)
	      (IF (EQ (CAR E) (QUOTE FORCE)) 
	          (COMP (CAR (CDR E)) N (CONS (QUOTE 35) C))
	      (IF (EQ (CAR E) (QUOTE DELAY))
	          (LET (CONS (QUOTE 33) (CONS BODY C))
	            (BODY COMP (CAR (CDR E)) N (QUOTE (34))))
	      (IF (EQ (CAR E) (QUOTE PRINT))
	          (COMP (CAR (CDR E)) N (CONS (QUOTE 51) C))
	      (IF (EQ (CAR E) (QUOTE IF))
	          (LET (COMP (CAR (CDR E)) N
	                     (CONS (QUOTE 8) (CONS THENPT (CONS ELSEPT C))))
	            (THENPT COMP (CAR (CDR (CDR E))) N (QUOTE (9)))
	            (ELSEPT COMP (CAR (CDR (CDR (CDR E)))) N (QUOTE (9))))
	      (IF (EQ (CAR E) (QUOTE LAMBDA))
	          (LET (CONS (QUOTE 3) (CONS BODY C))
	            (BODY COMP (CAR (CDR (CDR E))) (CONS (CAR (CDR E)) N) (QUOTE (5))))
	      (IF (EQ (CAR E) (QUOTE LET))
	          (LET (LET (COMPLIS ARGS N (CONS (QUOTE 3) 
	                                          (CONS BODY (CONS (QUOTE 4) C))))
	                 (BODY COMP (CAR (CDR E)) M (QUOTE (5))))
	            (M CONS (VARS (CDR (CDR E))) N)
	            (ARGS EXPRS (CDR (CDR E))))
	      (IF (EQ (CAR E) (QUOTE LETREC))
	          (LET (LET (CONS (QUOTE 6) 
	                          (COMPLIS ARGS M (CONS (QUOTE 3) 
	                                                (CONS BODY (CONS (QUOTE 7) C)))))
	                 (BODY COMP (CAR (CDR E)) M (QUOTE (5))))
	            (M CONS (VARS (CDR (CDR E))) N)
	            (ARGS EXPRS (CDR (CDR E))))
	      (COMPLIS (CDR E) N (COMP (CAR E) N (CONS (QUOTE 4) C)))
	      )))))))))))))))))))))))))
	    (COMPLIS LAMBDA (E N C)
	      (IF (EQ E (QUOTE NIL)) (CONS (QUOTE 2) (CONS (QUOTE NIL) C))
	          (COMPLIS (CDR E) N (COMP (CAR E) N (CONS (QUOTE 13) C)))))
	    (LOCATION LAMBDA (E N)
	      (LETREC (IF (MEMBER E (CAR N)) (CONS (QUOTE 0) (POSN E (CAR N)))
	                  (INCAR (LOCATION E (CDR N))))
	        (MEMBER LAMBDA (E N)
	          (IF (EQ N (QUOTE NIL)) (QUOTE F)
	          (IF (EQ E (CAR N)) (QUOTE T) (MEMBER E (CDR N)))))
	        (POSN LAMBDA (E N)
	          (IF (EQ E (CAR N)) (QUOTE 0) (ADD (QUOTE 1) (POSN E (CDR N)))))
	        (INCAR LAMBDA (L)
	          (CONS (ADD (QUOTE 1) (CAR L)) (CDR L)))))
	    (VARS LAMBDA (D)
	      (IF (EQ D (QUOTE NIL)) (QUOTE NIL)
	          (CONS (CAR (CAR D)) (VARS (CDR D)))))
	    (EXPRS LAMBDA (D)
	      (IF (EQ D (QUOTE NIL)) (QUOTE NIL)
	          (CONS (CDR (CAR D)) (EXPRS (CDR D))))))
  )

(defun compile-lispkit-file (source-path)
  (let*
		((code-path (merge-pathnames (make-pathname :type "lkc") source-path))
		 (source-expression 
			(with-open-file (source-stream (open source-path))
			  (read source-stream)))
		 )
	 (with-open-file (code-stream code-path :direction :output)
		(write 
		 (lk-apply +lispkit-compile-expression+ (list source-expression))
		 :stream code-stream :level nil :length nil )
		)
	 )
  )

#+sbcl
(defun toplevel-function ()
  (if (not (= (length sb-ext:*posix-argv*) 2))
		(format t "Usage: ~A <source file name>~%" (first sb-ext:*posix-argv*))
		(compile-lispkit-file (second sb-ext:*posix-argv*)))
  )

#+sbcl
(defun save-lk-compile ()
  (sb-ext:save-lisp-and-die "lk-compile"
									 :toplevel #'toplevel-function
									 :executable t
									 :purify t)
  )
