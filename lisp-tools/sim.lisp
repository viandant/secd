(in-package :secd-tools)

(defvar *empty-stream* (make-broadcast-stream))

(defun load-microcode (filename)
  "Load SECD microcode from FILENAME into *MICROCODE*"
  (reset)
  (with-open-file (input filename)
	 (with-open-stream (*error-output* (make-synonym-stream '*standard-output*))
		(parse-microcode input :listing *empty-stream*)
		)
	 ))

(defun addr-sexp (addr &optional (visited nil))
  "Return the S-expression starting at address ADDR of SECD-memory *MEMORY*
and an alist with all addresses visited mapped to their S-expressions.
VISITED is an alist mapping addresses to S-expressions, which should contain
all addresses visited when this function is called recursively.
If ADDR is found in VISITED the corresponding S-expression is returned as the result.
VISITED allows to handle cyclic structures."
  (if (null addr) (values 'none visited)
		(let ((mw (read-mem addr)))
		  (case (mw-type mw)
			 (symbol (values (lookup-symbol-reverse (mw-car mw)) visited))
			 (number (values (mw-car mw) visited))
			 (cons
			  (let ((vpair (assoc addr visited)))
				 (if vpair (values (cdr vpair) visited)
					  (let* ((new-cons (cons nil nil))
								(visited0 (acons addr new-cons visited)))
						 (multiple-value-bind (car-sexp visited1) (addr-sexp (mw-car mw) visited0)
							(multiple-value-bind (cdr-sexp visited2) (addr-sexp (mw-cdr mw) visited1)
							  (rplaca new-cons car-sexp)
							  (rplacd new-cons cdr-sexp)
							  (values new-cons visited2))))
					  )
				 )
			  )
			 (t (values (list :error mw) visited))
			 ))
		)
  )

(defun print-sc ()
  "Print the S and C register."
  (let ((*print-circle* t))
	 (format t "s: ~A~%c: ~A~%"
				(addr-sexp (gethash 's *regs*))
				(sexp-code-sym (addr-sexp (gethash 'c *regs*)))
				)
	 )
  )

(defvar *code-alist*
  '((1 . ld) (2 . ldc) (3 . ldf) (4 . ap) (5 . rtn) (6 . dum)
	 (7 . rap) (8 . sel) (9 . join) (10 . car) (11 . cdr) (12 . atom)
	 (13 . cons) (14 . eq) (15 . add) (16 . sub) (17 . mul) (18 . div)
	 (19 . rem) (20 . leq) (21 . stop) (22 . external) (23 . cmd23)
	 (24 . cmd24) (25 . cmd25) (26 . cmd26) (27 . cmd27) (28 . cmd28)
	 (29 . cmd29) (30 . cmd30) (31 . fork) (32 . fail) (33 . delay)
	 (34 . delay0) (35 . force)))

(defun code-sym (code)
  (let ((p (assoc code *code-alist*)))
	 (if p (copy-tree p)
		  (cons code nil))))

(defun sexp-code-sym (sexp)
  (cond
	 ((numberp sexp) (code-sym sexp))
	 ((atom sexp) sexp)
	 (t (cons (sexp-code-sym (car sexp))
				 (sexp-code-sym (cdr sexp))))
	 )
  )

(defvar *secd-step-post-hook* #'print-sc)

(defun secd-step ()
  (execute-next)
  (do ((mi (aref *microcode* *mpc*) (aref *microcode* *mpc*)))
		((or *stopped*
			  (not (eql *state* :running))
			  (eql (mi-label mi)  'top-of-cycle)) mi)
	 (execute-next))
  (funcall *secd-step-post-hook*))

(defun get-running ()
  (do () ((eql *state* :running))
	 (execute-next)))

(defun run ()
  (do () (*stopped*) (secd-step)))

(defun run-with-forks ()
  (do () ((or *stopped* (eql *state* :failed)))
	 (case *state*
		((:running :halted)
		 (secd-step))
		((:gc :idle :external)
		 (error "Reached state ~A which this simulation currently cannot handle."
				  *state*))
		(:fork-requested
		 (format t "> Fork requested~%")
		 (multiple-value-bind
				 (regs mpc stack state stopped memory machine)
			  (let
					((*regs*
						(let ((new-hash (make-hash-table)))
						  (maphash (lambda (key value) (setf (gethash key new-hash) value)) *regs*)
						  new-hash
						  ))
					 (*mpc* *mpc*)
					 (*stack* (copy-seq *stack*))
					 (*state* *state*)
					 (*stopped* *stopped*)
					 (*memory* (copy-seq *memory*))
					 (*machine* :machine1))
				 (get-running)
				 (run-with-forks)
				 (values *regs* *mpc* *stack* *state* *stopped* *memory* *machine*))
			(format t "> Left fork branch finished with state ~A.~%" state)
			(if (eql state :failed)
				 (let ((*machine* :machine2))
					(get-running)
					(run-with-forks)
					(format t "> Right fork branch finished with state ~A.~%" *state*)
					)
				 (setq *regs* regs
						 *mpc* mpc
						 *stack* stack
						 *state* state
						 *stopped* stopped
						 *memory* memory)
				 )
			)
		 )
		)
	 )
  )

;; Abbreviations
(defun st () (secd-step))
  
(defun secd-debug-loop ()
  (do ((stop nil))
		(stop)
	 (case (read)
		(:e (execute-next))
		(:s (secd-step))
		(:r (run))
		(:x (setf stop t))))
  )

  
