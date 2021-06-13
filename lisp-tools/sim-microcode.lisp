(in-package :secd-tools)

(defvar *regs* (make-hash-table))
(defvar *mpc* 0)
(defvar *microcode* (read-intermediate))
(defvar *stack* nil)
(defvar *state* :running) ;; Initial value of state_reg is "00" ( = :running)
(defvar *stopped* nil)    ;; Initial value of vhd variable stopped is not explicitely set


(defconstant *content-bits* 28) ;; size of car and cdr field together, or size of integer or symbol
;; i.e. memory cell without type flags and gc flags
(defconstant *content-mask* (1- (ash 1 *content-bits*)))

(defun sim-reset ()
  (setf *mpc* 0)
  (setf *regs* (make-hash-table))
  (setf *stack* nil)
  (setf *stopped* nil)
  )

(defun read-reg (name)
  (or (gethash name *regs*) 0))

(defun write-reg (name value)
  (setf (gethash name *regs*) value))

(defmethod read-mem ((addr integer))
  (aref *memory* addr))

(defmethod read-mem ((addr memory-word))
  (aref *memory*
		  (ecase (mw-type addr)
			 (cons (mw-cdr addr))
			 (number (mw-car addr))
			 )))

(defun write-mem (addr value)
  (setf (aref *memory* addr) value))

(defmethod get-cdr ((i integer)) i)
(defmethod get-cdr ((mw memory-word))
  (mw-cdr mw))

;; VHDL extracts the bits representing to the memory cell's content
;; as values for comparison. These bits are interpreted as
;; signed values.
(defun memory-content (mw)
  (let ((unsigned  (logand (make-binary mw) *content-mask*)))
	 (if (logbitp (1- *content-bits*) unsigned)
		  (- unsigned *content-mask* 1)
		  unsigned)
	 ))

(defvar *execute-next-pre-hook*
  (lambda (mi) (format t "~A~%" mi)))

(defun execute-next-unchecked-stop ()
  (let ((mi (aref *microcode* *mpc*))
        databus
		  alu-out)
	 (funcall *execute-next-pre-hook* mi)
    (when (mi-read mi)
      (case (mi-read mi)
        (num (setf databus (1- +memory-size+)));;(make-memory-word :type 'number :car (1- +memory-size+))))
        (mem (setf databus (read-mem (gethash 'mar *regs*))))
		  (nilx (setf databus (make-memory-word :type 'cons :car 0 :cdr 0)))
		  (true  (setf databus (make-memory-word :type 'cons :car 0 :cdr 1)))
		  (false (setf databus (make-memory-word :type 'cons :car 0 :cdr 2)))
		  (cons (setf databus (make-memory-word :type 'cons
															 :car (gethash 'x1 *regs*)
															 :cdr (gethash 'x2 *regs*))))
        (t (setf databus (gethash (mi-read mi) *regs*)))))
    (when (mi-alu mi)
      (setf alu-out (execute-alu mi databus)))
    (when (mi-write mi)
		(case (mi-write mi)
		  ((buf1 buf2)
			(setf (gethash (mi-write mi) *regs*) alu-out))
		  (car
			(setf (gethash 'car *regs*) (mw-car databus)))
		  (arg
			(setf (gethash 'arg *regs*) databus))
		  (bidir
			(setf (aref *memory* (gethash 'mar *regs*)) databus))
		  (t
			(setf (gethash (mi-write mi) *regs*)
					(if (integerp databus) databus
						 (mw-cdr databus))))))
	 (setf *mpc*
          (ecase (mi-test mi)
            (jump (mi-a mi))
            (button? (mi-a mi))
				(nil?    (if (or (null databus)
									  (and (or (eq (mw-type databus) 'symbol)
												  (eq (mw-type databus) 'number))
											 (eq (mw-car  databus) 0)))
								 (mi-a mi) (1+ *mpc*)))
				(leq?    (if (<= (memory-content databus)
									  (memory-content (gethash 'arg *regs*)))
								 (mi-a mi) (1+ *mpc*)))
				(eq?    (if (= (memory-content databus)
								   (memory-content (gethash 'arg *regs*)))
								(mi-a mi) (1+ *mpc*)))
				(num?   (if (= (get-cdr databus) (1- +memory-size+))
								(mi-a mi) (1+ *mpc*)))
				(mark?  (if (mw-mark databus)
								(mi-a mi) (1+ *mpc*)))
				(atom?  (if (not (eql (mw-type databus) 'cons))
								(mi-a mi) (1+ *mpc*)))
				(dispatch
				 (format t "arg = ~S~%" (gethash 'arg *regs*))
				 (mw-car (gethash 'arg *regs*)))
				(call
				 (push *mpc* *stack*)
				 (mi-a mi))
				(return
				  (1+ (pop *stack*)))
				(stop
				 (setf *stopped* t)
				 *mpc*
				 )
            ((nil next) (1+ *mpc*))))))


(defun execute-next ()
  (if (not *stopped*)
		(execute-next-unchecked-stop)))


(defun print-regs ()
  (maphash
	(lambda (name value)
	  (format t "~A: ~A~%" name value)
	  )
	*regs*)
  )
