(in-package :secd-tools)

(defun mw-number-op1 (op a)
  (make-memory-word
	:type 'number
	:car  (funcall op (mw-car a))
	)
  )

(defun mw-number-op2 (op a b)
  (make-memory-word
	:type 'number
	:car  (funcall op (mw-car a) (mw-car b))
	)
  )

(defun execute-alu (mi databus &optional (regs *regs*))
  (case (mi-alu mi)
	 (nil
	  (make-memory-word :type 'symbol
							  :car  0
							  :note nil
							  )
	  )
	 (dec (mw-number-op1 #'1- (gethash 'arg regs)))
	 (add (mw-number-op2 #'+ databus (gethash 'arg regs)))
	 (sub (mw-number-op2 #'- databus (gethash 'arg regs)))
	 (mul (mw-number-op2 #'* databus (gethash 'arg regs)))
	 ;; Note #'/ returns a rational number. We need an integer
	 (div (mw-number-op2 #'floor databus (gethash 'arg regs)))
	 (rem (mw-number-op2 #'rem databus (gethash 'arg regs))) 

	 (replcar
	  (let ((arg (gethash 'arg regs)))
		 (make-memory-word
		  :type 'cons
		  :car (gethash 'y2 regs)
		  :cdr (mw-cdr arg)
		  :mark (mw-mark arg)
		  :field (mw-field arg)
		  :note (mw-note arg))
		 ))
	 (replcdr
	  (let ((arg (gethash 'arg regs)))
		 (make-memory-word
		  :type 'cons
		  :car (mw-car arg)
		  :cdr (gethash 'y2 regs)
		  :mark (mw-mark arg)
		  :field (mw-field arg)
		  :note (mw-note arg))
		 ))
	 
	 ;; GC operations
	 ;; bit 30: Mark
	 ;; bit 31: Field
	 ;; bit 30 - 28: cell type
	 ;;   cons: 0
	 ;; bit 27 - 14 : car
	 ;; bit 13 - 0  : cdr
	 (set-mark
	  (let ((res (copy-structure (gethash 'arg regs))))
		 (setf (mw-mark res) t)
		 res)
	  )
	 (clear-mark
	  (let ((res (copy-structure (gethash 'arg regs))))
		 (setf (mw-mark res) nil)
		 res)
	  )
	 (gcmark
	  (let ((arg (gethash 'arg regs)))
		 
		 (setf (gethash 'buf2 regs)
				 (make-memory-word
				  :mark t
				  :field t
				  :type 'cons
				  :car (gethash 'y1 regs)
				  :cdr (mw-car arg)
				  )
				 )
		 (setf (gethash 'root regs) (mw-car arg))
		 )
	  nil)
	 
	 ;; when gcreset =>
    ;; -- report "gcreset";
    ;; assert(arg(29 downto 28) = "00") report "trying to gcreset non-cons";
    ;; buf2(31 downto 28) <= arg(31 downto 28);
    ;; buf2(27 downto 14) <= arg(27 downto 14);
    ;; buf2(13 downto 0) <= y1;
    ;; root <= y2;
    ;; parent <= arg(13 downto 0);
	 
	  
	 (gcresest
	  (let ((new-buf2
				 (copy-structure (gethash 'arg regs))))
		 (setf (mw-cdr new-buf2) (gethash 'y1 regs))
		 (setf (gethash 'buf2 regs) new-buf2)
		 (setf (gethash 'root regs) (gethash 'y2 regs))
		 (setf (gethash 'parent regs) (mw-cdr (gethash 'arg regs)))
		 nil
		 )
	  )
	 
	 ;; assert(arg(29 downto 28) = "00") report "trying to gcreverse non-cons";
    ;; buf2(31) <= '0';   field
    ;; buf2(30) <= '1';   mark
    ;; buf2(29 downto 28) <= "00";
    ;; buf2(27 downto 14) <= y2;               -- car(parent) := root
    ;; buf2(13 downto 0) <= arg(27 downto 14); -- cdr(parent) := car(parent)
    ;; root <= arg(13 downto 0);

	 (gcreverse
	  (let ((arg (gethash 'arg regs)))
		 (setf (gethash 'buf2 regs)
				 (make-memory-word
				  :mark t
				  :field nil
				  :type 'cons
				  :car (gethash 'y2 regs)
				  :cdr (mw-cdr arg)
				  )
				 )
		 (setf (gethash 'root regs)
				 (mw-cdr arg))
		 nil
		 )
	  )

	 (stop     (setf *state* :stopped))    ;; stopped == 1
	 (running  (setf *state* :running))    ;; state_reg == "00"
	 (external (setf *state* :external))   ;; state_reg == "11"
	 (gc       (setf *state* :gc))         ;; state_reg == "10"
	 (halted   (setf *state* :halted))     ;; state_reg == "01"
	 (idle     (setf *state* :idle))       ;; not yet used
	 (fail     (setf *state* :failed))     ;; not yet used
	 (fork     (setf *state* :fork-requested)) ;; not yet used
	 )
  )
