;; Write memory dump of the problem into a file.
;; This file is intended for loading the problem into
;; the memory shared between the SECD system and a front system.

(in-package :secd-tools)

(defun write-syms (output-pathname)
  (with-open-file
		(out output-pathname
			  :direction :output
			  :if-exists :supersede
			  )
	 (let ((sym-assoc nil))
		(maphash #'(lambda (sym index)
						 (push (cons index sym) sym-assoc))
					*symbol-table*)
		(map
		 nil
		 (lambda (p)
			(format out "~a~a" (cdr p) #\Nul))
		 (sort sym-assoc #'< :key #'car)
		 ))
	 )
  )

(defun write-shared-ram (input argument output-pathname &optional sym-pathname)
  (create-memory input argument)
  (with-open-file
		(out output-pathname
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 32))
	 (map
	  nil
	  (lambda (mw)
		 (write-byte (secd-tools::make-binary mw) out))
	  secd-tools::*memory*)
	 )
  (if sym-pathname
		(write-syms sym-pathname))
  )


