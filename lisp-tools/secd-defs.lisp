(in-package :secd-tools)

(defvar *secd-inst* '(nil
                      ld
                      ldc
                      ldf
                      ap
                      rtn
                      dum
                      rap
                      sel
                      join
                      car
                      cdr
                      atom
                      cons
                      eq
                      add
                      sub
                      mul
                      div
                      rem
                      leq
                      stop
		      external))

(defvar *next-mi-code* '(nil
                         jump
                         dispatch
                         mark?
                         field?
                         zero?
                         eq?
                         leq?
                         num?
                         atom?
                         nil?
                         button?
                         call
                         return
                         stop
								 forkack?
								 machine1?
								 ))

(defvar *flagsunit-bits* '(atom
                           mark
                           field
                           zero
                           nil
                           eq
                           leq))

(defvar *read-regs* '(nil
                      alu
                      mem
                      arg
                      buf1
                      buf2
                      car
                      s
                      e
                      c
                      d
                      mar
                      x1
                      x2
                      free
                      parent
                      root
                      y1
                      y2
                      num
                      nilx
                      true
                      false
                      cons
							 r))

(defvar *write-regs* '(nil
                       bidir
                       arg
                       buf1
                       buf2
                       car
                       s
                       e
                       c
                       d
                       mar
                       x1
                       x2
                       free
                       parent
                       root
                       y1
                       y2
							  r))

(defvar *alu-ops* '(nil
                    dec
                    add
                    sub
                    mul
                    div
                    rem
                    set-mark
                    set-field
                    clear-mark
                    replcar
                    replcdr
                    clear-field
                    gcreverse
                    gcreset
                    gcmark
                    ;; flag status bit manipulation pseudo-ops
                    running
                    halted
                    gc
						  external
						  fail
						  forkreq
						  ))

