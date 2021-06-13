;; -*- Lisp -*-

(defsystem :sim
  :name "SECD Microcode Simulator"
  :description "Microcode simulator for the SECD microprocessor"
  :author ("Hans Hübner <hans.huebner@gmail.com>" "Viandant <viandant@etheory.de>")
  :licence "BSD"
  :depends-on ("secd-tools")
  :components ((:file "sim-alu")
               (:file "sim-microcode" :depends-on ("sim-alu"))
               (:file "sim" :depends-on ("sim-microcode")))
  )
  
