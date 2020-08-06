;; to load the microcode workbench
(asdf:oos 'asdf:load-op :secd-tools)

(in-package :secd-tools)

;; assuming that you are chdir'd to the secd-tools directory:

;; to assemble the microcode file:
(assemble-microcode "../secd-microcode.txt")

;; to set up a ram image for the simulator:
(write-vhdl-ram "../lispkit/LKIT-2/NQUEEN.LKC" 6 "../vhdl/secd_ram_defs.vhd")

;; to write a memory dump that can be loaded into the SECD machine (via a ram
;; shared with a front system)
(write-shared-ram "../lispkit/LKIT-2/NQUEEN.LKC" 6 "nqueens.bin")

;; ... and to produce a symbol table. This will allow us to render the symbols
;; with their original names, when reading the result from the shared memory.
;; Here the compiler will compile itself.
(write-shared-ram "../lispkit/LKIT-2/LISPKIT.LKC" "../lispkit/LKIT-2/LISPKIT.LKS" "lispkit.bin" "lispkit.sym")
