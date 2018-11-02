(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :rcurry
               :with-gensyms
               :symb

               )
  :package "ROSALIND.QUICKUTILS")
