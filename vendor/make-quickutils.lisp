(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :rcurry
               :with-gensyms
               :read-file-into-string
               :symb

               )
  :package "ROSALIND.QUICKUTILS")
