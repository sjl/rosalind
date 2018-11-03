(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :rcurry
               :with-gensyms
               :once-only
               :symb

               )
  :package "ROSALIND.QUICKUTILS")
