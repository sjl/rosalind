(defpackage :rosalind
  (:use
    :cl
    :iterate
    :losh
    :rosalind.quickutils)
  (:import-from :1am :is)
  (:shadowing-import-from :1am :test)
  (:export :run-tests))
