(defpackage :rosalind
  (:use :cl :iterate :losh)
  (:import-from :1am :is)
  (:import-from :alexandria
    :curry :rcurry :compose
    :ensure-gethash
    :with-gensyms :once-only :symbolicate)
  (:shadowing-import-from :1am :test)
  (:export :run-tests))
