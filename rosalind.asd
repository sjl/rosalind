(asdf:defsystem :rosalind
  :name "rosalind"
  :description "Rosalind solutions."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (

               :1am
               :alexandria
               :iterate
               :losh
               :str

               )

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "utils")
                             (:module "problems"
                              :components ((:file "dna")
                                           (:file "rna")
                                           (:file "revc")
                                           (:file "gc")
                                           (:file "hamm")
                                           (:file "prot")
                                           (:file "perm")
                                           (:file "fib")
                                           (:file "subs")
                                           (:file "iprb")
                                           (:file "iev")
                                           (:file "fibd")
                                           (:file "cons")))))))

