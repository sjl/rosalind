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
               :cl-digraph
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
                              :components (

                                           (:file "cons")
                                           (:file "dna")
                                           (:file "fib")
                                           (:file "fibd")
                                           (:file "gc")
                                           (:file "grph")
                                           (:file "hamm")
                                           (:file "iev")
                                           (:file "iprb")
                                           (:file "lcsm")
                                           (:file "lia")
                                           (:file "mrna")
                                           (:file "perm")
                                           (:file "prot")
                                           (:file "prtm")
                                           (:file "revc")
                                           (:file "rna")
                                           (:file "splc")
                                           (:file "subs")

                                           ))))))

