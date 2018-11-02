(asdf:defsystem :rosalind
  :name "rosalind"
  :description "Rosalind solutions."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (

               :1am
               :iterate
               :losh

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
                                           (:file "gc")))))))

