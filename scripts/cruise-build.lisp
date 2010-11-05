(in-package :cl-user)

(require "ASDF")

(pushnew (merge-pathnames ".nkzs-reg/asd/"
                          (user-homedir-pathname))
         asdf:*central-registry*
         :test #'equal)

(pushnew (merge-pathnames ".clnk/asd/"
                          (user-homedir-pathname))
         asdf:*central-registry*
         :test #'equal)

(require 'arnesi)

(asdf:oos 'arnesi:clean-op :musicxml)

(require 'musicxml)

(assert (test:run-tests))
