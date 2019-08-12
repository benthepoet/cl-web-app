#|
 This file is a part of cl-web-app
 (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
 Author: Ben Hanna <benpaulhanna@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem cl-web-app
  :version "0.0.0"
  :license "BSD-3"
  :author "Ben Hanna <benpaulhanna@gmail.com>"
  :maintainer "Ben Hanna <benpaulhanna@gmail.com>"
  :description "A simple web application in Common Lisp using Hunchentoot."
  :serial T
  :components ((:file "package"))
  :depends-on ())
