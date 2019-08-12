#|
  This file is a part of cl-web-app
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#

(defpackage #:cl-web-app
  (:use #:cl
        #:cl-who
        #:hunchentoot)
  (:export #:start-server
           #:home-page
           #:login-page
           #:with-layout))

(in-package #:cl-web-app)

(defmacro with-layout (&rest body)
  `(cl-who:with-html-output-to-string (output)
     (:html
      (:body ,@body))))

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defun home-page ()
  (hunchentoot:define-easy-handler (home-page :uri "/") ()
    (with-layout
      (:h4 "Home"))))

(defun login-page ()
  (hunchentoot:define-easy-handler (login-page :uri "/login") ()
    (with-layout
      (:h4 "Login")
      (:form :method "post"
             (:input :type "email" :name "email")
             (:input :type "password" :name "password")
             (:button :type "submit" "Login")))))
 
