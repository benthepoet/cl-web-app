#|
  This file is a part of cl-web-app
  (c) 2019 Ben Hanna (benpaulhanna@gmail.com)
  Author: Ben Hanna <benpaulhanna@gmail.com>
|#

(defpackage #:cl-web-app
  (:use #:cl
        #:cl-who
        #:hunchentoot
        #:ironclad
        #:sqlite)
  (:export #:start-server
           #:setup-database
           #:auth-user
           #:create-user
           #:home-page
           #:login-page
           #:logout-page
           #:with-database
           #:with-layout))

(in-package #:cl-web-app)

(defmacro with-database (db &rest body)
  `(sqlite:with-open-database (,db "data.sqlite")
     ,@body))

(defmacro with-layout (&rest body)
  `(cl-who:with-html-output-to-string (output)
     (:html
      (:body ,@body))))

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (home-page)
  (login-page)
  (logout-page))

(defun setup-database ()
  (with-database db
    (sqlite:execute-non-query db "CREATE TABLE user (id integer primary key not null, email text not null, password text not null)")))

(defun home-page ()
  (hunchentoot:define-easy-handler (home :uri "/") ()
    (with-layout
      (:h4 "Home"))))

(defun login-page ()
  (hunchentoot:define-easy-handler (login :uri "/login") ((email :request-type :post)
                                                               (password :request-type :post))
    (let ((message nil))
      (case (hunchentoot:request-method*)
        (:post
         (if (auth-user email password)
             (progn
               (setf message "Login successful.")
               (hunchentoot:start-session))
             (setf message "Login failed."))))
      (with-layout
          (:h4 "Login")
          (:div 
             (:form :method "post"
                (:input :type "email" :name "email")
                (:input :type "password" :name "password")
                (:button :type "submit" "Login")))
        (:p (cl-who:str message))
        (when hunchentoot:*session*
            (cl-who:htm (:p (cl-who:str "Session Active"))))))))

(defun logout-page ()
  (hunchentoot:define-easy-handler (logout :uri "/logout") ()
    (hunchentoot:remove-session hunchentoot:*session*)
    (hunchentoot:redirect "/")))

(defun auth-user (email password)
  (with-database db
    (let ((password-bytes (ironclad:ascii-string-to-byte-array password))
          (hash (sqlite:execute-single db "SELECT password FROM user WHERE email = ?" email)))
      (when hash
        (ironclad:pbkdf2-check-password password-bytes hash)))))

(defun create-user (email password)
  (let* ((password-bytes (ironclad:ascii-string-to-byte-array password))
         (hash (ironclad:pbkdf2-hash-password-to-combined-string password-bytes)))
    (with-database db
      (sqlite:execute-non-query db "INSERT INTO user (email, password) VALUES (?, ?)" email hash))))
