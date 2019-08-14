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
      (:head
       (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.css"))
      (:body :style "padding-top: 32px; background-color: #eee;"
       (:div :class "columns" ,@body)))))

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
          (:div :class "column")
          (:div :class "column is-narrow"
                (:div :class "card" :style "width: 400px;"
                      (:div :class "card-content"
                            (:h2 :class "subtitle is-4 has-text-centered" "Log in to your account")
                            (when message
                              (cl-who:htm (:article :class "message is-danger has-text-centered"
                                                    (:div :class "message-body" :style "padding: 0.5em;" (cl-who:str message)))))
                            (:form :method "post"
                                   (:div :class "field"
                                         (:p :class "control"
                                             (:input :class "input" :placeholder "Email" :type "email" :name "email")))
                                   (:div :class "field"
                                         (:p :class "control"
                                             (:input :class "input" :placeholder "Password" :type "password" :name "password")))
                                   (:button :class "button is-link" :style "width: 100%;" :type "submit" "Login")))))
        (:div :class "column")
        ))))

(defun logout-page ()
  (hunchentoot:define-easy-handler (logout :uri "/logout") ()
    (when hunchentoot:*session*
      (hunchentoot:remove-session hunchentoot:*session*))
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
