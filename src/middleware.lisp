;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/postmodern)

(defvar *postmodern-pools* nil
  "Property list containing configured connection pools; dynamically bound.")

(define-condition pool-not-defined-error (error)
  ((pool-id :initarg :pool-id))
  (:report (lambda (condition stream)
             (with-slots (pool-id)
                 condition
               (format stream "No Postmodern pool defined for pool-id: ~A" pool-id))))
  (:documentation "Error signalled when attempting to connect to an unknown pool
from WITH-POSTMODERN."))

(defmacro with-postmodern ((pool-id) &body body)
  "Checks out a connection from the pool POOL-ID, a keyword.
POSTMODERN:*DATABASE*, the connection object, is dynamically bound in BODY."
  (let ((gensym-pool-id (gensym "pool-id")))
    `(let* ((,gensym-pool-id ,pool-id)
            (pool-info (getf lack/middleware/postmodern:*postmodern-pools* ,gensym-pool-id)))
       (declare (type (or null lack/middleware/postmodern::pool-info) pool-info))
       (or pool-info (error 'pool-not-defined-error :pool-id ,gensym-pool-id))
       (with-slots (pool schema-path query-log ssl-certificate-file ssl-key-file ssl-root-ca-file
                    on-too-many-open-connections)
           pool-info
         (let ((postmodern::*schema-path* schema-path)
               (postmodern::*query-log* query-log)
               (cl-postgres:*ssl-certificate-file* ssl-certificate-file)
               (cl-postgres:*ssl-key-file* ssl-key-file)
               (cl-postgres:*ssl-root-ca-file* ssl-root-ca-file))
           (block nil
             (handler-bind ((anypool:too-many-open-connection
                              (lambda (condition)
                                (and on-too-many-open-connections
                                     (funcall on-too-many-open-connections condition))
                                (return
                                  `(503
                                    (:content-type "text/plain"
                                     :content-length 19)
                                    ("Service Unavailable"))))))
               (anypool:with-connection (postmodern:*database* pool)
                 ,@body))))))))

(defclass pool-info ()
  ((pool :reader pool
         :initarg :pool)
   (query-log :reader query-log
              :initarg :query-log)
   (schema-path :reader schema-path
                :initarg :schema-path)
   (ssl-certificate-file :reader ssl-certificate-file
                         :initarg :ssl-certificate-file)
   (ssl-key-file :reader ssl-key-file
                 :initarg :ssl-key-file)
   (ssl-root-ca-file :reader ssl-root-ca-file
                     :initarg :ssl-root-ca-file)
   (on-too-many-open-connections :reader on-too-many-open-connections
                                 :initarg :on-too-many-open-connections
                                 :type (or null function))))

(defparameter *lack-middleware-postmodern*
  (lambda (app &key pools)
    (declare (optimize (speed 0) (safety 3) (debug 3))
             (type function app))
    (check-type pools list)
    (assert (not (null pools))
            nil
            "POOLS cannot be empty - define at least one pool.")
    (let ((pool-infos ()))
      (declare (type list pool-infos))
      (dolist (pool-spec pools)
        (let ((pool-id (getf pool-spec :pool-id)))
          (check-type pool-id keyword)
          (when (getf *postmodern-pools* pool-id)
            (error "Non-unique POOL-ID: ~A" pool-id))
          (let ((pool-info
                  (destructuring-bind (&key pool-id
                                         pool-name
                                         database
                                         username
                                         password
                                         host
                                         port
                                         use-ssl
                                         use-binary
                                         service
                                         application-name
                                         query-log
                                         schema-path
                                         ssl-certificate-file
                                         ssl-key-file
                                         ssl-root-ca-file
                                         (max-open-count 10)
                                         (max-idle-count 3)
                                         (timeout 3000)
                                         (idle-timeout 60000)
                                         ping
                                         on-too-many-open-connections)
                      pool-spec
                    (declare (ignore port use-ssl use-binary service application-name)
                             (ignore max-open-count max-idle-count timeout idle-timeout))
                    (check-type pool-name (or null string))
                    (check-type ping (or null function))
                    (check-type on-too-many-open-connections (or null function))
                    (let* ((connect-kwargs
                             (loop for (key value) on pool-spec by #'cddr
                                   append (when (and (member key '(:port
                                                                   :use-ssl
                                                                   :use-binary
                                                                   :service
                                                                   :application-name))
                                                     value)
                                            (list key value))))
                           (pool-options
                             (loop for (key value) on pool-spec by #'cddr
                                   when (and (member key '(:max-open-count
                                                           :max-idle-count
                                                           :timeout
                                                           :idle-timeout))
                                             value)
                                     append (list key value)))
                           (pool (apply #'anypool:make-pool
                                        (append
                                         (list
                                          :name (or pool-name (format nil "postmodern-~A"
                                                                      (string-downcase pool-id)))
                                          :connector (lambda ()
                                                       (apply #'postmodern:connect
                                                              `(,database
                                                                ,username
                                                                ,password
                                                                ,host
                                                                ,@connect-kwargs)))
                                          :disconnector #'postmodern:disconnect
                                          :ping (lambda (connection)
                                                  (declare (type postmodern:database-connection
                                                                 connection))
                                                  (let ((postmodern:*database* connection))
                                                    (declare (type postmodern:database-connection
                                                                   postmodern:*database*))
                                                    (if ping
                                                        (funcall ping)
                                                        (postmodern:execute "select 1")))))
                                         pool-options))))
                      (make-instance 'pool-info
                                     :pool pool
                                     :query-log query-log
                                     :schema-path schema-path
                                     :ssl-certificate-file ssl-certificate-file
                                     :ssl-key-file ssl-key-file
                                     :ssl-root-ca-file ssl-root-ca-file
                                     :on-too-many-open-connections on-too-many-open-connections)))))
            (setf (getf pool-infos pool-id)
                  pool-info))))
      (lambda (env)
        (declare (optimize (speed 3) (safety 0) (debug 0))
                 (type list env))
        (let ((*postmodern-pools* pool-infos))
          (declare (type list *postmodern-pools*))
          (funcall app env)))))
  "Middleware to provide Postmodern connection pools, dynamically bound to *POSTMODERN-POOLS*.")
