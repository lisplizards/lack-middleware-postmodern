;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/postmodern/tests)

;; N.B. Tests assume that superuser "postgres" is configured for passwordless authentication.

(deftest middleware
    (testing "create database, run query, drop database"
             (flet ((app (env)
                      (declare (ignore env))
                      (lack/middleware/postmodern:with-postmodern (:middleware-test)
                        (ok (not (null pomo:*database*)))
                        (ok (eq 'pomo::database-connection (type-of pomo:*database*)))
                        (ok (pomo:connected-p pomo:*database*))
                        (when (pomo:database-exists-p "lack_middleware_postmodern_test")
                          (pomo:drop-database "lack_middleware_postmodern_test"))
                        (pomo:create-database 'lack-middleware-postmodern-test
                                              :limit-public-access t
                                              :comment "This database is used to test CL package LACK/MIDDLEWARE/POSTMODERN")
                        (ok (pomo:database-exists-p "lack_middleware_postmodern_test"))
                        (ok (= 1 (pomo:query "select 1" :single)))
                        (pomo:drop-database 'lack-middleware-postmodern-test)
                        (ng (pomo:database-exists-p "lack_middleware_postmodern_test"))
                        `(200
                          (:content-type "text/plain"
                           :content-length 13)
                          ("Hello, World.")))))
               (let ((app (funcall lack/middleware/postmodern:*lack-middleware-postmodern*
                                   #'app
                                   :pools `((:pool-id :middleware-test
                                             :query-log ,*standard-output*
                                             :database "postgres"
                                             :username "postgres"
                                             :password nil
                                             :host "localhost"
                                             :use-ssl :no
                                             :use-binary t
                                             :application-name "postmodern-lack-middleware-test"
                                             :max-open-count 3)))))
                 (funcall app ()))))

  (testing "signals POOL-NOT-DEFINED-ERROR when an unknown pool is given to WITH-POSTMODERN"
           (flet ((app (env)
                    (declare (ignore env))
                    (lack/middleware/postmodern:with-postmodern (:foo)
                      `(200
                        (:content-type "text/plain"
                         :content-length 13)
                        ("Hello, World.")))))
             (let ((app (funcall lack/middleware/postmodern:*lack-middleware-postmodern*
                                 #'app
                                 :pools '((:pool-id :middleware-test
                                           :database "postgres"
                                           :username "postgres"
                                           :password nil
                                           :host "localhost"
                                           :use-ssl :no
                                           :use-binary t
                                           :application-name "postmodern-lack-middleware-test"
                                           :max-open-count 3)))))
               (ok (signals (funcall app ())
                            'lack/middleware/postmodern:pool-not-defined-error)))))

  (testing "returns a 503 response when the pool is exhausted and no more pool availability within timeout limit (timeout: 0)"
           (let ((failure-count 0))
             (flet ((app (env)
                      (declare (ignore env))
                      (lack/middleware/postmodern:with-postmodern (:middleware-test)
                        (sleep 3)
                        `(200
                          (:content-type "text/plain"
                           :content-length 13)
                          ("Hello, World.")))))
               (let ((app (funcall lack/middleware/postmodern:*lack-middleware-postmodern*
                                   #'app
                                   :pools '((:pool-id :middleware-test
                                             :query-log nil
                                             :database "postgres"
                                             :username "postgres"
                                             :password nil
                                             :host "localhost"
                                             :use-ssl :no
                                             :use-binary t
                                             :application-name "postmodern-lack-middleware-test"
                                             :max-open-count 2
                                             :timeout 0
                                             :max-idle-count 3)))))
                 (flet ((make-thread ()
                          (bt2:make-thread
                           (lambda ()
                             (let ((response (funcall app ())))
                               (when (= 503 (first response))
                                 (incf failure-count))
                               response)))))
                   (let ((threads (list (make-thread)
                                        (make-thread)
                                        (make-thread)
                                        (make-thread)
                                        (make-thread))))
                     (dolist (thread threads)
                       (bt2:join-thread thread))
                     (ok (= 3 failure-count))))))))

  (testing "does NOT return 503 response when the pool is exhausted but pool has availability within timeout limit"
           (let ((failure-count 0))
             (flet ((app (env)
                      (declare (ignore env))
                      (lack/middleware/postmodern:with-postmodern (:middleware-test)
                        (sleep 3)
                        `(200
                          (:content-type "text/plain"
                           :content-length 13)
                          ("Hello, World.")))))
               (let ((app (funcall lack/middleware/postmodern:*lack-middleware-postmodern*
                                   #'app
                                   :pools `((:pool-id :middleware-test
                                             :query-log nil
                                             :database "postgres"
                                             :username "postgres"
                                             :password nil
                                             :host "localhost"
                                             :use-ssl :no
                                             :use-binary t
                                             :application-name "postmodern-lack-middleware-test"
                                             :max-open-count 2
                                             :timeout 10000
                                             :max-idle-count 3)))))
                 (flet ((make-thread ()
                          (bt2:make-thread
                           (lambda ()
                             (let ((response (funcall app ())))
                               (when (= 503 (first response))
                                 (incf failure-count))
                               response)))))
                   (let ((threads (list (make-thread)
                                        (make-thread)
                                        (make-thread)
                                        (make-thread)
                                        (make-thread))))
                     (dolist (thread threads)
                       (bt2:join-thread thread))
                     (ok (= 0 failure-count))))))))

  (testing "create database, run query, drop database, from two separate pools, with nesting"
           (flet ((app (env)
                    (declare (ignore env))
                    (lack/middleware/postmodern:with-postmodern (:pool-1)
                      (let ((pool-1-pomo-database pomo:*database*))
                        (ok (not (null pomo:*database*)))
                        (ok (eq 'pomo::database-connection (type-of pomo:*database*)))
                        (ok (pomo:connected-p pomo:*database*))
                        (when (pomo:database-exists-p "lack_middleware_postmodern_test_db1")
                          (pomo:drop-database "lack_middleware_postmodern_test_db1"))
                        (pomo:create-database 'lack-middleware-postmodern-test-db1
                                              :limit-public-access t
                                              :comment "This database is used to test CL package LACK/MIDDLEWARE/POSTMODERN")
                        (ok (pomo:database-exists-p "lack_middleware_postmodern_test_db1"))
                        (ok (= 1 (pomo:query "select 1" :single)))
                        (pomo:drop-database 'lack-middleware-postmodern-test-db1)
                        (ng (pomo:database-exists-p "lack_middleware_postmodern_test_db1"))
                        (lack/middleware/postmodern:with-postmodern (:pool-2)
                          (ok (not (null pomo:*database*)))
                          (ok (eq 'pomo::database-connection (type-of pomo:*database*)))
                          (ok (not (eq pool-1-pomo-database pomo:*database*)))
                          (ok (pomo:connected-p pomo:*database*))
                          (when (pomo:database-exists-p "lack_middleware_postmodern_test_db2")
                            (pomo:drop-database "lack_middleware_postmodern_test_db2"))
                          (pomo:create-database 'lack-middleware-postmodern-test-db2
                                                :limit-public-access t
                                                :comment "This database is used to test CL package LACK/MIDDLEWARE/POSTMODERN")
                          (ok (pomo:database-exists-p "lack_middleware_postmodern_test_db2"))
                          (ok (= 1 (pomo:query "select 1" :single)))
                          (pomo:drop-database 'lack-middleware-postmodern-test-db2)
                          (ng (pomo:database-exists-p "lack_middleware_postmodern_test_db2")))
                        `(200
                          (:content-type "text/plain"
                           :content-length 13)
                          ("Hello, World."))))))
             (let ((app (funcall lack/middleware/postmodern:*lack-middleware-postmodern*
                                 #'app
                                 :pools '((:pool-id :pool-1
                                           :database "postgres"
                                           :username "postgres"
                                           :password nil
                                           :host "localhost"
                                           :use-ssl :no
                                           :use-binary t
                                           :application-name "postmodern-lack-middleware-test"
                                           :max-open-count 2
                                           :timeout 0
                                           :max-idle-count 3)
                                          (:pool-id :pool-2
                                           :database "postgres"
                                           :username "postgres"
                                           :password nil
                                           :host "localhost"
                                           :use-ssl :no
                                           :use-binary t
                                           :application-name "postmodern-lack-middleware-test"
                                           :max-open-count 2
                                           :timeout 0
                                           :max-idle-count 3)))))
               (funcall app ())))))
