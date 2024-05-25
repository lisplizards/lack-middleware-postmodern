;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-postmodern"
  :version "1.0.2"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/lack-middleware-postmodern"
  :bug-tracker "https://github.com/lisplizards/lack-middleware-postmodern/issues"
  :source-control (:git "https://github.com/lisplizards/lack-middleware-postmodern.git")
  :depends-on ("anypool"
               "postmodern")
  :components ((:module "src"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Lack middleware to provide Postmodern connection pools (PostgreSQL)."
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-postmodern/tests"))))

(defsystem "foo.lisp.lack-middleware-postmodern/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("bordeaux-threads"
               "foo.lisp.lack-middleware-postmodern"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "middleware" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-middleware-postmodern"
  :perform (test-op (op c) (symbol-call :rove :run c)))
