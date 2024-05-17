# lack-middleware-postmodern

## Usage

Wrap app:

```common-lisp
(funcall lack/middleware/postmodern:*lack-middleware-postmodern*
         *app*
         :pools `((:pool-id :mydb
                   :database "mydb"
                   :username "myuser"
                   :host "localhost"
                   :use-binary t
                   :application-name "myapp"
                   :query-log ,(when (developmentp)
                                 *standard-output*)
                   :max-open-count 12
                   :timeout 4000
                   :idle-timeout 30000)))
```

Lack Builder:

```common-lisp
(lack:builder
 (:postmodern :pools `((:pool-id :mydb
                        :database "mydb"
                        :username "myuser"
                        :password nil
                        :host "localhost"
                        :use-binary t
                        :application-name "myapp"
                        :query-log ,(when (developmentp)
                                      *standard-output*)
                        :max-open-count 12
                        :max-idle-count 3
                        :timeout 5000
                        :idle-timeout 40000)
                       (:pool-id :otherdb
                        :database "otherdb"
                        :username "foo"
                        :password "abcxyz"
                        :host "localhost"
                        :use-binary t)))
 *app*)
```

Once you have a pool configured, you can call macro `WITH-POSTMODERN` from your application to checkout a connection from the specified pool:


```common-lisp
(with-postmodern (:mydb)
  (assert (= 1 (pomo:query "select 1" :single))))
```

### Pool options

* `POOL-ID`: required; unique name of the pool, a keyword
* `POOL-NAME`: optional; passed through to the `ANYPOOL:MAKE-POOL` as the `NAME` parameter, defaulting to "postmodern-[downcased-pool-id]" if not provided
* `MAX-OPEN-COUNT`: optional; passed through to `ANYPOOL:MAKE-POOL` (default: 10)
* `MAX-IDLE-COUNT`: optional; passed through to `ANYPOOL:MAKE-POOL` (default 3)
* `TIMEOUT`: optional; passed through to `ANYPOOL:MAKE-POOL` (default: 3000)
* `IDLE-TIMEOUT`: optional; passed through to `ANYPOOL:MAKE-POOL` (default: 60000)
* `PING`: optional; allows providing a custom ping function to `ANYPOOL:MAKE-POOL`. If you provide your own ping function, this is not passed through to `ANYPOOL:MAKE-POOL` directly, instead it binds `POSTMODERN:*DATABASE*` for you and then calls the provided function
* `DATABASE`: required; the name of the PostgreSQL database
* `USERNAME`: required; the name of the PostgreSQL user
* `PASSWORD`: optional; the password for the PostgreSQL user
* `HOST`: required; the PostgreSQL host
* `PORT`: optional; the PostgreSQL port, passed through to `POSTMODERN:CONNECT` keyword arguments when non-nil (default: 5432)
* `USE-SSL`: optional; passed through to `POSTMODERN:CONNECT` keyword arguments when non-NIL
* `USE-BINARY`: optional; passed through to `POSTMODERN:CONNECT` keyword arguments when non-NIL
* `SERVICE`: optional; passed through to `POSTMODERN:CONNECT` keyword arguments when non-NIL
* `APPLICATION-NAME`: optional; passed through to `POSTMODERN:CONNECT` keyword arguments when non-NIL
* `QUERY-LOG`: optional; `POSTMODERN::*QUERY-LOG*` is bound to the provided value
* `SCHEMA-PATH`: optional; `POSTMODERN::*SCHEMA-PATH*` is bound to the provided value
* `SSL-CERTIFICATE-FILE`: optional; `CL-POSTGRES:*SSL-CERTIFICATE-FILE*` is bound to the provided value
* `SSL-KEY-FILE`: optional; `CL-POSTGRES:*SSL-KEY-FILE*` is bound to the provided value
* `SSL-ROOT-CA-FILE`: optional; `CL-POSTGRES:*SSL-ROOT-CA-FILE*`is bound to the provided value
* `ON-TOO-MANY-OPEN-CONNECTIONS`: optional; function called from `HANDLER-BIND` handler for `ANYPOOL:TOO-MANY-OPEN-CONNECTION` error before returning a 503 response; may be useful for sending a notification or some other purpose

See the [Postmodern documentation](https://marijnhaverbeke.nl/postmodern/postmodern.html) for further details related to the connection parameters. Also you can check out the [source](https://github.com/marijnh/Postmodern/blob/805f033974dcd5313091bb99e73f7a98b5892c56/postmodern/connect.lisp#L17) for function `POSTMODERN:CONNECT` to see the default values for the keyword parameters.

## Dependencies

### Middleware

* [anypool](https://github.com/fukamachi/anypool)
* [Postmodern](https://github.com/marijnh/Postmodern)

### Tests

* [rove](https://github.com/fukamachi/rove)
* [bordeaux-threads](https://github.com/sionescu/bordeaux-threads)

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-middleware-postmodern)
```

The tests assume that the "postgres" superuser is configured for passwordless authentication.

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
