# Emacs Parallel

Emacs Parallel is yet another library to simulate parallel
computations in Emacs (because it lacks threads support in Elisp).

# STARTED HowTo

You can execute a simple function a retrive the result like this:

    (parallel-get-result (parallel-start (lambda () (* 42 42))))

Though you won't benefit from the parallelism because
`parallel-get-result` is blocking, that is it waits for the function
to be executed.

So you can use define a callback to be called when the function is
finished:

    (parallel-start (lambda () (sleep-for 4.2) "Hello World")
                    :post-exec (lambda (results _status)
                                 (message (first results))))

Here, why `(first results)` and not `result`? Because you can send
data from the remote instance while it's running with
`parallel-send`:

    (parallel-start (lambda ()
                      (parallel-send "Hello")
                      (sleep-for 4.2)
                      "World")
                    :post-exec (lambda (results _status)
                                 (message "%s"
                                          (mapconcat #'identity (reverse results) " "))))

As you may have noticed the results are pushed in a list, so the
first element is the result returned by the function called, the
second is the last piece of data send, and so on&#x2026;

And of course you can execute some code when you receive data from
the remote instance:

    (parallel-start (lambda ()
                      (parallel-send 42)
                      (sleep-for 4.2)         ; heavy computation to compute PI
                      pi)
                    :on-event (lambda (data)
                                (message "Received %S" data)))

Because the function is executed in another Emacs instance (in Batch
Mode by default), the environment isn't the same. However you can
send some data with the `env` parameter:

    (let ((a 42)
          (b 12))
      (parallel-get-result (parallel-start (lambda (a b) (+ a b))
                                           :env (list a b))))

# TODO How does it work?

# Known limitations

You can only send data to the remote (with the `env` parameter) or
from the remote (with `parallel-send`) that have a printed
representation (see [<elisp#Printed> Representation](elisp#Printed Representation)).

So you can pass around numbers, symbols, strings, lists, vectors,
hash-table but you can't pass buffers, windows, frames&#x2026;


It lacks documentation, tests and probably a clean API, but I'm
working on it!
