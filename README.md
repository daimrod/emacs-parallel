<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Emacs Parallel</a></li>
<li><a href="#sec-2">2. HowTo</a></li>
<li><a href="#sec-3">3. Tips &amp; Tricks</a></li>
<li><a href="#sec-4">4. How does it work?</a></li>
<li><a href="#sec-5">5. Known limitations</a></li>
</ul>
</div>
</div>
# Emacs Parallel

Emacs Parallel is yet another library to simulate parallel
computations in Emacs (because it lacks threads support in Elisp).

# STARTED HowTo

You can execute a simple function a retrive the result like this:

    (parallel-get-result (parallel-start (lambda () (* 42 42))))
    ⇒ 1764

Though you won't benefit from the parallelism because
`parallel-get-result` is blocking, that is it waits for the function
to be executed.

So you can use define a callback to be called when the function is
finished:

    (parallel-start (lambda () (sleep-for 4.2) "Hello World")
                    :post-exec (lambda (results _status)
                                 (message (first results))))
    ⊣ Hello World

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
    ⊣ Hello World

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
    ⊣ Received 42

Because the function is executed in another Emacs instance (in Batch
Mode by default), the environment isn't the same. However you can
send some data with the `env` parameter:

    (let ((a 42)
          (b 12))
      (parallel-get-result (parallel-start (lambda (a b) (+ a b))
                                           :env (list a b))))
    ⇒ 54

# Tips & Tricks

If your windows manager is smart enough (like StumpwWM) you can use
it to move graphical windows (Emacs frames) in another desktop.

For example, I use this to move Emacs frames (with the title
"emacs-debug") to the group (aka desktop) 9:

    (define-frame-preference "9"
      (0 nil t :title "emacs-debug"))  

And this to specify the title of the frame:

    (parallel-start (lambda () 42)
                    :no-batch t
                    :emacs-args '("-T" "emacs-debug"))

# TODO How does it work?

# Known limitations

You can only send data to the remote (with the `env` parameter) or
from the remote (with `parallel-send`) that have a printed
representation (see [<elisp#Printed> Representation](elisp#Printed Representation)).

So you can pass around numbers, symbols, strings, lists, vectors,
hash-table but you can't pass buffers, windows, frames&#x2026;


It lacks documentation, tests and probably a clean API, but I'm
working on it!
