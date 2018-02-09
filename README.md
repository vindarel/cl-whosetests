whosconnected

Personal tries for an interactive prompt.

We use [replic](https://github.com/vindarel/replic) to ease the building of the readline repl.

```common-lisp
(setf replic:*prompt* (cl-ansi-text:green "whosconnected > "))

;; Create the completions bindings.
(replic:init-completions)

;; create commands from the exported functions and variables.
(replic:functions-to-commands :clwhosconnected)

;; run the repl
(replic:repl))
```

Lessons learned transfered to:

* https://lispcookbook.github.io/cl-cookbook/web-scraping.html
* https://lispcookbook.github.io/cl-cookbook/scripting.html
* https://github.com/vindarel/replic (inheriting and factoring ideas from here, to build custom readline programs)
* https://github.com/vindarel/cl-readline-example

We use [cl21](https://lispcookbook.github.io/cl-cookbook/cl21.html).

Specially cool for quick lambdas:

~~~lisp
(defun titles2url (titles)
  "From a list of user names, give back the full url."
  (map ^(str:concat *base-url* %) titles))
~~~

and [str](https://github.com/vindarel/cl-str).
