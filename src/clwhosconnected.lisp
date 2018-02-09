(in-package :cl21-user)
(defpackage clwhosconnected
  (:use :cl21)
  (:shadow :open)
  (:import-from :alexandria
                :if-let)
  (:export :print-name
           :print-connected
           :watched?
           :*watchlist*
           :get-all-connected
           :names
           :mfind
           :open
           :version
           :view
           :main
           ))
(in-package :clwhosconnected)
(annot:enable-annot-syntax)

(defconstant +version+ 0.3)

(defparameter *user-agent* "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")

(defparameter *base-url* ""
  "Valid base url.")

(defvar *page-suffix* "" "what to add to the main url to browse pages.")
(setq *page-suffix* "?page=")

(defun get-pages (&optional (base-url *base-url*) (suffix *page-suffix*))
  (map ^(str:concat base-url suffix %) '("1" "2" "3" "4" "5" "6" "7")))


(defvar *css-selector* "" "CSS selector to find users on the *base-url* domain.")

(setf *css-selector* ".title a")


(defparameter *url* ""
  "url from which is started a search. Should be base-url + suffix.")

(defparameter *watchlist* '() "list of people to watch. Add in init.el.")

(defparameter *data-file* (asdf:system-relative-pathname :clwhosconnected  "data.txt")
  "the file name in which are stored lines of things to watch.")

(defparameter *stats* #H())

(defparameter *all-connected* nil
  "list of connected names. Is set after a search by get-all-connected.")


(defun watchlist-names (&optional (watchlist *watchlist*))
  "List items can be a string or a cons cell with an alist of properties."
  (sort (map ^(if (consp %) (car %) %) watchlist)
        #'string<=))

(defun names ()
  (format t "~a~&" (watchlist-names)))

(defun titles2url (titles)
  "From a list of user names, give back the full url."
  (map ^(str:concat *BASE-URL* %) titles))

(defun name2url (name)
  "From a name, return the url."
  (str:concat *base-url* name))

(defvar *connected* nil)

(defun get-url (url)
  (dex:get url
           :headers '(("user-agent" . *user-agent*))
           :verbose nil))

(defun get-titles (url)
  (let (request soup names utime)
    (format t "requesting ~A...\n" url)
    (setf request (get-url url))
    (format t "...requested ~A" url)
    (setf soup (plump:parse request))
    (setf names (lquery:$ soup *css-selector* (text)))
    (setf names (map ^(str:trim %) names))
    (setq *connected* names)
    (setf utime (get-universal-time))
    (coerce names 'list)))

(defun get-all-titles ()
  (let (titles)
    (setf titles (lparallel:pmap 'list #'get-titles (GET-PAGES)))
    (flatten (concatenate 'list titles))))

(defun watched? (name)
  ;; (princ (find name *watchlist* :test 'equal :key ^(if (consp %) (car %) %)))
  (find name *watchlist* :test 'equal :key ^(if (consp %) (car %) %)))

(defun print-name (str-or-cons &key (stream t))
  (if (consp str-or-cons)
      (progn
        (format stream "~A: " (car str-or-cons))
        ;; todo map on keys
        (format stream "~{~a~^, ~}~%" (cdr (assoc :keywords (cdr str-or-cons)))))
      (format stream "~a~%" str-or-cons)))

(defun print-connected (names)
  (let* ((watched (map ^(watched? %) names))
         (watched (remove nil watched)))
    (format t "to print: ~A~%" watched)
    (format t "----------~%")
    (map ^(print-name %) watched)
    t))

(defun get-all-connected (&optional names)
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel 4)))
  (let ((names (or names (get-all-titles))))
    (if-let (connected (intersection names (watchlist-names *watchlist*) :test 'equal))
      (progn
        (princ "Connected:\n")
        (print-connected connected)
        (map ^(format t "~A\n" %) (titles2url connected))
        (setf *all-connected* connected)
        connected))))

(defun get-connected-ones (&optional (watchlist *watchlist*) (url *url*))
  (format t "searching again...\n")
  (let* ((soup (plump:parse (dex:get url)))
         (titles (lquery:$ soup ".title a" (text)))
         (titles (map ^ (str:trim %) titles))
         (titles (progn
                   ;; (format t "found: ~A" titles)
                   (setq *connected* titles)
                   (coerce titles 'list)))
         (titles (intersection titles watchlist :test 'equal)))
    (format t "Connected:\n")
    (map ^(format t "~A\n" %) (titles2url titles))
    titles))

(defun name-from-url (it)
  (if (str:starts-with? "http" it)
      (progn
        (last (str:split "/" (quri:uri-path (quri:uri it))
                         :omit-nulls t)))
      it))

(defun data-lines ()
  "Read config file, return a list of lines."
  (let* ((lines (uiop:read-file-lines *data-file*)))
    ;; rm comments
    (map ^(str:trim  (car (str:split ";" %)))
         lines)))

(defun data-words ()
  "Read config file, return list of list of words."
  (map ^(str:words %)
       (data-lines)))

(defun build-data-watchlist ()
  (map ^(progn
          (if (= 1 (length %))
              (name-from-url (car %))
              `(,(name-from-url (car %)) . ((:keywords . ,(cdr %))))))
       (data-words)))

(defun add-data-watchlist ()
  "Add items listed in `*data-file*' into the `*watchlist*'."
  (setf *watchlist* (append (build-data-watchlist) *watchlist*)))

(defparameter *data-directory* nil
  "The directory containing our files.")

(defun mfind (str)
  "Find for files matching `.*str.*` (inside the data repository)."
  ;; xxx recursively
  (format t "*data-directory*: ~a~&" *data-directory*)
  (let ((res (remove-if-not (lambda (it)
                              (str:contains? str (namestring it)))
                            (osicat:list-directory *data-directory*))))
    (format t "~a~&" res)
    res))

(defun open (name)
  "Open name with web browser. Complete with the connected names, not all."
  (uiop:run-program (list "firefox" (name2url name))))

(defun view (str)
  (let* ((res (mfind str))
         (res (map ^(namestring (truename %)) res)))
    (uiop:run-program (list "mpv" (first res)))))

;;
;; REPL
;;
;; path relative to this package. Can and should change for proper use.
(defparameter *init* (asdf:system-relative-pathname :clwhosconnected "init.lisp")
  "Path to the init file.")

(defun load-init (&key verbose print)
  "Load the config file.

   It must start with
   (in-package :clwhosconnected)
   "
  (format t "loading ~a: ~a~&" *init* (load *init* :verbose verbose :print print)))

;TODO: this is a list for custom complete. We might use replic's completion system.
(defparameter *commands* '(
                           ("search" . "search all")
                           ("list" . "list candidates")
                           ("get-all-connected" . "search all")
                           ("version" . "print current version")
                           ("open" . "open argument in browser")
                           ("mfind" . "find files matching ARG with wildcards.")
                           ("view" . "open the matching files with mpv")
                           ("names" . "show all names")
                           )
  )

(defparameter *verbs* (map #'first *commands*)
  "verbs of the prompt. Symbols.")

(defun common-prefix (items)
  ;; tmp waiting for cl-str 0.5 in Quicklisp february.
  "Find the common prefix between strings.

   Uses the built-in `mismatch', that returns the position at which
   the strings fail to match.

   Example: `(str:common-prefix '(\"foobar\" \"foozz\"))` => \"foo\"

   - items: list of strings
   - Return: a string.

  "
  ;; thanks koji-kojiro/cl-repl
  (when items (subseq
               (car items)
               0
               (apply
                #'min
                (map
                 ^(or (mismatch (car items) %) (length %))
                 (cdr items))))))

(defun select-completions (text list)
  "Select all verbs from `list' that start with `text'."
  (let ((els (remove-if-not (alexandria:curry #'str:starts-with? text)
                            list)))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))

(defun custom-complete (text start end)
  "Complete a symbol.

  text is the partially entered word. start and end are the position on `rl:*line-buffer*'.

  When the cursor is at the beginning of the prompt, complete commands.
  Otherwise, complete names.
  "
  (declare (ignore end))
  (let ((list-names))
    (setf list-names (if (string= "open" (first (str:words rl:*line-buffer*)))
                         *all-connected*
                         (watchlist-names)))
    (if (zerop start)
        (select-completions text *verbs*)
        (select-completions text list-names))))

(defun repl-help ()
  (map ^(format t "~10a-~t ~a~&" (car %) (cdr %))
       *commands*))

(defun version ()
  (format t "~a~&" +version+))


;; command-line.

(defun handle-parser-error (c)
  (format t "cli args parser error: ~a~&" (opts:option c)))

(defun main ()
  (setf lparallel:*kernel* (lparallel:make-kernel 4))
  (load-init)
  (add-data-watchlist)


  (opts:define-opts
    (:name :interactive
           :description "Enter interactive prompt."
           :short #\i
           :long "interactive"))

  (multiple-value-bind (options)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (if (getf options :interactive)
        (progn

          ;; Build our repl commands, help and repl goodies with replic.

          (setf replic:*prompt* (cl-ansi-text:green "whosconnected > "))

          ;; Create the completions bindings.
          (replic:init-completions)

          ;; create commands from the exported functions and variables.
          ;; xxx our custom-complete below overrides those commands.
          (replic:functions-to-commands :clwhosconnected)

          ;; use our completion function: always complete names after the verb.
          (replic:repl :custom-complete #'custom-complete))
        (format t "~a~&" (get-all-connected)))))
