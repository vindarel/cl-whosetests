(in-package :cl21-user)
(defpackage clwhosconnected
  (:use :cl21)
  (:import-from :alexandria
                :if-let)
  (:export :print-name
           :print-connected
           :watched?
           :*tracklist*
           :get-all-connected
           :names
           :present
           :mfind
           :browse
           :version
           :view
           :reload
           :main
           :*browser*
           ))
(in-package :clwhosconnected)
(annot:enable-annot-syntax)

(defconstant +version+ 0.3)

(defparameter *user-agent* "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")

(defparameter *browser* "firefox")

(defparameter *base-url* ""
  "Valid base url.")

(defvar *page-suffix* "" "what to add to the main url to open pages.")
(setq *page-suffix* "?page=")

(defun get-pages (&optional (base-url *base-url*) (suffix *page-suffix*))
  (when (str:blank? base-url)
    (format t "Did you forget to initialize ? use (reload)."))
  (assert (not (str:blank? base-url)))
  (map ^(str:concat base-url suffix %)
       (loop for i upto 10 collect (format nil "~a" i))))


(defvar *css-selector* "" "CSS selector to find users on the *base-url* domain.")

(setf *css-selector* ".title a")


(defparameter *url* ""
  "url from which is started a search. Should be base-url + suffix.")

(defparameter *tracklist* '() "list of users to track. Add in init.el.")

(defparameter *data-file* (asdf:system-relative-pathname :clwhosconnected  "data.txt")
  "the file name in which are stored lines of things to track.")

(defparameter *stats* #H())

(defparameter *all-connected* nil
  "list of connected names. Is set after a search by get-all-connected.")


(defun tracklist-names (&optional (tracklist *tracklist*))
  "List items can be a string or a cons cell with an alist of properties."
  (format t "tracklist...")
  (sort (map ^(if (consp %) (car %) %) tracklist)
        #'string<=))

(defun names ()
  (format t "~a~&" (tracklist-names)))

(defun present (it)
  (format t "~a" (count it (tracklist-names) :test #'equal)))

(defun titles2url (titles)
  "From a list of user names, give back the full url."
  (map ^(str:concat *BASE-URL* %) titles))

(defun name2url (name)
  "From a name, return the url."
  (assert *base-url*)
  (str:concat *base-url* name))

(defvar *connected* nil)

(defun get-url (url)
  (dex:get url
           :headers '(("user-agent" . *user-agent*))
           :verbose nil))

(defun get-titles (url)
  (assert (str:starts-with? "http" url))
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

(defun get-all-last-connections ()
  ())

(defun tracked? (name)
  ;; (princ (find name *tracklist* :test 'equal :key ^(if (consp %) (car %) %)))
  (find name *tracklist* :test 'equal :key ^(if (consp %) (car %) %)))

(defun print-name (str-or-cons &key (stream t))
  (if (consp str-or-cons)
      (progn
        (format stream "~A: " (car str-or-cons))
        ;; todo map on keys
        (format stream "~{~a~^, ~}~%" (cdr (assoc :keywords (cdr str-or-cons)))))
      (format stream "~a~%" str-or-cons)))

(defun print-connected (names)
  "Print who's connected (who was found with the last `get-all-connected`."
  (let* ((watched (map ^(tracked? %) names))
         (watched (remove nil watched)))
    ;; (format t "to print: ~A~%" watched)
    (format t "~&----------~%")
    (map ^(print-name %) watched)
    t))

(defun get-all-connected (&optional names)
  (assert *base-url*)
  ;; Sometimes, after a C-z or hibernation,
  ;; if we don't recreate the kernel, netwwork requests can hang.
  (setf lparallel:*kernel* nil)
  (setf lparallel:*kernel* (lparallel:make-kernel 4))
  (unless *tracklist*
    (load-init))
  (let ((names (or names (get-all-titles))))
    (if-let (connected (intersection names (tracklist-names *tracklist*) :test 'equal))
      (progn
        (print-connected connected)
        (map ^(format t "~A\n" %) (titles2url connected))
        (setf *all-connected* connected)
        connected))))

(defun get-connected-ones (&optional (tracklist *tracklist*) (url *url*))
  (format t "searching again...\n")
  (let* ((soup (plump:parse (dex:get url)))
         (titles (lquery:$ soup ".title a" (text)))
         (titles (map ^ (str:trim %) titles))
         (titles (progn
                   ;; (format t "found: ~A" titles)
                   (setq *connected* titles)
                   (coerce titles 'list)))
         (titles (intersection titles tracklist :test 'equal)))
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

(defun build-data-tracklist ()
  (map ^(progn
          (if (= 1 (length %))
              (name-from-url (car %))
              `(,(name-from-url (car %)) . ((:keywords . ,(cdr %))))))
       (data-words)))

(defun add-data-tracklist ()
  "Add items listed in `*data-file*' into the `*tracklist*'."
  (setf *tracklist* (append (build-data-tracklist) *tracklist*)))

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

(defun browse (name &rest rest)
  "Open name(s) with a web browser. Complete with the connected names, not all."
  (let ((names (cons name rest)))
    (loop for it in names
         do (uiop:run-program (list *browser* (name2url it))))))

(defun view (str)
  "Search for files with `mfind` and play the results with a media player."
  (let* ((res (mfind str))
         (res (map ^(namestring (truename %)) res))
         (playlist "/tmp/whosplaylist"))
    (when res
      (str:to-file playlist (str:join "\n" res))
      (uiop:run-program (list "mpv" (str:concat "--playlist=" playlist))))))

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
  (format t "loading ~a: ~a~&" *init* (load *init* :verbose verbose :print print))
  (add-data-tracklist))

(defun reload ()
  (load-init)
  (format t "loading ~a~&" *tracklist*)
  (add-data-tracklist))

(defun version ()
  (format t "~a~&" +version+))


;; command-line.

(defun handle-parser-error (c)
  (format t "cli args parser error: ~a~&" (opts:option c)))

(defun main (&key interactive)
  ;; (setf lparallel:*kernel* (lparallel:make-kernel 4))
  (load-init)


  (opts:define-opts
    (:name :interactive
           :description "Enter interactive prompt."
           :short #\i
           :long "interactive"))

  (multiple-value-bind (options)
      (handler-bind ((error #'handle-parser-error))
        (opts:get-opts))

    (if (or (getf options :interactive)
            interactive)
        (progn

          ;; Build our repl commands, help and repl goodies with replic.

          (setf replic:*prompt* (cl-ansi-text:green "whosconnected > "))

          ;; create commands from the exported functions and variables.
          (replic.completion:functions-to-commands :replic.base)
          (replic.completion:add-completion "help" #'replic::help-completion)
          (replic.completion:functions-to-commands :clwhosconnected)

          ;; read replic's config files and read the "clwhosconnected" section.
          ;; and only that section, not the default one, at the moment.
          (replic.config:apply-config :clwhosconnected)

          ;; define completions.
          (replic.completion:add-completion "browse" (lambda () *all-connected*))

          ;; complete all other commands with a name.
          (setf replic.completion:*default-command-completion* #'tracklist-names)

          ;; start the repl.
          (replic:repl))

        (format t "~a~&" (get-all-connected)))))
