(defpackage :symbol-munger
    (:use :cl :cl-user :iter)
  (:export :normalize-capitalization-and-spacing
	   :english->lisp-symbol
	   :english->lisp-name
	   :english->keyword
	   :english->camel-case
	   :english->studly-case
	   :english->underscores

	   :lisp->english
	   :lisp->keyword
	   :lisp->camel-case
	   :lisp->underscores
	   :lisp->studly-caps
           :combine-symbols
           :reintern
           :qualified-symbol-string

	   :camel-case->english
	   :camel-case->lisp-name
	   :camel-case->lisp-symbol
	   :camel-case->keyword
	   :camel-case->underscores

	   :underscores->english
	   :underscores->lisp-name
	   :underscores->lisp-symbol
	   :underscores->keyword
	   :underscores->camel-case
	   :underscores->studly-caps
	   ))

(in-package :symbol-munger)

(defgeneric %coerce-to-string (s)
  (:documentation "This method can be specialized to help turn objects into
   strings so they can be combined and normalized correctly")
  (:method (s)
    (typecase s
      (symbol (symbol-name s))
      (string s)
      (float (format nil "~F" s))
      (t (princ-to-string s)))))

(defmacro ensure-list! (place)
  `(setf ,place (alexandria:ensure-list ,place)))

(defmacro ensure-flat-list! (place)
  `(setf ,place (alexandria:flatten ,place)))

(defun qualified-symbol-string (sym)
  (let ((*package* (find-package :keyword))
        (*print-pretty* nil))
    (format nil "~S" sym)))

(defun normalize-capitalization-and-spacing
    (s &key (capitalize :each-word) (word-separators #\space)
       word-separators-to-replace stream in-place)
  "Will recapitalize a string and replace word-separators with a standard one
   (in-place if desired and possible)

   If s is a lisp tree, then each part will be %coerce-to-string'ed and treated
   as a separate part of the phrase being normalized

   Will write to a stream if given otherwise it.
   Defaults to capitalizing each word but can be any of
     {:each-word :first-word T (:all is an alias for T) nil :but-first-word (likeJavaScript) }

   word-separators are used to distinguish new words for the purposes of capitalization
     The first of these will be used to replace word-separators-to-replace (auto flattened)
   word-separators-to-replace helps normalize word separators so that spaces or underscores
     become the appropriate word-separator.
     If this eql :capitals it assumes capital letters indicate a new word separation
     (auto flattened)

   returns a string (new or the one passed in if in-place) unless :stream is provided"
  ;; Check and enforce our assumptions
  (ecase capitalize ((:each-word :first-word :but-first-word T :all nil) T))
  (when (and in-place (member :capitals word-separators-to-replace))
    (error "in-place replacement is not available for word separators which take no space (such as :capitals)"))
  (ensure-flat-list! word-separators)
  (ensure-flat-list! word-separators-to-replace)

  (let ((str (or stream (unless in-place
			  (make-string-output-stream))))
	(replacement-sep (let ((it (first word-separators)))
                           (typecase it
                             (string (if (= 1 (length it))
                                         (elt it 0)
                                         it))
                             (t it))))
        (just-wrote-separator? nil))
    (labels ((%write (c)
               (etypecase c
                 (character (write-char c str))
                 (string (write-string c str))
                 (symbol (write-string (symbol-name c) str))))
             (write-c (c)
               (cond ((string= c replacement-sep)
                      (unless just-wrote-separator?
                        (setf just-wrote-separator? t)
                        (%write c)))
                     (t
                      (setf just-wrote-separator? nil)
                      (%write c)))))
      (iter (for part in (alexandria:flatten s))
        (for source-string = (%coerce-to-string part))
        (for start-of-phrase? = (first-iteration-p))
        (iter
          (for c in-string source-string)
          (for last-c previous c)
          (for i from 0)
          (for is-cap? = (eql c (char-upcase c)))
          (setf start-of-phrase? (and start-of-phrase? (first-iteration-p)))
          (for start-of-word? =
               (or (first-iteration-p)
                   (and is-cap? (member :capitals word-separators-to-replace))
                   (and is-cap? (member :capitals word-separators))
                   ;; the last char we wrote was some kind of separator
                   (member last-c word-separators-to-replace :test #'string-equal)
                   (member last-c word-separators :test #'string-equal)))

          ;; handle capital letters as word-separators
          (when (and str ;; in-place will not work
                     replacement-sep ;; need to have a separator
                     (not start-of-phrase?) ;; dont start a string with a sep
                     ;; put separators before new words
                     start-of-word?)
            (write-c replacement-sep))

          (for should-cap? =
               (or (eq capitalize :all)
                   (eq capitalize T)
                   (and start-of-word?
                        (or (eq capitalize :each-word)
                            (if (first-iteration-p)
                                (eq capitalize :first-word)
                                (eq capitalize :but-first-word))))))

          (for char = (cond
                        ((member c word-separators-to-replace :test #'string-equal)
                         (or replacement-sep (next-iteration)))
                        (should-cap? (char-upcase c))
                        (T (char-downcase c))))

          (when in-place (setf (elt source-string i) char))
          (when str (write-c char)))))
    (cond ((not stream) (get-output-stream-string str))
	  (in-place s))))

(defun english->lisp-name (phrase &key stream capitalize)
  "Turns an english phrase into a string containing a common lisp style symbol-name"
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators (list #\-)
   :word-separators-to-replace (list #\_ #\space #\newline #\tab)))

(defun english->lisp-symbol (phrase &optional (package *package*))
  "Turns an english phrase into a common lisp symbol in the specified package"
  (intern (english->lisp-name phrase :capitalize T) package))

(defun english->keyword (phrase)
  "Turns an english phrase into a common lisp keyword"
  (english->lisp-symbol phrase :keyword))

(defun english->camel-case (phrase &key stream
		    (capitalize :but-first-word))
  "Turns an english phrase into a camelCasePhraseLikeThis "
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators nil
   :word-separators-to-replace (list #\_ #\space)))

(defun english->studly-case (phrase &key stream)
  "Turns an english phrase into a CamelCasePhraseLikeThis"
  (english->camel-case phrase :stream stream :capitalize :each-word))

(defun english->underscores (phrase &key stream capitalize)
  "Turns an english phrase into a a_phrase_like_this"
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators #\_
   :word-separators-to-replace (list #\space)))

(defun lisp->english (phrase &key stream
		      (capitalize :each-word)
		      (word-separator #\space))
  "Converts a common lisp symbol (or symbol-name) into an english phrase"
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators word-separator
   :word-separators-to-replace (list #\-)))

(defun lisp->keyword (phrase)
  (combine-symbols phrase :package :keyword))

(defun reintern (phrase &optional (package *package*))
  ;; never reintern nil
  (when phrase
    (combine-symbols phrase :package package)))

(defun combine-symbols (phrase &key (package *package*) (separator #\-))
  ;; never reintern nil
  (when phrase
    (intern
     (normalize-capitalization-and-spacing
      phrase
      :capitalize T
      ;; these are flattened so if nil it will just use #\-
      :word-separators separator
      :word-separators-to-replace nil)
     package)))

(defun lisp->camel-case (phrase &key stream)
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize :but-first-word
   :word-separators nil
   :word-separators-to-replace (list #\-)))

(defun lisp->studly-caps (phrase &key stream)
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize :each-word
   :word-separators nil
   :word-separators-to-replace (list #\-)))

(defun lisp->underscores (phrase &key stream capitalize)
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators #\_
   :word-separators-to-replace (list #\-)))

(defun camel-case->english (phrase &key stream
		    (capitalize :each-word))
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators #\space
   :word-separators-to-replace (list :capitals #\_)))

(defun camel-case->lisp-name (phrase &key stream
			    (capitalize nil))
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators #\-
   :word-separators-to-replace (list :capitals #\_)))

(defun camel-case->lisp-symbol (phrase &optional (package *package*))
  (intern (camel-case->lisp-name phrase :capitalize T) package))

(defun camel-case->keyword (phrase)
  (camel-case->lisp-symbol phrase :keyword))

(defun camel-case->underscores (phrase &key stream capitalize)
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators #\_
   :word-separators-to-replace (list :capitals #\space)))

;;;;;

(defun underscores->english (phrase &key stream
		    (capitalize :each-word))
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators #\space
   :word-separators-to-replace (list #\_)))

(defun underscores->lisp-name (phrase &key stream
			    (capitalize nil))
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize capitalize
   :word-separators #\-
   :word-separators-to-replace (list #\_)))

(defun underscores->lisp-symbol (phrase &optional (package *package*))
  "Turns a_phrase_with_underscores into a-phrase-with-underscores lisp symbol"
  (intern (underscores->lisp-name phrase :capitalize T) package))

(defun underscores->keyword (phrase)
  "Converts and underscores name to a common lisp keyword"
  (underscores->lisp-symbol phrase :keyword))

(defun underscores->camel-case (phrase &key stream)
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize :but-first-word
   :word-separators nil
   :word-separators-to-replace (list #\_)))

(defun underscores->studly-caps  (phrase &key stream)
  (normalize-capitalization-and-spacing
   phrase
   :stream stream :capitalize :each-word
   :word-separators nil
   :word-separators-to-replace (list #\_)))

