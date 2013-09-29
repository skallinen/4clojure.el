(require 'json)
(require 'request)

; format: (number question-data)
(setq 4clojure-cached-question nil)

(defun 4clojure/get-question-cached (problem-number)
  "Gets a 4clojure problem, saves it, returns that if asked again"
  (if (string= (car 4clojure-cached-question) problem-number)
      (cadr 4clojure-cached-question)
    (progn
      (request
       (format "http://www.4clojure.com/api/problem/%s" problem-number)
       :parser 'json-read
       :sync t
       :success (function*
                 (lambda (&key data &allow-other-keys)
                   (setq 4clojure-cached-question
                         `(,problem-number ,data)))))
      (cadr 4clojure-cached-question))))

(defun 4clojure/questions-for-problem (problem-number)
  "Gets a list of questions/tests corresponding to a 4clojure problem"
  (mapconcat 'identity
             (assoc-default 'tests
                            (4clojure/get-question-cached problem-number))
             "\n\n"))

(defun 4clojure/first-question-for-problem (problem-number)
  "Gets the first question of a 4clojure problem (sometimes there only is one),
these are called 'tests' on the site"
  (replace-regexp-in-string
   "" ""
   (elt (assoc-default 'tests
                       (4clojure/get-question-cached problem-number))
        0)))

(defun 4clojure/description-of-problem (problem-number)
  "Gets the description of a 4clojure problem"
  (assoc-default 'description
                 (4clojure/get-question-cached problem-number)))


(defun 4clojure/start-new-problem (problem-number)
  "Opens a new buffer with a 4clojure problem and description in it. Doesn't
clobber existing text in the buffer (if the problem was already opened"
  (let ((buffer (get-buffer-create (format "*4clojure-problem-%s*" problem-number)))
        (questions (4clojure/questions-for-problem problem-number))
        (description (4clojure/description-of-problem problem-number)))
    (switch-to-buffer buffer)
    ; only add to empty buffers, thanks: http://stackoverflow.com/q/18312897
    (when (= 0 (buffer-size buffer))
      (insert (4clojure/format-problem-for-buffer problem-number description questions))
      (beginning-of-buffer)
      (search-forward "__")
      (backward-char 2)
      (clojure-mode))))

(defun 4clojure/format-problem-for-buffer (problem-number description questions)
  "Formats a 4clojure question and description for an emacs buffer (adds a
header, a tip about how to check your answers, etc)"
  (concat
   ";; 4Clojure Question " problem-number "\n"
   ";;\n"
   ";; Use M-x 4clojure-check-answers when you're done!\n"
   ";;\n"
   ";; " description "\n\n"
   (replace-regexp-in-string "" "" questions)))

(defun 4clojure/get-answer-from-current-buffer (problem-number)
  "Gets the user's answer to the first question by getting the original question
 (with a blank in it) from 4clojure and matching that against the current
 buffer"
  (string-match
   (replace-regexp-in-string
    "[\s\n]\+"
    "[\s\n]\+"
    (replace-regexp-in-string
     "__"
     "\\(.*\\)"
     (regexp-quote (4clojure/first-question-for-problem problem-number))
     nil t)
    nil t)
   (buffer-string))
  (match-string 1 (buffer-string)))


(defun 4clojure/problem-number-of-current-buffer ()
  "Gets the problem number of the current buffer or 0 if current buffer isn't
named something like *blah-blah-123*"
  (let* ((bufname (buffer-name (current-buffer)))
         (number-with-star (first (last (split-string bufname "-"))))
         (problem-number (substring number-with-star
                                    0
                                    (1- (string-width number-with-star)))))
    (if (string-match "[^0-9]" problem-number)
        0
      (string-to-int problem-number))))

(defun 4clojure/check-answer (problem-number answer)
  "Sends an answer to 4clojure and returns the result"
  (request
   (format "http://www.4clojure.com/rest/problem/%s" problem-number)
   :type "POST"
   :parser 'json-read
   :sync t
   :data `(("id" . ,problem-number) ("code" . ,answer))
   :success (function*
             (lambda (&key data &allow-other-keys)
               (let ((error (assoc-default 'error data))
                     (message (assoc-default 'message data))
                     (indexOfFailing (assoc-default 'failingTest data)))
                 (setq result
                       (if (> (string-width error) 0)
                           `(,indexOfFailing ,error)
                         `(,nil ,message)))))))
  result)

;;;###autoload
(defun 4clojure-open-question ()
  "Opens a 4clojure problem in an aptly named buffer"
  (interactive)
  (setq 4clojure-problem-number (read-string "Which 4clojure question? "))
  (4clojure/start-new-problem 4clojure-problem-number))

;;;###autoload
(defun 4clojure-next-question ()
  "Gets the next 4clojure question or 1st question based on the current buffer
name"
  (interactive)
  (let ((problem-number (4clojure/problem-number-of-current-buffer)))
    (4clojure/start-new-problem (int-to-string (1+ problem-number)))))


;;;###autoload
(defun 4clojure-previous-question ()
  "Opens the previous 4clojure question or 1st question based on the current
buffer name"
  (interactive)
  (let ((problem-number (4clojure/problem-number-of-current-buffer)))
    (4clojure/start-new-problem (int-to-string (if (< problem-number 3)
                                                   1
                                                 (1- problem-number))))))

;;;###autoload
(defun 4clojure-check-answers ()
  "Sends the first answer to 4clojure and gets a message back"
  (interactive)
  (let* ((problem-number-as-int (4clojure/problem-number-of-current-buffer))
         (problem-number (int-to-string problem-number-as-int))
         (result (4clojure/check-answer
                  problem-number
                  (4clojure/get-answer-from-current-buffer problem-number))))
    (if (car result)
        (message "Test %d failed.\n%s"
                 (car result)
                 (cadr result))
      (message "%s" (cadr result)))))
