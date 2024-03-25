;;; org-count-words.el --- Count words in org-mode -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/org-count-words

;; Version: 0.1
;; Package-Requires: ((emacs "28.2"))
;; Keywords: org, count-word
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'org-element)

;;;; Customizations

(defgroup org-count-words nil
  "Count word in org-mode."
  :link '(url-link :tag "Homepage" "https://github.com/Elilif/org-count-words")
  :group 'org
  :prefix "org-count-words-")

(defcustom org-count-words-elemnts '(;; elements
                                     headline footnote-definition
                                     paragraph item
                                     ;; TODO count words in tables
                                     table
                                     example-block verse-block
                                     ;; objects
                                     latex-fragment link inline-src-block
                                     inline-babel-call export-snippet)
  "List of element types that should be included.

See `org-element-map' for details."
  :group 'org-count-words
  :type '(list symbol))

(defcustom org-count-words-no-recursive-elemnts '(drawer)
  "List of element types that should be exlucded in recursive mapping.

See `org-element-map' for details."
  :group 'org-count-words
  :type '(list symbol))

(defcustom org-count-words-element-functions '((headline . org-count-words-headline)
                                               (footnote-definition . org-count-words-footnote-definition)
                                               (paragraph . org-count-words-default)
                                               (link . org-count-words-link)
                                               (item . org-count-words-headline)
                                               (example-block . org-count-words-example-block)
                                               (verse-block . org-count-words-verse-block)
                                               (latex-fragment . org-count-words-remove)
                                               (inline-src-block . org-count-words-remove)
                                               (inline-babel-call . org-count-words-remove)
                                               (export-snippet . org-count-words-remove))
  "Alist of org elelemnt type and word-counting funtion.

Each function will be called with one arguemnt: the selected
element."
  :group 'org-count-words
  :type '(alist :key-type symbol :value-type function))

(defcustom org-count-words-function #'org-count-words-count-words
  "Defualt function to count words.

This function should take two argments(beg and end) and return a
number."
  :group 'org-count-words
  :type 'function)

;;;; Utilities

(defun org-count-words-count-words (start end)
  "Count words between START and END."
  (let ((words 0)
        ;; Count across field boundaries. (Bug#41761)
        (inhibit-field-text-motion t))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward
                "\\cc\\|[A-Za-z0-9][A-Za-z0-9[:punct:]]*"
                end t)
          (setq words (1+ words)))))
    words))

(defun org-count-words--general (element beg end)
  "General function for counting words.

Extract the BEG and END value from the property of ELEMENT and
count words in that region."
  (let ((beg (org-element-property beg element))
        (end (org-element-property end element)))
    (cond
     ((and beg end)
      (funcall org-count-words-function beg end))
     ((or beg end)
      (error "Wrong region!"))
     (t 0))))

(defun org-count-words-default (element)
  "Default function for count words in ELEMENT."
  (org-count-words--general element :begin :end))

(defun org-count-words-remove (element)
  (- (org-count-words-default element)))

(defun org-count-words-headline (element)
  "Count words in headline ELEMENT."
  (let ((beg (org-element-property :begin element))
        (end (or (org-element-property :contents-begin element)
                 (org-element-property :end element))))
    (funcall org-count-words-function beg end)))

(defun org-count-words-footnote-definition (_element)
  "Count words in footnote-definition ELEMENT."
  1)

(defun org-count-words-link (element)
  "Count words in link ELEMENT."
  (pcase (org-element-property :format element)
    ((or 'angle 'plain) 0)
    ('bracket
     (- (org-count-words--general element :contents-begin :contents-end)
        (org-count-words--general element :begin :end)))
    (_
     (error "Unkown link type: %s" (org-element-property :format element)))))

(defun org-count-words-example-block (element)
  "Count words in example-block ELEMENT."
  (- (org-count-words--general element :begin :end)
     2))

(defun org-count-words-verse-block (element)
  "Count words in verse-block ELEMENT."
  (org-count-words--general element :contents-begin :contents-end))

;;;; modeline segment
(defcustom org-count-words-buffer-idle-delay 0.3
  "Seconds to wait before updating the word counts in the current buffer."
  :group 'org-count-words
  :type 'number)

(defcustom org-count-words-region-idle-delay 0.1
  "Seconds to wait before updating the word counts in the selected
region."
  :group 'org-count-words
  :type 'number)

(defcustom org-count-words-mode-line '(:eval (org-count-words-update-modeline))
  "Mode line construct for displaying current word count."
  :group 'org-count-words
  :type 'sexp)

(defcustom org-count-words-mode-line-format '("Words:%s" . "Words:%s/%s")
  "The mode-line format for displaying current word count.

This value should be a `cons' whose `car' is a format string to
display the word count of the current buffer, and whose `cdr' is
a format string to display the word count of the selected region
and the current buffer."
  :group 'org-count-words
  :type 'cons)

(defvar-local org-count-words-buffer-count 0
  "Keep track of the per-buffer word-count statistics used to
update the modeline.")

(defvar-local org-count-words-region-count 0
  "Keep track of the per-buffer region word-count statistics used to
update the modeline.")

(defvar-local org-count-words--current-subtree nil
  "the current subtree.")

(defvar-local org-count-words--unchanged-count 0)

(defun org-count-words--buffer-set-info ()
  (let ((buffer-wc (org-count-words-buffer))
        (subtree-wc (org-count-words-subtree))
        (heading (org-element-lineage
                  (org-element-at-point)
                  '(headline) t)))
    (setq org-count-words--current-subtree heading
          org-count-words--unchanged-count (- buffer-wc subtree-wc))))

(defvar org-count-words-mode)

(defun org-count-words-update-modeline ()
  "Update word count information."
  (when org-count-words-mode
    (if (use-region-p)
        (format (cdr org-count-words-mode-line-format)
                org-count-words-region-count org-count-words-buffer-count)
      (format (car org-count-words-mode-line-format)
              org-count-words-buffer-count))))

(defun org-count-words-update-buffer-count (beg end len)
  (when (and org-count-words-mode
             (or (> len 0)
                 (string-match-p "\\cc\\|[A-Za-z0-9][A-Za-z0-9[:punct:]]*"
                                 (buffer-substring beg end))))
    (let* ((elem (org-element-at-point))
           (heading (or (org-element-lineage elem '(headline) t)
                        (org-element-lineage elem '(section) t)))
           subtree-wc)
      (unless (equal heading org-count-words--current-subtree)
        (while-no-input
          (org-count-words--buffer-set-info)))
      (pcase (setq subtree-wc (while-no-input (org-count-words-subtree)))
        ('t nil)
        ((pred numberp)
         (setq org-count-words-buffer-count
               (+ subtree-wc
                  org-count-words--unchanged-count)))))))

(defun org-count-words-update-region-count (&rest _args)
  (when (and org-count-words-mode (use-region-p))
    (let (region-wc)
      (pcase (setq region-wc (while-no-input
                               (org-count-words-region (region-beginning)
                                                       (region-end))))
        ('t nil)
        ((pred numberp)
         (setq org-count-words-region-count region-wc))))))

(defun org-count-words-debounce (&optional delay default)
  "Return a function that debounces its argument function."
  (let ((debounce-timer nil))
    (lambda (orig-fn &rest args)
      "Debounce calls to this function."
      (if (timerp debounce-timer)
          (timer-set-idle-time debounce-timer delay)
        (prog1 default
          (setq debounce-timer
                (run-with-idle-timer
                 delay nil
                 (lambda (buf)
                   (cancel-timer debounce-timer)
                   (setq debounce-timer nil)
                   (with-current-buffer buf
                     (apply orig-fn args)
                     (force-mode-line-update)))
                 (current-buffer))))))))

;;;###autoload
(define-minor-mode org-count-words-mode
  "Minor mode for displaying word count in the modeline."
  :group 'org-count-words
  (cond
   (org-count-words-mode
    (org-count-words-update-buffer-count (point-min) (point-max) 0)
    (advice-add #'org-count-words-update-buffer-count
                :around (org-count-words-debounce org-count-words-buffer-idle-delay)
                '((name . debounce)
                  (depth . -99)))

    (advice-add #'org-count-words-update-region-count
                :around (org-count-words-debounce org-count-words-region-idle-delay)
                '((name . debounce)
                  (depth . -99)))
    (set (make-local-variable 'mode-line-misc-info)
         (append mode-line-misc-info
                 (list org-count-words-mode-line)))
    (add-hook 'after-change-functions #'org-count-words-update-buffer-count nil t)
    (add-hook 'post-select-region-hook #'org-count-words-update-region-count nil t))
   (t
    (setq org-count-words-buffer-count 0
          org-count-words-region-count 0
          org-count-words--current-subtree nil)
    (setq mode-line-misc-info (delq org-count-words-mode-line mode-line-misc-info))
    (remove-hook 'after-change-functions #'org-count-words-update-buffer-count t)
    (remove-hook 'post-select-region-hook #'org-count-words-update-region-count t))))


;;;; interactive functions

;;;###autoload
(defun org-count-words-buffer ()
  "Count words in current org buffer.

Like `count-words' but ingore words in some elements like src-block."
  (interactive)
  (let ((datum (org-element-parse-buffer))
        (words 0))
    (org-element-map datum org-count-words-elemnts
      (lambda (elt)
        (let ((count (funcall
                      (alist-get (org-element-type elt)
                                 org-count-words-element-functions
                                 #'org-count-words-default)
                      elt)))
          (cl-incf words count)))
      nil nil org-count-words-no-recursive-elemnts)
    (if (not (called-interactively-p 'any))
        words
      (message "%s has %d words"
               (if (buffer-narrowed-p) "Narrowed part of buffer" "Buffer")
               words))))

;;;###autoload
(defun org-count-words-region (start end &optional arg)
  "Count the number of words in the region.

If called interactively, print a message reporting the number of
words in the region (whether or not the region is active); with
prefix ARG, report for the entire buffer rather than the region.

If called from Lisp, return the number of words between positions
START and END."
  (interactive (if current-prefix-arg
                   (list nil nil current-prefix-arg)
                 (list (region-beginning) (region-end) nil)))
  (save-excursion
    (save-restriction
      (cond
       ((not (called-interactively-p 'any))
        (narrow-to-region start end)
        (org-count-words-buffer))
       (arg
        (call-interactively #'org-count-words-buffer))
       (t
        (narrow-to-region start end)
        (call-interactively #'org-count-words-buffer))))))

;;;###autoload
(defun org-count-words-subtree ()
  "Count words in current subtree.

If called from Lisp, return the number of words."
  (interactive)
  (save-excursion
    (save-restriction
      (if (org-before-first-heading-p)
          (let ((elem (org-element-lineage
                       (org-element-at-point)
                       '(section) t)))
            (narrow-to-region
             (org-element-property :begin elem)
             (org-element-property :end elem)))
        (org-up-heading-safe)
        (org-narrow-to-subtree))
      (if (called-interactively-p 'any)
          (call-interactively #'org-count-words-buffer)
        (org-count-words-buffer)))))

;;;###autoload
(with-eval-after-load 'org
  (keymap-set org-mode-map "M-=" #'org-count-words-region))

(provide 'org-count-words)
;;; org-count-words.el ends here
