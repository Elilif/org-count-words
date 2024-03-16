;;; org-count-words.el --- Count words in org-mode -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/org-count-words

;; Version: 0.1
;; Package-Requires: ((emacs "28.2"))
;; Keywords: org, count-word
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(require 'org-element)

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

(defcustom org-count-words-no-recursive-elemnts '()
  "List of element types that should be exlucded in recursive map.

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

(defcustom org-count-words-functions #'count-words
  "Defualt function to count words.

This function should take two argments(beg and end) and return a
number."
  :group 'org-count-words
  :type 'function)

(defun org-count-words--general (element beg end)
  (let ((beg (org-element-property beg element))
        (end (org-element-property end element)))
    (cond
     ((and beg end)
      (funcall org-count-words-functions beg end))
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
    (funcall org-count-words-functions beg end)))

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
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (if (called-interactively-p 'any)
          (call-interactively #'org-count-words-buffer)
        (org-count-words-buffer)))))

;;;###autoload
(with-eval-after-load 'org
  (keymap-set org-mode-map "M-=" #'org-count-words-region))

(provide 'org-count-words)
;;; org-count-words.el ends here
