;;; cycle.el --- Simple module for working with cycles -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Something like this may already exist, but I'm having trouble finding it, and
;; I think writing my own is a nice exercise for learning more Elisp.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'maybe)
(require 'list)
(require 'struct)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wish list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - TODO: Provide immutable variant.
;; - TODO: Replace mutable consumption with immutable variant.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `current-index' tracks the current index
;; `xs' is the original list
(cl-defstruct cycle current-index previous-index xs)

(defun cycle-from-list (xs)
  "Create a cycle from a list of `XS'."
  (if (= 0 (length xs))
      (make-cycle :current-index nil
                  :previous-index nil
                  :xs xs)
    (make-cycle :current-index 0
                :previous-index nil
                :xs xs)))

(defun cycle-new (&rest xs)
  "Create a cycle with XS as the values."
  (cycle-from-list xs))

(defun cycle-to-list (xs)
  "Return the list representation of a cycle, XS."
  (cycle-xs xs))

(defun cycle-previous-focus (cycle)
  "Return the previously focused entry in CYCLE."
  (let ((i (cycle-previous-index cycle)))
    (if (maybe-some? i)
        (nth i (cycle-xs cycle))
      nil)))

(defun cycle-focus-previous! (xs)
  "Jump to the item in XS that was most recently focused; return the cycle.
This will error when previous-index is nil.  This function mutates the
underlying struct."
  (let ((i (cycle-previous-index xs)))
    (if (maybe-some? i)
        (progn
          (cycle-jump! i xs)
          (cycle-current xs))
      (error "Cannot focus the previous element since cycle-previous-index is nil"))))

(defun cycle-next! (xs)
  "Return the next value in `XS' and update `current-index'."
  (let* ((current-index (cycle-current-index xs))
         (next-index (cycle--next-index-> 0 (cycle-count xs) current-index)))
    (struct-set! cycle previous-index current-index xs)
    (struct-set! cycle current-index next-index xs)
    (nth next-index (cycle-xs xs))))

(defun cycle-prev! (xs)
  "Return the previous value in `XS' and update `current-index'."
  (let* ((current-index (cycle-current-index xs))
         (next-index (cycle--next-index<- 0 (cycle-count xs) current-index)))
    (struct-set! cycle previous-index current-index xs)
    (struct-set! cycle current-index next-index xs)
    (nth next-index (cycle-xs xs))))

(defun cycle-current (cycle)
  "Return the current value in `CYCLE'."
  (nth (cycle-current-index cycle) (cycle-xs cycle)))

(defun cycle-count (cycle)
  "Return the length of `xs' in `CYCLE'."
  (length (cycle-xs cycle)))

(defun cycle-jump! (i xs)
  "Jump to the I index of XS."
  (let ((current-index (cycle-current-index xs))
        (next-index (mod i (cycle-count xs))))
    (struct-set! cycle previous-index current-index xs)
    (struct-set! cycle current-index next-index xs))
  xs)

(defun cycle-focus! (p cycle)
  "Focus the element in CYCLE for which predicate, P, is t."
  (let ((i (->> cycle
                cycle-xs
                (-find-index p))))
    (if i
        (cycle-jump! i cycle)
      (error "No element in cycle matches predicate"))))

(defun cycle-focus-item! (x xs)
  "Focus item, X, in cycle XS.
ITEM is the first item in XS that t for `equal'."
  (cycle-focus! (lambda (y) (equal x y)) xs))

(defun cycle-append! (x xs)
  "Add X to the left of the focused element in XS.
If there is no currently focused item, add X to the beginning of XS."
  (if (cycle-empty? xs)
      (progn
        (struct-set! cycle xs (list x) xs)
        (struct-set! cycle current-index 0 xs)
        (struct-set! cycle previous-index nil xs))
    (let ((curr-i (cycle-current-index xs))
          (prev-i (cycle-previous-index xs)))
      (if curr-i
          (progn
            (struct-set! cycle xs (-insert-at curr-i x (cycle-xs xs)) xs)
            (when (and prev-i (>= prev-i curr-i))
              (struct-set! cycle previous-index (1+ prev-i) xs))
            (when curr-i (struct-set! cycle current-index (1+ curr-i) xs)))
        (progn
          (struct-set! cycle xs (cons x (cycle-xs xs)) xs)
          (when prev-i (struct-set! cycle previous-index (1+ prev-i) xs))))
      xs)))

(defun cycle-remove! (x xs)
  "Attempt to remove X from XS.

X is found using `equal'.

If X is the currently focused value, after it's deleted, current-index will be
  nil.  If X is the previously value, after it's deleted, previous-index will be
  nil."
  (let ((curr-i (cycle-current-index xs))
        (prev-i (cycle-previous-index xs))
        (rm-i (-elem-index x (cycle-xs xs))))
    (struct-set! cycle xs (-remove-at rm-i (cycle-xs xs)) xs)
    (when prev-i
      (when (> prev-i rm-i) (struct-set! cycle previous-index (1- prev-i) xs))
      (when (= prev-i rm-i) (struct-set! cycle previous-index nil xs)))
    (when curr-i
      (when (> curr-i rm-i) (struct-set! cycle current-index (1- curr-i) xs))
      (when (= curr-i rm-i) (struct-set! cycle current-index nil xs)))
    xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cycle-contains? (x xs)
  "Return t if cycle, XS, has member X."
  (->> xs
       cycle-xs
       (list-contains? x)))

(defun cycle-empty? (xs)
  "Return t if cycle XS has no elements."
  (= 0 (length (cycle-xs xs))))

(defun cycle-focused? (xs)
  "Return t if cycle XS has a non-nil value for current-index."
  (maybe-some? (cycle-current-index xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cycle--next-index<- (lo hi x)
  "Return the next index in a cycle when moving downwards.
- `LO' is the lower bound.
- `HI' is the upper bound.
- `X' is the current index."
  (if (< (- x 1) lo)
      (- hi 1)
    (- x 1)))

(defun cycle--next-index-> (lo hi x)
  "Return the next index in a cycle when moving upwards.
- `LO' is the lower bound.
- `HI' is the upper bound.
- `X' is the current index."
  (if (>= (+ 1 x) hi)
      lo
    (+ 1 x)))

(provide 'cycle)
;;; cycle.el ends here
