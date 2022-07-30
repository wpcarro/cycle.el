;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'cycle)
(require 'dash)
(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq xs (cycle-new 1 2 3))

(ert-deftest cycle-initializes-properly ()
  (should (= 3 (cycle-count xs)))
  (should (maybe-nil? (cycle-previous-focus xs)))
  (should (cycle-contains? 1 xs))
  (should (cycle-contains? 2 xs))
  (should (cycle-contains? 3 xs)))

(ert-deftest cycle-contains? ()
  ;; Returns t or nil
  (should (eq t (cycle-contains? 1 xs)))
  (should (eq t (cycle-contains? 2 xs)))
  (should (eq t (cycle-contains? 3 xs)))
  (should (eq nil (cycle-contains? 4 xs))))

(ert-deftest cycle-empty? ()
  (should (eq t (cycle-empty? (cycle-new))))
  (should (eq nil (cycle-empty? xs))))

(ert-deftest cycle-current ()
  (should (= 1 (cycle-current xs))))

(ert-deftest cycle-next! ()
  (let ((xs (cycle-from-list '(1 2 3))))
    (should (= 2 (cycle-next! xs)))))

(ert-deftest cycle-prev! ()
  (let ((xs (cycle-from-list '(1 2 3))))
    (cycle-next! xs)
    (should (= 1 (cycle-prev! xs)))))

(ert-deftest cycle-previous-focus ()
  (let ((xs (cycle-from-list '(1 2 3))))
    (cycle-focus-item! 2 xs)
    (cycle-next! xs)
    (should (= 2 (cycle-previous-focus xs)))))

(ert-deftest cycle-jump! ()
  (let ((xs (cycle-from-list '(1 2 3))))
    (should (= 1 (->> xs (cycle-jump! 0) cycle-current)))
    (should (= 2 (->> xs (cycle-jump! 1) cycle-current)))
    (should (= 3 (->> xs (cycle-jump! 2) cycle-current)))))

(ert-deftest cycle-focus-previous! ()
  (let ((xs (cycle-from-list '(1 2 3))))
    (cycle-focus-item! 2 xs)
    (cycle-next! xs)
    (should (= 2 (cycle-previous-focus xs)))
    (should (= 2 (cycle-focus-previous! xs)))))

(ert-deftest cycle-append! ()
  (let ((xs (cycle-from-list '(1 2 3))))
    (cycle-focus-item! 2 xs)
    (cycle-append! 4 xs)
    (should (equal '(1 4 2 3) (cycle-xs xs)))))

(ert-deftest cycle-remove! ()
  (let ((xs (cycle-from-list '(1 2 3))))
    (should (equal '(1 2) (cycle-xs (cycle-remove! 3 xs))))))

(ert-deftest cycle-misc ()
  (cycle-focus-item! 3 xs)
  (cycle-focus-item! 2 xs)
  (cycle-remove! 1 xs)
  (should (= 2 (cycle-current xs)))
  (should (= 3 (cycle-previous-focus xs))))
