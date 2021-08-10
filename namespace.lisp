;;;; namespace

(in-package #:clojure-cl.namespace)

;; vars

(defvar *current-name-space* nil)

;; structs

(defstruct (clj-namespace (:conc-name ns-))
  (name nil :type string)
  (aliases (make-hash-table :type 'equal))
  (imports (make-hash-table :type 'equal))
  (interns (make-hash-table :type 'equal))
  (map (make-hash-table :type 'equal))
  (publics (make-hash-table :type 'equal))
  (refers (make-hash-table :type 'equal)))

;; Creating/changing namespace

(defun create-ns ())
(defun in-ns ())
(defun ns ())

;; Adding to a namespace

(defun alias ())
(defun def ())
(defun import ())
(defun clj-intern ())
(defun refer ())

;; Finding namespaces

(defun all-ns ())
(defun find-ns ())

;; Getting a namespace from a symbol

(defun resolve ())
(defun ns-resolve ())
(defun namespace ())

;; Removing things

(defun ns-unalias ())
(defun ns-unmap ())
(defun remove-ns ())
