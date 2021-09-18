;;;; namespace

(in-package #:clojure-cl.namespace)

;; vars

(defvar *ns* nil)
(defvar *namespaces* (make-hash-table))

;; structs

(defstruct (clj-namespace (:conc-name ns-))
  (name nil :type symbol)
  (cl-package nil)
  (aliases (make-hash-table :test 'equal))
  (imports (make-hash-table :test 'equal))
  (interns (make-hash-table :test 'equal))
  (map (make-hash-table :test 'equal))
  (publics (make-hash-table :test 'equal))
  (refers (make-hash-table :test 'equal))
  (macros (make-hash-table)))

;; utils

(defmacro with-namespace ((var sym) &body body)
  `(alexandria:if-let ((,var (gethash ,sym *namespaces*)))
     (progn ,@body)
     (error (format nil "No namespace: ~a found" ,var))))

;; Creating/changing namespace

(defun create-ns (sym)
  "Create a new namespace named by the symbol if one doesn't already exist, returns it or the already-existing namespace of the same name."
  (or (gethash sym *namespaces*)
      (setf (gethash sym *namespaces*) (make-clj-namespace
					:name sym
					:cl-package
					(make-package (symbol-name sym))))))

(defun in-ns (name)
  "Sets *ns* to the namespace named by the symbol, creating it if needed."
  (setf *ns* (create-ns name)))

(defun ns (name &optional docstring attr-map &rest references)
  "Usage: (ns name docstring? attr-map? references*)

Sets *ns* to the namespace named by name (unevaluated), creating it
if needed.  references can be zero or more of: (:refer-clojure ...)
(:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)
with the syntax of refer-clojure/require/use/import/load/gen-class
respectively, except the arguments are unevaluated and need not be
quoted. (:gen-class ...), when supplied, defaults to :name
corresponding to the ns name, :main true, :impl-ns same as ns, and
:init-impl-ns true. All options of gen-class are
supported. The :gen-class directive is ignored when not
compiling. If :gen-class is not supplied, when compiled only an
nsname__init.class will be generated. If :refer-clojure is not used, a
default (refer 'clojure.core) is used.  Use of ns is preferred to
individual calls to in-ns/require/use/import:

(ns foo.bar
  (:refer-clojure :exclude [ancestors printf])
  (:require (clojure.contrib sql combinatorics))
  (:use (my.lib this that))
  (:import (java.util Date Timer Random)
           (java.sql Connection Statement)))

Specs:
  Args: (cat
         :ns-name simple-symbol?
         :docstring (? string?)
         :attr-map (? map?)
         :ns-clauses :clojure.core.specs.alpha/ns-clauses)
  Ret:  any?"
  (error "TODO"))

;; Adding to a namespace

(defun alias (alias namespace-sym)
  "Add an alias in the current namespace to another namespace. Arguments are two symbols: the alias to be used, and the symbolic name of the target namespace. Use :as in the ns macro in preference to calling this directly."
  (let ((n *ns*))
    (with-accessors ((al ns-aliases))
	n
      (if (gethash namespace-sym *namespaces*)
	  (setf (gethash alias al) namespace-sym)
	  (error (format nil "No namespace: ~a found" namespace-sym))))))

(defun def (symbol &optional doc-string init)
  "Usage: (def symbol doc-string? init?)
Creates and interns a global var with the name of symbol in the current namespace (*ns*) or locates such a var if it already exists.  If init is supplied, it is evaluated, and the root binding of the var is set to the resulting value.  If init is not supplied, the root binding of the var is unaffected."
  (declare (ignore symbol doc-string init))
  (error "TODO"))

(defun cl-import (&rest body)
  "Usage: (import & import-symbols-or-lists)

import-list => (package-symbol class-name-symbols*)

For each name in class-name-symbols, adds a mapping from name to the class named by package.name to the current namespace. Use :import in the ns macro in preference to calling this directly.

Specs:
  Args: (*
          (alt
           :class (quotable simple-symbol?)
           :package-list (quotable
                           :clojure.core.specs.alpha/package-list)))
  Ret:  any?"
  (error "TODO because macro"))

(defun clj-intern (ns name)
  (check-type ns symbol)
  (check-type name symbol)
  (with-namespace (n ns)
    (with-accessors ((package ns-cl-package)
		     (interns ns-interns))
	n
      (let ((new-sym (intern (symbol-name name) package)))
	(unless (gethash new-sym interns)
	  (setf (gethash new-sym interns) nil))
	new-sym))))

(defun clj-intern2 (ns name val)
  (check-type ns symbol)
  (check-type name symbol)
  (with-namespace (n ns)
    (with-accessors ((package ns-cl-package)
		     (interns ns-interns))
	n
      (let ((new-sym (intern (symbol-name name) package)))
	(setf (gethash new-sym interns) val)
	new-sym))))

(defun refer (ns-sym &rest filter)
  "refers to all public vars of ns, subject to filters.
filters can include at most one each of:

:exclude list-of-symbols
:only list-of-symbols
:rename map-of-fromsymbol-tosymbol

For each public interned var in the namespace named by the symbol,
adds a mapping from the name of the var to the var to the current
namespace.  Throws an exception if name is already mapped to
something else in the current namespace. Filters can be used to
select a subset, via inclusion or exclusion, or to provide a mapping
to a symbol different from the var's name, in order to prevent
clashes. Use :use in the ns macro in preference to calling this directly."
  (with-namespace (n ns-sym)
    (let ((excludes (getf filter :exclude nil))
	  (onlies (getf filter :only nil))
	  (renames (getf filter :rename nil)))
      (if onlies
	  (loop for key in (or onlies (alexandria:hash-table-keys (ns-publics n)))
		for (val s) = (multiple-value-list (gethash key (ns-publics n)))
		for sym-name = (getf renames key key)
		unless (find key excludes)
		  do (setf (gethash sym-name (ns-refers n)) key))))))

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
