;;; mocker.el --- mocking framework for emacs

;; Copyright (C) 2011  Yann Hodique.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: lisp, testing

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eieio)

(defvar mocker-mock-default-record-cls 'mocker-record)

(put 'mocker-mock-error 'error-conditions '(mocker-mock-error error))
(put 'mocker-mock-error 'error-message "Mocker mock error")

(put 'mocker-record-error 'error-conditions '(mocker-record-error error))
(put 'mocker-record-error 'error-message "Mocker record error")

(defun mocker--plist-remove (plist key)
  ;; courtesy of pjb
  (if (eq (car plist) key) (cdr (cdr plist))
    (cons (car plist)
          (cons (cadr plist)
                (mocker--plist-remove (cddr plist) key)))))

;;; Mock object
(defclass mocker-mock ()
  ((function :initarg :function :type symbol)
   (argspec :initarg :argspec :initform nil :type list)
   (ordered :initarg :ordered :initform t)
   (records :initarg :records :initform nil :type list)))

(defmethod constructor :static ((mock mocker-mock) newname &rest args)
  (let* ((obj (call-next-method))
         (recs (oref obj :records)))
    (oset obj :records nil)
    (mapc #'(lambda (r)
              (apply 'mocker-add-record obj r))
          recs)
    obj))

(defmethod mocker-add-record ((mock mocker-mock) &rest args)
  (object-add-to-list mock :records
                      (let ((cls mocker-mock-default-record-cls)
                            (tmp (plist-get args :record-cls)))
                        (when tmp
                          (setq cls tmp
                                args (mocker-read-record cls
                                                         (mocker--plist-remove
                                                          args :record-cls))))
                        (apply 'make-instance cls :-mock mock args))
                      t))

(defmethod mocker-fail-mock ((mock mocker-mock) args)
  (signal 'mocker-mock-error
          (list (format (concat "Unexpected call to mock `%s'"
                                " with input `%s'")
                        (oref mock :function) args))))

(defmethod mocker-run ((mock mocker-mock) &rest args)
  (let ((rec (mocker-find-active-record mock args))
        (ordered (oref mock :ordered)))
    (cond ((null rec)
           (mocker-fail-mock mock args))
          ((or (not ordered) (mocker-test-record rec args))
           (mocker-run-record rec args))
          (t
           (mocker-fail-record rec args)))))

(defmethod mocker-find-active-record ((mock mocker-mock) args)
  (flet ((first-match (pred seq)
                      (let ((x nil))
                        (while (and seq
                                    (not (setq x (funcall pred (pop seq))))))
                        x)))
    (let* ((ordered (oref mock :ordered))
           rec)
      (if ordered
          (setq rec (first-match
                     #'(lambda (r)
                         (when (oref r :-active)
                           (if (mocker-test-record r args)
                               (progn
                                 (mocker-use-record r)
                                 r)
                             (mocker-skip-record r args))))
                     (oref mock :records)))
        (setq rec (first-match
                   #'(lambda (r)
                       (and
                        (oref r :-active)
                        (mocker-test-record r args)
                        (progn
                          (mocker-use-record r)
                          r)))
                   (oref mock :records))))
      rec)))

(defmethod mocker-verify ((mock mocker-mock))
  (mapc #'(lambda (r) (when (and (oref r :-active)
                                 (< (oref r :-occurrences)
                                    (oref r :min-occur)))
                        (signal 'mocker-record-error
                                (list (format
                                       (concat "Expected call to mock `%s',"
                                               " with input like %s,"
                                               " was not run.")
                                       (oref mock :function)
                                       (mocker-get-record-expectations r))))))
        (oref mock :records)))

;;; Mock record base object
(defclass mocker-record-base ()
  ((min-occur :initarg :min-occur :initform 1 :type number)
   (max-occur :initarg :max-occur :type (or null number))
   (-occurrences :initarg :-occurrences :initform 0 :type number
                 :protection :protected)
   (-mock :initarg :-mock)
   (-active :initarg :-active :initform t :protection :protected)))

(defmethod mocker-read-record :static ((rec mocker-record-base) spec)
  spec
  ;; (eval (cons 'list spec))
  )

(defmethod mocker-use-record ((rec mocker-record-base))
  (let ((max (oref rec :max-occur))
        (n (1+ (oref rec :-occurrences))))
    (oset rec :-occurrences n)
    (when (and (not (null max))
               (= n max))
      (oset rec :-active nil))))

(defmethod mocker-skip-record ((rec mocker-record-base) args)
  (if (>= (oref rec :-occurrences)
          (oref rec :min-occur))
      (oset rec :-active nil)
    (mocker-fail-record rec args)))

(defmethod mocker-test-record ((rec mocker-record-base) args)
  (error "not implemented in base class"))

(defmethod mocker-run-record ((rec mocker-record-base) args)
  (error "not implemented in base class"))

(defmethod mocker-get-record-expectations ((rec mocker-record-base)))

(defmethod mocker-fail-record ((rec mocker-record-base) args)
  (signal 'mocker-record-error
          (list (format (concat "Violated record while mocking `%s'."
                                " Expected input like: %s, got: `%s' instead")
                        (oref (oref rec :-mock) :function)
                        (mocker-get-record-expectations rec)
                        args))))

;;; Mock record default object
(defclass mocker-record (mocker-record-base)
  ((input :initarg :input :initform nil :type list)
   (output :initarg :output :initform nil)
   (input-matcher :initarg :input-matcher :initform nil)
   (output-generator :initarg :output-generator :initform nil)))

(defmethod constructor :static ((rec mocker-record) newname &rest args)
  (let* ((obj (call-next-method)))
    (when (or (not (slot-boundp obj :max-occur))
              (and (oref obj :max-occur)
                   (< (oref obj :max-occur)
                      (oref obj :min-occur))))
      (oset obj :max-occur (oref obj :min-occur)))
    obj))

;; (defmethod mocker-read-record :before ((rec mocker-record) spec)
;;   (when (plist-member spec :input-matcher)
;;     (plist-put spec :input-matcher
;;                (function (plist-get spec :input-matcher))))
;;   (when (plist-member spec :output-generator)
;;     (plist-put spec :output-generator
;;                (function (plist-get spec :output-generator)))))

(defmethod mocker-test-record ((rec mocker-record) args)
  (let ((matcher (oref rec :input-matcher))
        (input (oref rec :input)))
    (cond (matcher
           (apply matcher args))
          (t
           (equal input args)))))

(defmethod mocker-run-record ((rec mocker-record) args)
  (let ((generator (oref rec :output-generator))
        (output (oref rec :output)))
    (cond (generator
           (apply generator args))
          (t
           output))))

(defmethod mocker-get-record-expectations ((rec mocker-record))
  (format "`%s'" (or (oref rec :input-matcher) (oref rec :input))))

;;; Mock simple stub object
(defclass mocker-stub-record (mocker-record-base)
  ((output :initarg :output :initform nil)))

(defmethod constructor :static ((rec mocker-stub-record) newname &rest args)
  (let* ((obj (call-next-method)))
    (unless (slot-boundp obj :min-occur)
      (oset obj :min-occur 0))
    (unless (slot-boundp obj :max-occur)
      (oset obj :max-occur nil))
    obj))

(defmethod mocker-test-record ((rec mocker-stub-record) args)
  t)

(defmethod mocker-run-record ((rec mocker-stub-record) args)
  (oref rec :output))

(defmethod mocker-get-record-expectations ((rec mocker-stub-record))
  "anything")

;;; Helpers
(defun mocker-gen-mocks (mockspecs)
  "helper to generate mocks from the input of `mocker-let'"
  (mapcar #'(lambda (m)
              (let ((func (car m))
                    (argspec (cadr m))
                    (rest (cddr m)))
                (list (make-symbol (concat (symbol-name func) "--mock"))
                      (apply 'make-instance 'mocker-mock
                             :function func
                             :argspec argspec
                             (let* ((order (if (plist-member rest :ordered)
                                               (prog1
                                                   (plist-get rest :ordered)
                                                 (mocker--plist-remove
                                                  rest :ordered))
                                             (oref-default 'mocker-mock
                                                           :ordered))))
                               (list :ordered order)))
                      (if (plist-member rest :records)
                          (plist-get rest :records)
                        (car rest)))))
          mockspecs))

;;;###autoload
(defmacro mocker-let (mockspecs &rest body)
  (declare (indent 1) (debug t))
  (let* ((mocks (mocker-gen-mocks mockspecs))
         (vars (mapcar #'(lambda (m)
                           `(,(car m) ,(cadr m)))
                       mocks))
         (specs (mapcar
                 #'(lambda (m)
                     (let* ((mock-sym (car m))
                            (mock (cadr m))
                            (func (oref mock :function))
                            (spec (oref mock :argspec))
                            (args (loop for el in spec
                                        if (or (not (symbolp el))
                                               (not (equal
                                                     (elt (symbol-name el) 0)
                                                     ?&)))
                                        collect el)))
                       (list func
                             spec
                             `(mocker-run ,mock-sym ,@args))))
                 mocks))
         (inits (mapcar #'(lambda (m)
                            (cons 'progn
                                  (mapcar #'(lambda (rec)
                                              `(mocker-add-record ,(car m)
                                                              ,@rec))
                                          (nth 2 m))))
                        mocks))
         (verifs (mapcar #'(lambda (m)
                             `(mocker-verify ,(car m)))
                         mocks)))
    `(let (,@vars)
       (flet (,@specs)
         ,@inits
         (prog1
             (progn
               ,@body)
           ,@verifs)))))

(provide 'mocker)
;;; mocker.el ends here
