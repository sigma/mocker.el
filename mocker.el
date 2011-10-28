;;; mocker.el --- mocking framework for emacs

;; Copyright (C) 2011  Yann Hodique.

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

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

(require 'eieio)

(defclass mocker-mock ()
  ((function :initarg :function :type symbol)
   (argspec :initarg :argspec :initform nil :type list)
   (mode :initarg :mode :initform :ordered :type symbol)
   (records :initarg :records :initform nil :type list)
   (record-cls :initarg :record-cls :initform 'mocker-record :type symbol)))

(defmethod constructor :static ((mock mocker-mock) newname &rest args)
  (let* ((obj (call-next-method))
         (cls (oref obj :record-cls)))
    (oset obj :records (mapcar #'(lambda (r)
                                   (apply 'make-instance cls :-mock obj r))
                               (oref obj :records)))
    obj))

(defmethod mocker-fail-mock ((mock mocker-mock) args)
  (error (format (concat "Unexpected call to mock `%s'"
                         " with input `%s'")
                 (oref mock :function) args)))

(defmethod mocker-run ((mock mocker-mock) &rest args)
  (let ((rec (mocker-find-active-record mock args))
        (ordered (eq (oref mock :mode) :ordered)))
    (cond ((null rec)
           (mocker-fail-mock mock args))
          ((or (not ordered) (mocker-test-record rec args))
           (apply 'mocker-run-record rec args))
          (t
           (mocker-fail-record rec args)))))

(defmethod mocker-find-active-record ((mock mocker-mock) args)
  (let* ((ordered (eq (oref mock :mode) :ordered))
         rec)
    (if ordered
        (setq rec (some #'(lambda (r)
                            (when (oref r :-active)
                              (if (mocker-test-record r args)
                                  (progn
                                    (mocker-use-record r)
                                    r)
                                (if (>= (oref r :-occurrences)
                                        (oref r :min-occur))
                                    (oset r :-active nil)
                                  (mocker-fail-record r args)))))
                        (oref mock :records)))
      (setq rec (some #'(lambda (r)
                          (and
                           (mocker-is-active-record r)
                           (mocker-test-record r args)
                           (progn
                             (mocker-use-record r)
                             r)))
                      (oref mock :records))))
    rec))

(defmethod mocker-verify ((mock mocker-mock))
  (mapc #'(lambda (r) (when (and (oref r :-active)
                                 (< (oref r :-occurrences)
                                    (oref r :min-occur)))
                        (error (format (concat "Expected call to mock `%s'"
                                               " with input matching `%s'"
                                               " was not run.")
                                       (oref mock :function)
                                       (or (oref r :input-matcher)
                                           (oref r :input))))))
        (oref mock :records)))

(defclass mocker-record ()
  ((input :initarg :input :initform nil :type list)
   (output :initarg :output :initform nil)
   (input-matcher :initarg :input-matcher :initform nil)
   (output-generator :initarg :output-generator :initform nil)
   (min-occur :initarg :min-occur :initform 1 :type number)
   (max-occur :initarg :max-occur :initform 1 :type number)
   (-occurrences :initarg :-occurrences :initform 0 :type number
                 :protection :protected)
   (-mock :initarg :-mock)
   (-active :initarg :-active :initform t :protection :protected)))

(defmethod mocker-test-record ((rec mocker-record) args)
  (let ((matcher (oref rec :input-matcher))
        (input (oref rec :input)))
    (cond (matcher
           (apply matcher args))
          (t
           (equal input args)))))

(defmethod mocker-use-record ((rec mocker-record))
  (let ((max (oref rec :max-occur))
        (n (1+ (oref rec :-occurrences))))
    (oset rec :-occurrences n)
    (when (and (not (null max))
               (= n max))
      (oset rec :-active nil))))

(defmethod mocker-run-record ((rec mocker-record) &rest args)
  (let ((generator (oref rec :output-generator))
        (output (oref rec :output)))
    (cond (generator
           (apply generator args))
          (t
           output))))

(defmethod mocker-fail-record ((rec mocker-record) args)
  (error (format (concat "Violated record while mocking `%s'."
                         " Expected input matching: `%s', got: `%s' instead")
                 (oref (oref rec :-mock) :function)
                 (or (oref rec :input-matcher) (oref rec :input))
                 args)))

(defun mocker-gen-mocks (mockspecs)
  (mapcar #'(lambda (m)
              (apply 'make-instance 'mocker-mock
                     :function (car m)
                     :argspec (cadr m)
                     (cddr m)))
          mockspecs))

(defmacro mocker-let (mockspecs &rest body)
  (declare (indent 1) (debug t))
  (let* ((mocks (mocker-gen-mocks mockspecs))
         (specs (mapcar #'(lambda (m)
                            (let ((func (oref m :function))
                                  (spec (oref m :argspec)))
                              (list func
                                    spec
                                    `(mocker-run ,m ,@spec))))
                        mocks))
         (verifs (mapcar #'(lambda (m)
                             `(mocker-verify ,m))
                         mocks)))
    `(flet (,@specs)
       (prog1
           (progn
             ,@body)
         ,@verifs))))

(provide 'mocker)
;;; mocker.el ends here
