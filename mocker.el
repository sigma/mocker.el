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
                                   (apply 'make-instance cls r))
                               (oref obj :records)))
    obj))

(defmethod mocker-fail-mock ((mock mocker-mock))
  (error "invalid mock"))

(defmethod mocker-run ((mock mocker-mock) &rest args)
  (let (rec
        (ordered (eq (oref mock :mode) :ordered)))
    (if ordered
        (setq rec (some 'mocker-is-active-record
                       (oref mock :records)))
      (setq rec (some #'(lambda (r)
                          (and
                           (mocker-is-active-record r)
                           (mocker-test-record rec args)))
                      (oref mock :records))))
    (cond ((null rec)
           (mocker-fail-mock mock))
          ((and ordered (apply 'mocker-test-record rec args))
           (apply 'mocker-run-record rec args))
          (t
           (mocker-fail-record rec)))))

(defclass mocker-record ()
  ((input :initarg :input :initform nil :type list)
   (output :initarg :output :initform nil)
   (input-matcher :initarg :input-matcher :initform nil)
   (output-generator :initarg :output-generator :initform nil)
   (min-occur :initarg :min-occur :initform 1 :type number)
   (max-occur :initarg :max-occur :initform 1 :type number)
   (-occurrences :initarg :-occurrences :initform 0 :type number
                 :protection :protected)))

(defmethod mocker-test-record ((rec mocker-record) &rest args)
  (let ((matcher (oref rec :input-matcher))
        (input (oref rec :input)))
    (cond (matcher
           (apply matcher args))
          (t
           (equal input args)))))

(defmethod mocker-is-active-record ((rec mocker-record))
  (let ((n (oref rec :max-occur)))
    (and
     (or (null n)
         (> n (oref rec :-occurrences)))
     rec)))

(defmethod mocker-run-record ((rec mocker-record) &rest args)
  (oset rec :-occurrences (1+ (oref rec :-occurrences)))
  (let ((generator (oref rec :output-generator))
        (output (oref rec :output)))
    (cond (generator
           (apply generator args))
          (t
           output))))

(defmethod mocker-fail-record ((rec mocker-record))
  (error "invalid record"))

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
                        mocks)))
    `(flet (,@specs)
       ,@body)))

(provide 'mocker)
;;; mocker.el ends here
