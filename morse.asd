;;;; -*- mode: lisp -*-

(defsystem :morse
  :name "morse"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Morse code library"
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "morse" :depends-on ("package"))))))
