;;;; 
;;;; timer.asd
;;;; 
;;;; Created: 2003-11-08 by Zach Beane <xach@xach.com>
;;;; 
;;;; timer.asd,v 1.4.4.1 2003/11/19 17:26:37 xach Exp

(defpackage #:timer-system
  (:use #:cl #:asdf))

(in-package #:timer-system)

(defsystem #:timer
    :name "timer"
    :author "Zach Beane <xach@xach.com>"
    :maintainer "Alexander Vynnyk <cosmonaut.ok@zoho.com>"
    :version "0.4.1"
    :depends-on (#-sbcl :bordeaux-threads)
    :components ((:file "package")
		 (:file "time" :depends-on ("package"))
		 (:file "timers" :depends-on ("package" "time"))
		 (:file "queue" :depends-on ("package"))
		 (:file "scheduler" :depends-on ("package" "time"
						 "queue" "timers"))))


    
  
