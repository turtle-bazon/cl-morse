;;;; -*- mode: lisp -*-

(in-package #:ru.bazon.morse)

(defparameter *morse-tree*
  '(nil
    (#\e
     (#\i
      (#\s
       (#\h
        (#\5
         ()
         ())
        (#\4
         ()
         ()))
       (#\v
        ()
        (#\3
         ()
         ())))
      (#\u
       (#\f
        ()
        ())
       (nil
        (nil
         (#\?
          ()
          ())
         (#\_
          ()
          ()))
        ())))
     (#\a
      (#\r
       (#\l
        ()
        (nil
         (#\"
          ()
          ())
         ()))
       (nil
        (#\+
         ()
         (#\.
          ()
          ()))
        ()))
      (#\w
       (#\p
        (nil
         ()
         ())
        (nil
         (#\@
          ()
          ())
         ()))
       (#\j
        (nil
         ()
         ())
        (#\1
         (#\'
          ()
          ())
         ())))))
    (#\t
     (#\n
      (#\d
       (#\b
        (#\6
         ()
         (#\-
          ()
          ()))
        (#\=
         ()
         ()))
       (#\x
        (#\/
         ()
         ())
        ()))
      (#\k
       (#\c
        (nil
         ()
         ())
        (nil
         (#\;
          ()
          ())
         (#\!
          ()
          ())))
       (#\y
        (nil
         (#\(
          ()
          ())
         (#\)
          ()
          ()))
        ())))
     (#\m
      (#\g
       (#\z
        (#\7
         ()
         ())
        (nil
         ()
         (#\,
          ()
          ())))
       (#\q
        (nil
         ()
         ())
        (nil
         ()
         ())))
      (#\o
       (nil
        (#\8
         (#\:
          ()
          ())
         ())
        ())
       (nil
        (#\9
         ()
         ())
        (#\0
         ()
         ())))))))

(defun nleft (node)
  (car (cdr node)))

(defun nright (node)
  (car (cdr (cdr node))))

(defun nvalue (node)
  (car node))

(defun morse-map (node bag)
  (let* ((value (nvalue node))
         (left (nleft node))
         (right (nright node))
         (map-value (cons value (map 'string #'identity (reverse bag)))))
    (concatenate 'list
                 (if value (list map-value))
                 (if right (morse-map right (cons #\- bag)))
                 (if left (morse-map left (cons #\. bag))))))

(defparameter *morse-map* (morse-map *morse-tree* '()))

(defun to-morse (string)
  (map
   'list
   (lambda (character)
     (cdr (assoc character *morse-map*)))
   (string-downcase string)))

(defun to-morse-string (string)
  (apply #'concatenate (cons 'string (cdr (reduce (lambda (c l)
                                                    (cons " " (cons c l)))
                                                  (to-morse string)
                                                  :initial-value '()
                                                  :from-end t)))))

(defun _morse-letter (morse-seq node)
  (let ((morse-symbol (car morse-seq)))
    (if (not morse-symbol)
        (or (nvalue node) #\Space)
        (_morse-letter (cdr morse-seq) (ecase morse-symbol
                                         (#\. (nleft node))
                                         (#\- (nright node))
                                         (#\Space '(#\Space)))))))

(defun _from-morse-seq (morse-seq bag-morse)
  (let ((morse-symbol (car morse-seq)))
    (case morse-symbol
      (#\Space (cons (_morse-letter  (reverse bag-morse) *morse-tree*)
                     (_from-morse-seq (cdr morse-seq) '())))
      ((#\. #\-) (_from-morse-seq (cdr morse-seq) (cons morse-symbol bag-morse)))
      (otherwise (list (_morse-letter (reverse bag-morse) *morse-tree*))))))

(defun from-morse-string (string)
  (map 'string #'identity (_from-morse-seq (map 'list #'identity string) '())))
