#!/usr/bin/sbcl --script
# -*- coding: utf-8 -*-
(ql:quickload '() :silent t)
(defpackage :myapp
  (:use :cl)
  (:export :main))			; => #<PACKAGE "MYAPP">
(in-package :myapp)			; => #<PACKAGE "MYAPP">

;; pointの構造体定義
(defstruct pt
  x
  y
  z)					; => PT


;; p1とp2のインスタンスを作成
(setq p0 (make-pt :x 0 :y 0 :z 0))	; => #S(PT :X 0 :Y 0 :Z 0)
(setq p1 (make-pt :x 1 :y 5 :z 7))	; => #S(PT :X 1 :Y 5 :Z 7)
(setq p2 (make-pt :x 1 :y 3 :z 0))	; => #S(PT :X 1 :Y 3 :Z 0)


(defstruct ln
  p1
  p2
  length)				; => LN
(setq l1 (make-ln :p1 p0 :p2 p1 :length (llen p1 p2)))


;;ベクターの構造体を定義
(defstruct vec
  strt
  end
  comp
  size)					; => VEC

;; (defun vec (p1 p2)
;;   (cons (abs (- (pt-x p1) (pt-x p2)))
;;    (abs (- (pt-y p1) (pt-y p2)))))	; => VEC

(defun vec (p1 p2)
  (vector (abs (- (pt-x p1) (pt-x p2)))
	  (abs (- (pt-y p1) (pt-y p2)))
	  (abs (- (pt-z p1) (pt-z p2)))
	  ))				; => VEC

(vec p1 p2)				; => #(1 2 0 0)
(aref (vec p1 p2) 1)			; => 2


(defun v-size (v)
  (sqrt (+ (expt (aref v 0) 2)
	   (expt (aref v 1) 2)
	   (expt (aref v 2) 2)
	   )))				; => V-SIZE
(let ((*read-default-float-format* 'double-float))
  (v-size (vec p0 p1)))			; => 8.6602545


(setf v1 (make-vec :strt p0 :end p1
		   :comp (vec p0 p1)
		   :size (v-size (vec p0 p1)))) ; =>
#S(VEC
   :STRT #S(PT :X 0 :Y 0 :Z 0 :W 0)
   :END #S(PT :X 2 :Y 1 :Z 0 :W 0)
   :COMP #(2 1 0 0)
   :SIZE 2.236068)

(setf v2 (make-vec :strt p0 :end P2
		   :comp (vec p0 p2)
		   :size (v-size (vec p0 p2)))) ; =>
#S(VEC
   :STRT #S(PT :X 0 :Y 0 :Z 0 :W 0)
   :END #S(PT :X 1 :Y 3 :Z 0 :W 0)
   :COMP #(1 3 0 0)
:SIZE 3.1622777)

(defun v+ (v1 v2)
  "ベクトルの和を求める関数"
  (let ((c1 (vec-comp v1))
	(c2 (vec-comp v2)))
    (vector (+ (aref c1 0) (aref c2 0))
	  (+ (aref c1 1) (aref c2 1))
	  (+ (aref c1 2) (aref c2 2))
	  (+ (aref c1 3) (aref c2 3))))) ; => V+

(defun v- (v1 v2)
  "ベクトルの差を求める関数"
  (let ((c1 (vec-comp v1))
	(c2 (vec-comp v2)))
    (vector (- (aref c1 0) (aref c2 0))
	    (- (aref c1 1) (aref c2 1))
	    (- (aref c1 2) (aref c2 2))
	    (- (aref c1 3) (aref c2 3))))) ; => V-

(v- v1 v2)				; => #(1 -2 0 0)
(v+ v1 v2)				; => #(3 4 0 0)

;; ベクトルの内積(inner product)
(defmethod in-prod1 ((v1 vec) (v2 vec))
  (let ((c1 (vec-comp v1))
	(c2 (vec-comp v2)))
    (+ (* (aref c1 0) (aref c2 0))
       (* (aref c1 1) (aref c2 1))
       (* (aref c1 2) (aref c2 2))
       (* (aref c1 3) (aref c2 3)))))	; => #<STANDARD-METHOD MYAPP::IN-PROD1 (VEC VEC) {100385B5B3}>

(in-prod v1 v2)				; => 

(/ (in-prod1 v1 v2) (* (abs (vec-size v1)) (abs (vec-size v2)))) ; => 0.70710677

(* (abs (vec-size v1)) (abs (vec-size v2)))	; => 7.071068
(/ 5 7.071068)					; => 0.70710677

(defun in-prod2 (v1 v2 theta)
  (* (abs (vec-size v1)) (abs (vec-size v2)) (cos theta))) ; => IN-PROD2
(in-prod v1 v2 0.70710677)				   ; => 5.375741
(cos 30)				       ; => 0.15425146
(vec-size v1)				       ; => 2.236068
(aref #(1 2 3) 1)			       ; => 2

;; ベクトルの外積(cross product)
(defun x-prod (v1 v2)
  (let ((c1 (vec-comp v1))
	(c2 (vec-comp v2)))
    (vector (- (* (aref c1 1) (aref c2 2))
	       (* (aref c2 1) (aref c1 2)))
	    (- (* (aref c1 2) (aref c2 0))
	       (* (aref c2 2) (aref c1 0)))
	    (- (* (aref c1 0) (aref c2 1))
	       (* (aref c2 0) (aref c1 1)))))) ; => X-PROD

;; 2つのベクトルで出来た平行四辺形の面積、あるいはｚ軸方向へのベクトルの大きさ
(x-prod v1 v2)					   ; => #(0 0 5)




