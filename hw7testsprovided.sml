(* University of Washington, Programming Languages, Homework 7
   hw7testsprovided.sml *)
(* Will not compile until you implement preprocess and eval_prog *)

(* These tests do NOT cover all the various cases, especially for intersection *)

use "hw7.sml";

(* Must implement preprocess_prog and Shift before running these tests *)

fun real_equal(x,y) = Real.compare(x,y) = General.EQUAL;

(* Preprocess tests *)
let
	val Point(a,b) = preprocess_prog(LineSegment(3.2,4.1,3.2,4.1))
	val Point(c,d) = Point(3.2,4.1)
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "preprocess converts a LineSegment to a Point successfully\n")
	else (print "preprocess does not convert a LineSegment to a Point succesfully\n")
end;

let 
	val LineSegment(a,b,c,d) = preprocess_prog (LineSegment(3.2,4.1,~3.2,~4.1))
	val LineSegment(e,f,g,h) = LineSegment(~3.2,~4.1,3.2,4.1)
in
	if real_equal(a,e) andalso real_equal(b,f) andalso real_equal(c,g) andalso real_equal(d,h)
	then (print "preprocess flips an improper LineSegment successfully\n")
	else (print "preprocess does not flip an improper LineSegment successfully\n")
end;

(* eval_prog tests with Shift*)
let 
	val Point(a,b) = (eval_prog (preprocess_prog (Shift(3.0, 4.0, Point(4.0,4.0))), []))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with empty environment worked\n")
	else (print "eval_prog with empty environment is not working properly\n")
end;

(* Using a Var *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with 'a' in environment is working properly\n")
	else (print "eval_prog with 'a' in environment is not working properly\n")
end;


(* With Variable Shadowing *)
let 
	val Point(a,b) = (eval_prog (Shift(3.0,4.0,Var "a"), [("a",Point(4.0,4.0)),("a",Point(1.0,1.0))]))
	val Point(c,d) = Point(7.0,8.0) 
in
	if real_equal(a,c) andalso real_equal(b,d)
	then (print "eval_prog with shadowing 'a' in environment is working properly\n")
	else (print "eval_prog with shadowing 'a' in environment is not working properly\n")
end;


val t0 = let
			val Point(x,y) = preprocess_prog(LineSegment(1.0,1.0,1.0,1.0))
		 in
		 	real_close(x,1.0) andalso real_close(y,1.0)
		 end

val t1 = let
	val LineSegment(t1x1,t1y1,t1x2,t1y2) = preprocess_prog(LineSegment(1.0,1.0,0.0,0.0))
	val LineSegment(t2x1,t2y1,t2x2,t2y2) = preprocess_prog(LineSegment(1.0,1.0,1.0,0.0))
	val LineSegment(t3x1,t3y1,t3x2,t3y2) = preprocess_prog(LineSegment(1.0,1.0,2.0,0.0))
	val LineSegment(t4x1,t4y1,t4x2,t4y2) = preprocess_prog(LineSegment(1.0,1.0,2.0,1.0))
	val LineSegment(t5x1,t5y1,t5x2,t5y2) = preprocess_prog(LineSegment(1.0,1.0,2.0,2.0))
	val LineSegment(t6x1,t6y1,t6x2,t6y2) = preprocess_prog(LineSegment(1.0,1.0,1.0,2.0))
	val LineSegment(t7x1,t7y1,t7x2,t7y2) = preprocess_prog(LineSegment(1.0,1.0,0.0,2.0))
	val LineSegment(t8x1,t8y1,t8x2,t8y2) = preprocess_prog(LineSegment(1.0,1.0,0.0,1.0))
	val t1 = real_close(t1x1,0.0) andalso real_close(t1y1,0.0) andalso real_close(t1x2,1.0) andalso real_close(t1y2, 1.0)
	val t2 = real_close(t2x1,1.0) andalso real_close(t2y1,0.0) andalso real_close(t2x2,1.0) andalso real_close(t2y2, 1.0)
	val t3 = real_close(t3x1,1.0) andalso real_close(t3y1,1.0) andalso real_close(t3x2,2.0) andalso real_close(t3y2, 0.0)
	val t4 = real_close(t4x1,1.0) andalso real_close(t4y1,1.0) andalso real_close(t4x2,2.0) andalso real_close(t4y2, 1.0)
	val t5 = real_close(t5x1,1.0) andalso real_close(t5y1,1.0) andalso real_close(t5x2,2.0) andalso real_close(t5y2, 2.0)
	val t6 = real_close(t6x1,1.0) andalso real_close(t6y1,1.0) andalso real_close(t6x2,1.0) andalso real_close(t6y2, 2.0)
	val t7 = real_close(t7x1,0.0) andalso real_close(t7y1,2.0) andalso real_close(t7x2,1.0) andalso real_close(t7y2, 1.0)
	val t8 = real_close(t8x1,0.0) andalso real_close(t8y1,1.0) andalso real_close(t8x2,1.0) andalso real_close(t8y2, 1.0)
in
	[t1,t2,t3,t4,t5,t6,t7,t8]
end