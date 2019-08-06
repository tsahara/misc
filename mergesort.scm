(use scheme.vector)

(define (merge-sort! vec)
  (define cmp string<?)
  (define (merge! vec left mid right)
    (define vec-copy (vector-copy vec left (+ mid 1)))
    (define (get-left l)
      (if (<= l mid)
	  (vector-ref vec-copy (- l left))
	  #f))
    (define (get-right r)
      (if (<= r right)
	  (vector-ref vec r)
	  #f))
    (let loop ((i           left)
	       (left-index  left)
	       (right-index (+ mid 1)))
      (let ((lval (get-left left-index))
	    (rval (get-right right-index)))
	#;(format #t "  i=~a lindex=~a rindex=~a lval=~a rval=~a\n"
		i left-index right-index lval rval)
	(cond ((and lval rval)
	       (if (not (cmp rval lval))
		   (begin
		     (vector-set! vec i lval)
		     (loop (+ i 1) (+ left-index 1) right-index))
		   (begin
		     (vector-set! vec i rval)
		     (loop (+ i 1) left-index (+ right-index 1)))))
	      ;; no more right values
	      (lval (begin
		      (vector-set! vec i lval)
		      (loop (+ i 1) (+ left-index 1) right-index)))
	      ;; no more left values
	      (rval (begin
		      (vector-set! vec i rval)
		      (loop (+ i 1) left-index (+ right-index 1))))

	      ;; no values
	      (else #f)))))
  (define (sub vec left right)
    #;(format #t "left=~a, right=~a, vec=~a\n"
	    left right (vector-copy vec left (+ right 1)))
    (if (< left right)
	(if (= (+ left 1) right)
	    (if (cmp (vector-ref vec right) (vector-ref vec left))
		(vector-swap! vec left right))
	    (let1 mid (div (+ left right) 2)
	      (sub vec left mid)
	      (sub vec (+ mid 1) right)
	      #;(format #t "(merge! ~a ~a ~a) vec=~a\n"
		      left mid right (vector-copy vec left (+ right 1)))
	      (merge! vec left mid right))))
    #;(format #t "=> ~a\n"
	    (vector-copy vec left (+ right 1))))
  (sub vec 0 (- (vector-length vec) 1)))

(define (selection-sort! vec )
  (define cmp string<?)
  (define (find-min start)
    (let loop ((min start)
	       (j   (+ start 1)))
      (if (< j (vector-length vec))
	  (if (cmp (vector-ref vec min) (vector-ref vec j))
	      (loop min (+ j 1))
	      (loop j   (+ j 1)))
	  min)))
  (if (> (vector-length vec) 1)
      (dotimes (i (vector-length vec))
	(let1 min (find-min i)
	  (if (not (= i min))
	      (vector-swap! vec i min))))))

(call-with-input-file "0"
  (^p (let1 v (list->vector (port->string-list p))
	(merge-sort! v)
	(vector-for-each print v))))
