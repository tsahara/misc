(use gauche.test)

(test-start "rbtree")
(load "./rbtree.scm")

(test* "" #t (empty? (make <rbtree>)))

(test* "" #f (find (make <rbtree>) 1))

(let1 t (make <rbtree>)
  (insert! t 5)
  (test* "5 found" #t (find t 5))
  (test* "5 notfound" #f (find t 3))

  (insert! t 3)
  (test* "5 3" #t (find t 3))
  (test* "5 3 found" #t (find t 5))
  (test* "5 3 notfound" #f (find t 7))

  (insert! t 1)
  (test* "5 3 1 (3)" #t (find t 3))
  (test* "5 3 1 (1)" #t (find t 1))

  (insert! t 2)
  (test* "5 3" #t (find t 2))
  )

(let1 t (make <rbtree>)
  (check-invariants t)
  (insert! t 4)
  (check-invariants t)
  (insert! t 2)
  (check-invariants t)
  (insert! t 3)
  (check-invariants t)
  (insert! t 1)
  (check-invariants t)
  )

(print "* rebalance")
#;(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(4 2 3 1))
      (check-invariants t))
(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(4 3 1 2)))
(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(1 2 4 3)))
(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(1 4 2 3)))

(test-end :exit-on-failure 1)