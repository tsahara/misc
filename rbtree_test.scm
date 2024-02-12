(use gauche.test)

(test-start "rbtree")
(load "./rbtree.scm")

(test* "" #t (empty? (make <rbtree>)))
(test* "" #f (find (make <rbtree>) 1))


(test-section "insert")
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
  (test* "5 3 1 (5)" #t (find t 5))

  (insert! t 2)
  (test* "5 3 1 2 (2)" #t (find t 2))
  )

(print "* rebalance")
(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(4 2 3 1)))
(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(4 3 1 2)))
(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(1 2 4 3)))
(let1 t (make <rbtree>) (for-each (^n (insert! t n)) '(1 4 2 3)))


(test-section "delete")

(define (rbtree-inserted . vals)
  (let1 t (make <rbtree>)
    (dolist (i vals)
      (insert! t i))
    t))

(test* "delete from empty rbtree" #f (delete! (make <rbtree>) 1))
(test* "delete root" 1 (delete! (rbtree-inserted 1) 1))

(test* "delete not root" #f (delete! (rbtree-inserted 1) 2))
(test* "delete 1 twice"  #f (let1 t (rbtree-inserted 1)
                              (delete! t 1)
                              (delete! t 1)))
(test* "root node -> value" #t
       (let1 t (rbtree-inserted 2 1 3)
         (delete! t 1)
         (delete! t 3)
         (find t 2)))

(let1 t (rbtree-inserted 6 3 2 4 1 5 7 80 90 100)
  (check-invariants t)
  (dump t)
  )


(test-end :exit-on-failure 1)
