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

(define (test-insert-rebalance . vals)
  (let1 t (make <rbtree>)
    (for-each (^n (insert! t n)) vals)
    (check-invariants t)))

(test-section "insert with rebalance")
(test-insert-rebalance 4 2 3 1)
(test-insert-rebalance 4 3 1 2)
(test-insert-rebalance 1 2 4 3)
(test-insert-rebalance 1 4 2 3)
(test-insert-rebalance 6 3 2 4 1 5 7 8 9 10 11 12 13 14 15)


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

(test* "delete leaf value (with sibling)" 1 (let1 t (rbtree-inserted 2 1 3)
                                             (delete! t 1)))
(test* "delete leaf value (without sibling)" 1 (let1 t (rbtree-inserted 2 1)
                                                 (delete! t 1)))
(test* "delete not leaf value" #f (let1 t (rbtree-inserted 2 1 3)
                                   (delete! t 4)))

(test* "delete root with only child" 1 (let1 t (rbtree-inserted 1 2)
                                         (delete! t 1)))
(test* "delete node with only child" 5 (let1 t (rbtree-inserted 1 2 3 4 5)
                                         (delete! t 5)))
(test* "delete root has two values"
       2 (let1 t (rbtree-inserted 1 2 3) (delete! t 2)))
(test* "delete node has value and node"
       5 (let1 t (rbtree-inserted 1 2 3 4 5 6 7 8) (delete! t 5)))

(let1 t (rbtree-inserted 6 3 2 4 1 5 7 8 9 10 11 12 13 14 15)
  (test* "delete a node with two children, replaced with a leaf value"
         4 (delete! t 4)))

(let1 t (rbtree-inserted 6 3 2 4 1 5 7 8 9 10 11 12 13 14 15)
  (delete! t 5)
  (test* "delete a node with two children, replaced with a node"
         4 (delete! t 4)))

(let1 t (rbtree-inserted 1 2 3 4 5 6 7 8 9 10 11)
  (delete! 1)
  (delete! 2)
  (dump-mermaid t)
  (dump t)
  (check-invariants t))


;;(let1 t (rbtree-inserted 6 3 2 4 1 5 7 8 9 10 11 12 13 14 15)
;;  (dump-mermaid t)
;;  (check-invariants t))


(test-end :exit-on-failure 1)
