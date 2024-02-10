;; TODO
;; - (delete! rbtree value)

(define-class <rbtree> ()
  (root compare))

(define-method initialize ((self <rbtree>) initargs)
  (next-method)
  (let-optionals* initargs ((compare (lambda (a b)
                                       (cond ((= a b) 'equal)
                                             ((< a b) 'less)
                                             (else    'more)))))
    (slot-set! self 'root #f)
    (slot-set! self 'compare compare)))

(define-class <rbtree-node> ()
  ;; 'value cannot be #f.
  ;; 'left and 'right can be: <rbtree-node>, a value, #f (no value).
  ;; 'parent is <rbtree-node> or #f.
  ;; 'red is a boolean.
  (value left right parent red))

(define-method initialize ((self <rbtree-node>) initargs)
  (next-method)
  (slot-set! self 'value  (car initargs))
  (slot-set! self 'parent (cadr initargs))
  (slot-set! self 'left   #f)
  (slot-set! self 'right  #f)
  (slot-set! self 'red    (boolean (caddr initargs))))

(define-method is-black? ((self <rbtree-node>))
  (not (is-red? self)))

(define-method is-red? ((self <rbtree-node>))
  (slot-ref self 'red))

(define-method recolor-black! ((self <rbtree-node>))
  (slot-set! self 'red  #f))

(define-method recolor-red! ((self <rbtree-node>))
  (slot-set! self 'red  #t))

(define-method write-object ((self <rbtree-node>) out)
  (format out "#<rbtree-node v=~s ~a>"
          (slot-ref self 'value)
          (if (is-red? self) "red" "black")))


(define (make-rbtree-black-node value parent)
  (make <rbtree-node> value parent #f))

(define (make-rbtree-red-node value parent)
  (make <rbtree-node> value parent #t))

;; returns 'equal, 'less, or 'more.
;;   a = b => 'equal
;;   a < b => 'less
;;   a > b => 'more
(define-method compare-nodes ((self <rbtree>) a b)
  ((slot-ref self 'compare) (slot-ref a 'value) (slot-ref b 'value)))

(define-method compare-values ((self <rbtree>) a b)
  ((slot-ref self 'compare) a b))

(define-method dump ((self <rbtree>))
  (define (dump-node node)
    (let ((left  (slot-ref node 'left))
          (right (slot-ref node 'right)))
      (format #t "  ~a -> l=~a r=~a~%" node left right)
      (if (and left  (is-a? left  <rbtree-node>)) (dump-node left))
      (if (and right (is-a? right <rbtree-node>)) (dump-node right))))

  (print #"dumping ~self")
  (dump-node (slot-ref self 'root)))

(define-method empty? ((self <rbtree>))
  (eq? (slot-ref self 'root) #f))

;; -> (values node match)
;;    match := (enum 'found 'left 'right)
(define-method find-node ((self <rbtree>) value)
  (let loop ((node (slot-ref self 'root)))
    (if (not node)
        (values #f #f)
        (let1 r (compare-values self value (slot-ref node 'value))
          (if (eq? r 'equal)
              (values node 'found)
              (let* ((lr    (if (eq? r 'less) 'left 'right))
                     (child (slot-ref node lr)))
                (if (is-a? child <rbtree-node>)
                    (loop child)
                    (values node lr))))))))

(define-method find ((self <rbtree>) value)
  (receive (node match)
      (find-node self value)
    (and node
         (or (eq? match 'found)
             (and-let* ((lv (slot-ref node match)))
               (eq? (compare-values self value lv) 'equal))))))

(define-method insert! ((self <rbtree>) value)
  (define (flip-left-right lr)
    (if (eq? lr 'left) 'right 'left))

  ;; Note: leaf is (slot-ref node lr).
  (define (insert-new-value node value lr leaf)
    ;; insert a new red node.
    (let1 new-node (make-rbtree-red-node value node)
      (case (compare-values self value leaf)
        ((equal) #f)
        ((less)  (slot-set! new-node 'right leaf))
        ((more)  (slot-set! new-node 'left  leaf)))
      (slot-set! node lr new-node)

      ;; if node is red, we must restore an invariant.
      (while (is-red? node)
        (let* ((parent  (slot-ref node 'parent))
               (sibling (let ((pl (slot-ref parent 'left))
                              (pr (slot-ref parent 'right)))
                          (if (eq? pl node) pr pl))))
          (if (and (is-a? sibling <rbtree-node>)
                   (is-red? sibling))
              ;; recolor red sibling
              (begin
                (recolor-black! new-node)
                (recolor-black! sibling)
                (recolor-red! parent) ; parent was black because sibling was red
                ;; if both parent and grandparent are red, we have a broken
                ;; invariant to fix recursively.
                (set! new-node node)
                (set! node parent))
              ;; swap nodes
              (let ((a  new-node)  ;; red
                    (b  node)      ;; red
                    (c  parent)    ;; black
                    (n< (lambda (x y) (eq? (compare-nodes self x y) 'less)))
                    (top #f)
                    (grandparent (slot-ref parent 'parent)))
                (if (n< a b)
                    (if (n< b c)
                        (begin  ;; a < b < c
                          (print "  swap a < b < c")
                          (set! top b)
                          (slot-set! c 'left   (slot-ref b 'right))
                          (slot-set! b 'right  c)
                          (slot-set! c 'parent b)
                          (recolor-black! b)
                          (recolor-red! c))
                        (begin  ;; c < a < b
                          (print "  swap c < a < b")
                          (set! top a)
                          (slot-set! c 'right  (slot-ref a 'left))
                          (slot-set! b 'left   (slot-ref a 'right))
                          (slot-set! a 'left   c)
                          (slot-set! a 'right  b)
                          (slot-set! b 'parent a)
                          (slot-set! c 'parent a)
                          (recolor-black! a)
                          (recolor-red! c)))
                    (if (n< b c)
                        (begin  ;; b < a < c
                          (print "  swap b < a < c")
                          (set! top a)
                          (slot-set! b 'right  (slot-ref a 'left))
                          (slot-set! c 'left   (slot-ref a 'right))
                          (slot-set! a 'left   b)
                          (slot-set! a 'right  c)
                          (slot-set! b 'parent a)
                          (slot-set! c 'parent a)
                          (recolor-black! a)
                          (recolor-red! c))
                        (begin  ;; c < b < a
                          (print "  swap c < b < a")
                          (set! top b)
                          (slot-set! c 'right  (slot-ref b 'left))
                          (slot-set! b 'left   c)
                          (slot-set! c 'parent b)
                          (recolor-black! b)
                          (recolor-red! c))))
                (if grandparent
                    (if (eq? (slot-ref grandparent 'left) parent)
                        (slot-set! grandparent 'left  top)
                        (slot-set! grandparent 'right top))
                    (slot-set! self 'root top))
                (slot-set! top 'parent grandparent)
                (set! node top)
                ))))))

  (if (empty? self)
      (slot-set! self 'root (make-rbtree-black-node value #f))
      (receive (node match)
          (find-node self value)
        (if (eq? match 'found)
            #f
            (if-let1 leaf (slot-ref node match)
              (insert-new-value node value match leaf)
              (slot-set! node match value))))))

;; for test
(define-method check-invariants ((self <rbtree>))
  (define (check-node node)
    (let ((left  (slot-ref node 'left))
          (right (slot-ref node 'right))
          (value (slot-ref node 'value))
          (n<    (lambda (x y) (eq? (compare-nodes self x y) 'less))))
      (when left
        (if (is-a? left <rbtree-node>)
            (begin
              (unless (n< left node)
                (error "left node is not smaller"))
              (unless (eq? (slot-ref left 'parent) node)
                (error "left's parent is bad"))
              (check-node left))
            (unless (eq? (compare-values self left value) 'less)
                (error "left value is not smaller"))))
      (when right
        (if (is-a? right <rbtree-node>)
            (begin
              (unless (n< node right)
                (error "right node is not larger"))
              (unless (eq? (slot-ref right 'parent) node)
                (error "right's parent is bad"))
              (check-node right))
            (unless (eq? (compare-values self right value) 'more)
              (error "right value is not larger")))
        )))

  (unless (empty? self)
    (let1 root (slot-ref self 'root)
      (unless (eq? (slot-ref root 'parent) #f)
        (error "root's parent is not #f"))
      (check-node root))))
