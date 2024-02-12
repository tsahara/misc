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

(define (flip-left-right lr)
  (if (eq? lr 'left) 'right 'left))

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
    (let ((left   (slot-ref node 'left))
          (right  (slot-ref node 'right))
          (parent (slot-ref node 'parent)))
      (format #t "  ~a -> l=~a r=~a p=~s~%" node left right parent)
      (if (and left  (is-a? left  <rbtree-node>)) (dump-node left))
      (if (and right (is-a? right <rbtree-node>)) (dump-node right))))

  (print #"dumping ~self")
  (let ((root (slot-ref self 'root)))
    (if (is-a? root <rbtree-node>)
        (dump-node root)
        (format #t "  root = ~s~%" root))))

(define-method empty? ((self <rbtree>))
  (eq? (slot-ref self 'root) #f))

;; -> (node match)
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
  (let ((root (slot-ref self 'root)))
    (cond ((not root) #f)
          ((not (is-a? root <rbtree-node>)) (eq? root value))
          (else
           (receive (node match)
               (find-node self value)
             (and node
                  (or (eq? match 'found)
                      (and-let* ((lv (slot-ref node match)))
                        (eq? (compare-values self value lv) 'equal)))))))))

(define-method insert! ((self <rbtree>) value)
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
      (while (and (is-red? node) (slot-ref node 'parent))
        (let* ((parent (slot-ref node 'parent))
               (sibling (let ((pl (slot-ref parent 'left))
                              (pr (slot-ref parent 'right)))
                          (if (eq? pl node) pr pl))))
          (if (and (is-a? sibling <rbtree-node>)
                   (is-red? sibling))
              ;; recolor red sibling
              (begin
                (recolor-black! new-node)
                (recolor-black! sibling)
                (recolor-red! parent)
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

  (let1 root (slot-ref self 'root)
    (cond ((not root)
           (slot-set! self 'root value))

          ((not (is-a? root <rbtree-node>))
           (let1 new-root (make-rbtree-black-node root #f)
             (slot-set! self 'root new-root)
             (case (compare-values self root value)
               ((equal) #f)
               ((less)  (begin
                          (slot-set! new-root 'right value)
                          #t))
               ((more)  (begin
                          (slot-set! new-root 'left  value)
                          #t)))))
          (else
           (receive (node match)
               (find-node self value)
             (if (eq? match 'found)
                 #f
                 (if-let1 leaf (slot-ref node match)
                   (insert-new-value node value match leaf)
                   (slot-set! node match value))))))))

(define-method delete! ((self <rbtree>) value)
  (define (replace-with-value-child node left right)
    (cond ((not (is-a? left <rbtree-node>))
           (slot-set! node 'value left)
           (slot-set! node 'left #f)
           #t)
          ((not (is-a? right <rbtree-node>))
           (slot-set! node 'value right)
           (slot-set! node 'right #f)
           #t)
          (else #f)))

  (define (replace-with-a-child node child)
    (let ((parent (slot-ref node 'parent)))
      (slot-set! parent
                 (if (eq? node (slot-ref parent 'left))
                     'left
                     'right)
                 child)
      (when (is-a? child <rbtree-node>)
        (slot-set! child 'parent parent))
      (when (and (is-black? node) (is-black? parent))
        (remedy-double-black! parent))))

  (define (convert-node-to-leaf! node)
    (let ((parent (slot-ref node 'parent)))
      (if parent
          (begin
            (slot-set! parent
                       (if (eq? node (slot-ref parent 'left))
                           'left
                           'right)
                       (slot-ref node 'value))
            (when (and (is-black? node) (is-black? parent))
              (remedy-double-black! parent)))
          (begin ;; node is root
            (slot-set! self 'root (slot-ref node 'value))))))

  (define (copy-and-delete-next! node)
    ;; assumes right child of `node` is <rbtree-node>.
    (let loop ((child (slot-ref node 'right)))
      (let ((left (slot-ref child 'left)))
        (if (is-a? left <rbtree-node>)
            (loop left)
            (if left
                (begin  ;; has a left value
                  (slot-set! node 'value left)
                  (slot-set! child 'left #f)
                  (unless (slot-ref child 'right)
                    (convert-node-to-leaf! child)))
                (begin  ;; does not have a left child
                  (slot-set! node 'value (slot-ref child 'value))
                  (replace-with-a-child child (slot-ref child 'right))))))))

  (define (remedy-double-black! node)
    (errorf "remedy ~s" node)
    )

  (let ((root (slot-ref self 'root)))
    (cond ((not root) #f) ;; tree is empty

          ((not (is-a? root <rbtree-node>)) ;; root is a value
           (if (eq? root value)
               (begin
                 (slot-set! self 'root #f)
                 value)
               #f))

          (else ;; root is a node.
           (receive (node match)
               (find-node self value)
             (if (eq? match 'found)
                 ;; deleting value in a node.
                 (let ((left  (slot-ref node 'left))
                       (right (slot-ref node 'right))
                       (nvalue (slot-ref node 'value)))
                   (if (and left right)
                       (or (replace-with-value-child node left right)
                           (copy-and-delete-next! node))
                       (replace-with-a-child node (or left right)))
                   nvalue)
                 ;; deleting value in a leaf (or not in the tree)
                 (if (eq? (slot-ref node match) value)
                     (if (slot-ref node (flip-left-right match))
                         (begin ;; just remove the value
                           (slot-set! node match #f)
                           value)
                         (convert-node-to-leaf! node))
                     #f  ;; value is not in the tree
                     )))))))

;; for test
(define-method check-invariants ((self <rbtree>))
  (define (check-node node)
    (let ((parent (slot-ref node 'parent))
          (left   (slot-ref node 'left))
          (right  (slot-ref node 'right))
          (value  (slot-ref node 'value))
          (n<     (lambda (x y) (eq? (compare-nodes self x y) 'less))))
      (if (eq? node (slot-ref self 'root))
          (when parent
            (error #"root's parent is not #f (but ~s)" parent))
          (when (not parent)
            (error #"~node parent is #f")))
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

  (let1 root (slot-ref self 'root)
    (or (empty? self)
        (not (is-a? root <rbtree-node>))
        (check-node root))))
