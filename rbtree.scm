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

(define-method dump-mermaid ((self <rbtree>))
  (define node-alist '())
  (define (new-label node)
    (let ((label #"l~(length node-alist)"))
      (push! node-alist (cons label node))
      label))
  (define (dump-a-node node parent-label)
    (let ((label (new-label node)))
      (if (is-a? node <rbtree-node>)
          (begin
            (format #t "  ~a{~a}~a~%" label (slot-ref node 'value)
                    (if (is-red? node) ":::red" ""))
            (let ((left  (slot-ref node 'left))
                  (right (slot-ref node 'right)))
              (when left
                (format #t "  ~a --> ~a~%" label (dump-a-node left label)))
              (when right
                (format #t "  ~a --> ~a~%" label (dump-a-node right label)))
              (when parent-label
                (format #t "  ~a --> ~a~%" label parent-label))))
          (format #t "  ~a[~a]~%" label node))
      label))
  (print "flowchart TB")
  (print "    classDef red fill:#faa;")
  (dump-a-node (slot-ref self 'root) #f))

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
  (define (set-parent! node parent)
    (when (is-a? node <rbtree-node>)
      (slot-set! node 'parent parent)))

  ;; Both "node" and "child" are red.
  (define (remedy-double-red! node child)
    (let ((parent (slot-ref node 'parent)))
      (if (not parent)
          ;; if node is root, just paint it black.
          (recolor-black! node)
          ;; otherwise, it is not simple.
          (let ((grandparent (slot-ref parent 'parent))
                (sibling     (let ((pl (slot-ref parent 'left))
                                   (pr (slot-ref parent 'right)))
                               (if (eq? pl node) pr pl))))
            (if (and (is-a? sibling <rbtree-node>)
                     (is-red? sibling))
                ;; paint red sibling black.
                (begin
                  (recolor-black! node)
                  (recolor-black! sibling)
                  (recolor-red! parent)
                  (when (and grandparent (is-red? grandparent))
                    (remedy-double-red! grandparent parent)))
                ;; reorder child, node, and parent.
                (let ((a  child)   ;; red
                      (b  node)    ;; red
                      (c  parent)  ;; black (because node is red)
                      (n< (lambda (x y) (eq? (compare-nodes self x y) 'less)))
                      (top #f))
                  (if (n< a b)
                      (if (n< b c)
                          (begin  ;; a < b < c
                            (set! top b)
                            (let ((b-r (slot-ref b 'right)))
                              (slot-set! b 'right c)
                              (slot-set! c 'parent b)
                              (slot-set! c 'left b-r)
                              (set-parent! b-r c))
                            (recolor-black! b)
                            (recolor-red! c))
                          (begin  ;; c < a < b
                            (set! top a)
                            (let ((a-l (slot-ref a 'left))
                                  (a-r (slot-ref a 'right)))
                              (slot-set! a 'left   c)
                              (slot-set! a 'right  b)
                              (slot-set! c 'parent a)
                              (slot-set! c 'right  a-l)
                              (set-parent! a-l c)
                              (slot-set! b 'parent a)
                              (slot-set! b 'left   a-r)
                              (set-parent! a-r b))
                            (recolor-black! a)
                            (recolor-red! c)))
                      (if (n< b c)
                          (begin  ;; b < a < c
                            (set! top a)
                            (let ((al (slot-ref a 'left))
                                  (ar (slot-ref a 'right)))
                              (slot-set! a 'left   b)
                              (slot-set! a 'right  c)
                              (slot-set! b 'parent a)
                              (slot-set! b 'right  al)
                              (set-parent! al b)
                              (slot-set! c 'parent a)
                              (slot-set! c 'left   ar)
                              (set-parent! ar c))
                            (recolor-black! a)
                            (recolor-red! c))
                          (begin  ;; c < b < a
                            (set! top b)
                            (let ((b-l (slot-ref b 'left)))
                              (slot-set! b 'left c)
                              (slot-set! c 'parent b)
                              (slot-set! c 'right b-l)
                              (set-parent! b-l c))
                            (recolor-black! b)
                            (recolor-red! c))))
                  (if grandparent
                      (if (eq? (slot-ref grandparent 'left) parent)
                          (slot-set! grandparent 'left  top)
                          (slot-set! grandparent 'right top))
                      (slot-set! self 'root top))
                  (slot-set! top 'parent grandparent)))))))

  ;; Note: leaf is (slot-ref node lr).
  (define (insert-new-value node value lr leaf)
    (let1 new-node (make-rbtree-red-node value node)
      (case (compare-values self value leaf)
        ((equal) #f)
        ((less)  (slot-set! new-node 'right leaf))
        ((more)  (slot-set! new-node 'left  leaf)))
      (slot-set! node lr new-node)
      (when (is-red? node)
        (remedy-double-red! node new-node))))

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

  (define (replace-with-only-child! node child)
    (let ((parent (slot-ref node 'parent)))
      ;; child has new parent (unless it is root)
      (when (is-a? child <rbtree-node>)
        (slot-set! child 'parent parent))
      (if (not parent)
          (slot-set! self 'root child)
          (begin
            (slot-set! parent
                       (if (eq? node (slot-ref parent 'left))
                           'left
                           'right)
                       child)
            (when (and (is-black? node) (is-black? parent))
              (remedy-double-black! parent))))))

  (define (convert-node-to-leaf! node)
    (let ((parent (slot-ref node 'parent)))
      (if (not parent)
          (slot-set! self 'root (slot-ref node 'value)) ;; node is root
          (begin
            (slot-set! parent
                       (if (eq? node (slot-ref parent 'left))
                           'left
                           'right)
                       (slot-ref node 'value))
            (when (and (is-black? node) (is-black? parent))
              (remedy-double-black! parent))))))

  (define (copy-and-delete-next! node)
    ;; assumes both children of `node` is <rbtree-node>.
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
                  (replace-with-only-child! child (slot-ref child 'right))))))))

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
                 ;; delete a node
                 (let ((left  (slot-ref node 'left))
                       (right (slot-ref node 'right))
                       (nvalue (slot-ref node 'value)))
                   (if (and left right)
                       (or (replace-with-value-child node left right)
                           (copy-and-delete-next! node))
                       (replace-with-only-child! node (or left right)))
                   nvalue)
                 ;; deleting value in a leaf (or not in the tree)
                 (if (eq? (slot-ref node match) value)
                     (begin
                       (if (slot-ref node (flip-left-right match))
                           ;; one child remains -> just remove the value
                           (slot-set! node match #f)
                           ;; no child -> convert node to value
                           (convert-node-to-leaf! node))
                       value)
                     #f  ;; value is not in the tree
                     )))))))

;; for test
(define-method check-invariants ((self <rbtree>))
  (define (check-node node)
    (define (check-child node child lr cmp)
      (let ((value (slot-ref node 'value)))
        (if (is-a? child <rbtree-node>)
            (begin
              (unless (eq? (compare-values self
                                           (slot-ref child 'value)
                                           value)
                           cmp)
                (error #"~lr value is not ~cmp"))
              (unless (eq? (slot-ref child 'parent) node)
                (error #"~|lr|'s parent is bad"))
              (when (and (is-red? node) (is-red? child))
                (errorf "double red ~a ~a" node child))
              (check-node child))
            (when child
              (unless (eq? (compare-values self child value) cmp)
                (error #"~lr value is not ~cmp"))))))
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
      (check-child node left  'left  'less)
      (check-child node right 'right 'more)
      ))

  (let1 root (slot-ref self 'root)
    (or (empty? self)
        (not (is-a? root <rbtree-node>))
        (check-node root))))
