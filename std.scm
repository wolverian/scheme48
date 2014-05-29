(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)
(define (id obj) obj)
(define (flip f) (lambda (a b) (f b a)))
(define (curry f a) (lambda (b) (apply f (list a b))))
(define (compose f g) (lambda (x) (f (apply g x))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define even? (compose not odd?))

(define (foldr f end xs)
  (if (null? xs)
    end
    (f (car xs) (foldr f end (cdr xs)))))

(define (foldl f accum lst)
  (if (null? lst)
    accum
    (foldl f (f accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold f init stop?)
  (if (stop? init)
    (cons init '())
    (cons init (unfold f (f init) stop?))))

(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))

(define (max first . rest)
  (fold (lambda (old new)
          (if (> old new) old new))
        first
        rest))
(define (min first . rest)
  (fold (lambda (old new)
          (if (< old new) old new))
        first
        rest))

(define (length lst)
  (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op)
  (lambda (acc next)
    (if (and (not acc)
             (pred (op next)))
      next
      acc)))

(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map f lst) (foldr (lambda (x xs) (cons (f x) xs)) '() lst))
(define (filter keep? lst)
  (foldr (lambda (x xs)
           (if (keep? x)
             (cons x xs)
             xs))
         '()
         lst))

