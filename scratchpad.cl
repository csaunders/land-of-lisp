(reduce (lambda (best item)
  (if (and (evenp item) (> item best))
    item
    best))
  '(7 4 6 5 2)
  :initial-value 0)

(defun sum (lst)
  (reduce #'+ lst))

(map 'list
  (lambda (x)
    (if (eq x #\s)
      #\S
      x))
  "this is a string")

(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((and (listp a) (listp b)) (append a b))))

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(loop repeat 10 do
  (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
    (random-animal)
    (random-animal)
    (random-animal)))

(defun random-animals (size)
  (loop repeat size collect (random-animal)))
