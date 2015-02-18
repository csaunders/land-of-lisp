(load "svg_dsl")

(with-open-file
  (*standard-output* "random-walk.svg"
    :direction :output
    :if-exists :supersede)
  (svg 400 200
    (loop repeat 10
          do (polygon (append '((0 . 200))
                              (loop for x
                                    for y in (random-walk 100 400)
                                    collect (cons x y))
                              '((400 . 200)))
                      (loop repeat 3
                            collect (random 256))))))
