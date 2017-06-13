; lisp.bb

nil.
cons.
format (cons a b) = "(" + format(a) + " . " + format(b) + ")"
format %nil = "nil"
format _ = "" + _

"format test"
format (nil)
format (cons 1 (cons 2 (cons 3 nil)))


car (cons a b) = a
cdr (cons a b) = b
double a = a + a

mapcar ($fn (cons a b)) = (cons (fn a) (mapcar (fn b)))
mapcar ($fn %nil) = nil

"mapcar test"
format (mapcar (double nil))
format (mapcar (double (cons 1 (cons 2 (cons 3 nil)))))
