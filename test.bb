(vector x1 y1 z1) + (vector x2 y2 z2) = vector (x1 + x2) (y1 + y2) (z1 + z2)

fib 0 = 0
fib 1 = 1
; fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)
; fib (n - 1) = fib (n - 2) + fib (n - 3)

fib 0
fib 1
fib 2
fib 3
fib 4
fib 5
fib 6
fib 7
fib 8
fib 9
fib 10
fib 11
fib 12
fib 13
fib 14
; fib 15
; fib 16
; fib 17
; fib 18
; fib 19
; fib 20

test true d= "Test passed"

"point tests"
point x y.
getX (point x y) = x
getY (point x y) = y
origin = point 0 0
getX origin
getY origin

"vector tests"
vector x y z = 0
; should this work???
; YES!!
format (vector x y z) = '<' + x + ', ' + y + ', ' + z + '>'
format ((vector 1 2 3) + (vector 4 5 6))

"fib str tests"

;str 0 = "0"
;str 1 = "1"

str 0 = "0"
; str 1 = "1"
; str (fib 0) = "0"
str (fib 1) = "1"
str (a + b) = "(" + str a + "+" + str b + ")"
; str fib n = "(" + str(fib(n - 1)) + "+" + str(fib(n - 2)) + ")"

; str (fib (2 - 1))

; str (fib 0)
; str (fib 1)
"2:"
fib 2
str (fib 2)

"4:"
fib 4
str (fib 4)

"5:"
fib 5
str (fib 5)

fib 6
str (fib 6)

fib 7
str (fib 7)

fib 8
str (fib 8)
