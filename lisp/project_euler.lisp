(defun init ()
  (load "~/desktop/project_euler.lisp"))

(defun next (strand)
  (cons (+ (first strand) (second strand)) strand))

(defun digit-length (x)
  (ceiling (log x 10)
   ))


(defun rec (strand)
  (if
   (= 1000 (digit-length (first strand)))
   (cons 
    (first strand)
    (length strand))
   (rec (next strand))))


(defun make-linear-congruential-generator (a b c)
  (let ((x 0))
    (lambda () 
      (setq x (mod (+ (* a x) b) (expt 2 c))) 
      (- x (expt 2 (- c 1))))))

(defun sum-of-lines (x)
  (* (1+ x) (/ x 2)))

;a triangle is an x value, a y value, and a size

(defun make-world-triangle (size fill-function) 
  (let* ((triangle-dimensions size)
	 (triangle-data (make-array (list (sum-of-lines triangle-dimensions)))))
     (dotimes (x (array-dimension triangle-data 0))
       (setf (aref triangle-data x) (funcall fill-function)))
     (labels ((get-item (x y) 
		(aref triangle-data (+ (sum-of-lines (- triangle-dimensions x)) y -1)))
	      (make-sub-triangle (x y size)
		(lambda (func-name &rest args)
		  (case func-name
		    (args args)
		    (get-x x)
		    (get-y y)
		    (get-size size)
		    (get (list x y size))
		    (x-vert  (get-item x (+ y size -1)))
		    (y-vert  (get-item (+ x size -1) y))
		    (xy-vert (get-item x y))	
		    (x-side 
		     (let ((total 0))
		       (dotimes (n (- size 2))
			 (setf total (+ total (get-item x (+ y n 1)))))
		       total))
		    (y-side 
		     (let ((total 0))
		       (dotimes (n (- size 2))
			 (setf total (+ total (get-item (+ x n 1) y))))
		       total))
		    (xy-side 
		     (let ((total 0))
		       (dotimes (n (- size 2))
			 (setf total (+ total (get-item (+ x size -2 (- n)) (+ y n 1)))))
		       total))
		    (xy-triangle 
		     (let ((delta (first args)))
		       (make-sub-triangle x y (+ delta size))))
		    (x-triangle 
		     (let ((delta (first args)))
		       (make-sub-triangle (- x delta) y (+ delta size))))
		    (y-triangle 
		     (let ((delta (first args)))
		       (make-sub-triangle x (- y delta) (+ delta size))))
		    (get-sum 
		     (let ((total 0))
		       (dotimes (dx size)
			 (dotimes (dy (- size dx))
			   (let ((delta (get-item (+ x dx) (+ y dy))))
			     (setf total (+ total delta)))))
		       total))
		    (validp  
		     (if (< 0 (min x y size))
			 (if (<= (+ x y size -2) triangle-dimensions)
			     t
			     nil)
			 nil))
		    (get-world (function world-gen)))))
	      (random-triangle () (apply (function make-sub-triangle) (butlast (partition (+ 2 size) 4))))
	      (world-gen (function-identifier &rest args)
		(case function-identifier
		  (args args)
		  (item (apply (function get-item) args))
		  (dimension triangle-dimensions)
		  (raw triangle-data)
		  (sub-triangle (apply (function make-sub-triangle) args))
		  (random-sub-triangle (random-triangle)))))
       (function world-gen)))) 

(defun triangle-relatives (tri)
  (let ((world (funcall tri 'get-world))
	(x (funcall tri 'get-x))
	(y (funcall tri 'get-y))
	(size (funcall tri 'get-size))
	(xy-collected nil)
	(x-collected nil)
	(y-collected nil)
	(sum-aux 0))
    (setf sum-aux 0) ;varying size in the xy direction
    (dotimes (n (- (funcall world 'dimension) -2 x y))
      (let* ((another-triangle (funcall world 'sub-triangle x y (+ 1 n)))
	     (expanse (+ (funcall another-triangle 'x-vert)
			 (if (= 0 n)
			     0
			     (funcall another-triangle 'y-vert))
			 (funcall another-triangle 'xy-side))))
	(setf sum-aux (+ sum-aux expanse))
	(push (list sum-aux another-triangle) xy-collected)))
    (setf sum-aux 0) ;varying size and x in the x direction
    (dotimes (n (+ size -1 x))
      (let* ((another-triangle (funcall world 'sub-triangle (+ x size -1 (- n)) y (+ 1 n)))
	     (expanse (+ (funcall another-triangle 'xy-vert)
			 (if (= 0 n)
			     0
			     (funcall another-triangle 'x-vert))
			 (funcall another-triangle 'x-side))))
	(setf sum-aux (+ sum-aux expanse))
	(push (list sum-aux another-triangle) x-collected))) 
    (setf sum-aux 0) ;varying size and y in the y direction
    (dotimes (n (+ size -1 y))
      (let* ((another-triangle (funcall world 'sub-triangle x (+ y size -1 (- n)) (+ 1 n)))
	     (expanse (+ (funcall another-triangle 'xy-vert)
			 (if (= 0 n)
			     0
			     (funcall another-triangle 'y-vert))
			 (funcall another-triangle 'y-side))))
	(setf sum-aux (+ sum-aux expanse))
	(push (list sum-aux another-triangle) y-collected)))
    (append xy-collected x-collected y-collected))) 

(defparameter alive t)

(defun traverse-relatives (tri &optional size)
  (if size
      nil
      (setf size (funcall tri 'get-sum)))
  (let* ((tri-gents (triangle-relatives tri))
	 (candidates-left (sort
			   (remove-if-not
			    (lambda (x) (< (car x) size)) 
			    tri-gents)
			   #'< :key #'car))
	 (candidate-ends nil))
    (if candidates-left
	(progn
	  (let ((candidate (car candidates-left)))
	    (if alive
		(if candidate
		    (traverse-relatives (cadr candidate) (car candidate)))))
	  candidate-ends)
	(progn 
	  (print size)
	  (list size tri)))))

(defun rand (&rest args)
  (+ 1 (apply #'random args)))

(defun distinct-random-ints (how-many range)
  (if (>= range how-many)
      (let ((collection nil))
	(labels ((add-num ()
		   (let ((newnum (rand range)))
		     (if (member newnum collection)
			 (add-num)
			 (push newnum collection)))))
	  (dotimes (n how-many)
	    (add-num)))
	collection)
      "impossible"))

(defun small-sort (seq)
  (sort seq #'<))

(defun large-sort (seq)
  (sort seq #'>))

(defun finite-difference (seq &optional chain)
  (if (cdr seq)
      (progn 
	(finite-difference (cdr seq) (cons (- (cadr seq) (car seq)) chain)))
      (nreverse chain)))

(defun partition (num parts)
  (finite-difference 
   (append
    (list 0)
    (small-sort 
     (distinct-random-ints (- parts 1) num))
    (list num))))

(defparameter world nil)
(let ((lcg (make-linear-congruential-generator 615949 797807 20)))
  (setf world (make-world-triangle 1000 lcg)))

(defun init ()
  (load "~/desktop/project_euler.lisp")
  (load "~/desktop/project_euler.lisp"))

(defparameter foo-triangles (funcall world 'random-sub-triangle))

(defparameter p8 (remove #\linefeed 
"73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"))

(defun f8 ()
  (let ((biggy 0))
    (dotimes (x 987)
      (let ((total 1))
	(dotimes (bar 13)
	  (setf total (* total (digit-char-p (aref p8 (+ x bar))))))
	(if (< biggy total)
	    (setf biggy total))))
    biggy))

(defun quadratic-formula (a b c)
  (let ((foo (- b))
	(discriminant (sqrt (- (* b b) (* 4 a c))))
	(denom (+ a a)))
    (values 
     (/ (+ foo discriminant) denom)
     (/ (- foo discriminant) denom))))

(defun hexnum (n)
  (* n (- (+ n n) 1)))

(defun pentnump (n)
  (intp (muntnep n)))

(defun pentnum (n)
  (* n (- (* 3 n) 1) 1/2))

(defun muntnep (n)
    (quadratic-formula 3 -1 (- (* 2 n))))

(defun trinum (a)
  (/ (* a (+ a 1)) 2))

(defun munirt (n)
  (quadratic-formula 1 1 (- 0 n n)))

(defun intp (n)
  (let* ((baz (multiple-value-list (floor n)))
	 (num (first baz))
	 (crust (second baz)))
    (if (= crust 0)
	num)))

(defun f45 (start amount)
  (dotimes (n amount)
    (let* ((num (hexnum (+ start n)))
	   (foo (- (* 2 num)))
	   (a (quadratic-formula 1 1 foo))
	   (b (quadratic-formula 3 -1 foo)))
      (if (and (intp a) (intp b)
	       (and (= (trinum (intp a)) (pentnum (intp b))))) 
	  (print (list (intp a) (intp b) (+ start n) num))))))

(defun zigzag (n)
  (let* ((tier (ceiling (munirt n)))
	 (tmax (trinum tier))
	 (diff (- tmax n))
	 (x (- tier diff 1)))
    (list x diff)))

(defun f44 (start amount)
  (let ((smallest 123456789)
	(answer nil))
    (dotimes (x amount)
      (let* ((baz (zigzag (+ start x)))
	     (f (1+ (first baz)))
	     (j (1+ (second baz)))
	     (pentf (pentnum f))
	     (pentj (pentnum j))
	     (diff (abs (- pentj pentf)))
	     (sum (+ pentj pentf)))
	(if (and (pentnump sum) (pentnump diff))
	    (if (< diff smallest)
		(progn 
		  (setf smallest diff)
		  (setf answer (list f j pentf pentj sum diff)))))))
    answer))

(defun pythagp (a b c)
  (= (+ (* a a) (* b b)) (* c c)))

(defun f9 ()
  (dotimes (a 1000)
    (dotimes (b 1000)
      (if (pythagp a b (- 500 (/ (* a b) 1000)))
	  (print (list a b))))))

;returns an array filled with numbers in their corresponding indices
(defun Z+ (amount)
  (let ((aray (make-array amount)))
    (dotimes (n amount)
      (setf (aref aray n) n))
    aray))

(defun next-array-thing (aray location)
  (if (< location (length aray))
      (if (aref aray location)
	  location
	  (next-array-thing aray (1+ location)))
      "OUT OF BOUNDS"))

(defun comb (aray size)
  (let ((counter size))
    (dotimes (n (- (ceiling (/ (length aray) size)) 2))
      (incf counter size)
      (setf (aref aray counter) nil))
    aray))

(defun sieve (amount &optional (stuff (z+ amount)))
  (setf (aref stuff 0) nil)
  (setf (aref stuff 1) nil)
  (labels ((rec (current-prime)
	     (if (numberp current-prime) 
		 (progn
		   (comb stuff current-prime)
		   (rec (next-array-thing stuff (1+ current-prime)))))))
    (rec 2))
  (remove nil stuff))

(defun p14 (size)
  (let* ((stuff  (make-array size :initial-element nil)))
    (setf (aref stuff 0) -1)
    (setf (aref stuff 1) 0)
    (labels ((rec (location chain)
	       (let* ((num nil))
		 (if (< location (length stuff))
		     (setf num (aref stuff location)))
		 (if (null num)
		     (if (evenp location)  
			 (rec (/ location 2) (cons location chain))
			 (rec (1+ (* 3 location)) (cons location chain)))
		     (let ((counter 0))
		       (dolist (place chain)
			 (setf counter (1+ counter))
			 (if (< place (length stuff))
			     (setf (aref stuff place) (+ counter num)))))))))
      (dotimes (n (length stuff))
	(rec n nil)))
    stuff))

(defun max-in-array (ar)
  (let ((max nil)
	(ans nil))
    (setf max (aref ar 0))
    (dotimes (n (length ar))
      (if (< max (aref ar n))
	  (progn 
	    (setf max (aref ar n))
	    (setf ans n))))
    (cons ans max)))

(defun list-to-vector (dalis)
  (make-array (length dalis) :initial-contents dalis))

(defun factor (n &optional chain (place 0))
  (if (= 1 n)
      (return-from factor chain)
      (if (< place (length primes))
	  (if (zerop (mod n (aref primes place)))
	      (factor (/ n (aref primes place)) (cons (aref primes place) chain) place)
	      (factor n chain (1+ place)))
	  (return-from factor (cons n chain)))))

(defun ffactor (n)
  (if (and (intp n) (< 0 n))
      (tally-occurences (factor n))
      "fuck you"))

(defun how-many (x jar &optional (tally 0))
  (if jar
      (if (= x (car jar))
	  (how-many x (cdr jar) (1+ tally))
	  (how-many x (cdr jar) tally))
      tally))

(defun fmultiply (&rest args)
  (let* ((product nil))
    (dolist (num args)
      (dolist (term num)
	(let* ((pow (cdr term))
	       (base (car term))
	       (pterm (assoc base product))
	       (ptermpow (cdr pterm)))
	  (if pterm
	      (rplacd (assoc base product) (+ ptermpow pow))
	      (setf product (cons term product))))))
    product))

(defun fdivide (num &rest denom)
  (let ((d (apply (function fmultiply) denom))
	(quotient num))
    (dolist (term d)
      (let* ((pow (cdr term))
	     (base (car term))
	     (pterm (assoc base quotient))
	     (ptermpow (cdr pterm)))
	(if pterm 
	    (rplacd (assoc base quotient) (- ptermpow pow))
	    (setf quotient (cons (cons base (- pow)) quotient)))))
    quotient))


(defun tally-occurences (paper &optional chain)
  (if paper
      (let* ((a (first paper))
	     (harmony (how-many a paper)))
	(tally-occurences (remove a paper) (cons (cons a harmony) chain)))
      chain))

(defun number-of-divisors (x)
  "you need to provide the assoc list representation of the factors"
  (let ((product 1))
    (dolist (c x)
      (setf product (* product (1+ (cdr c)))))
    product))

(defun p12 (amount target)
  (let ((primes (sieve amount))
	(an0 (list (cons 2 1)))
	(an1 (list (cons 3 1))))
    (dotimes (nope amount)
      (let* ((n (+ 3 nope))
	     (num (fdivide (fmultiply an0 an1) (list (cons 2 1))))
	     (divs (number-of-divisors num)))	
	(if (< target divs)
	    (return-from p12 (values n num divs)))
	(setf an0 an1)
	(setf an1 (tally-occurences (factor n primes)))))))

(defun p11 ()
  (let ((grid (make-array 400 :initial-contents 
			  (list  
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))
	(biggest 0)
	(ans nil))
    (labels ((getit (x y)
	       (if (and (< -1 x 20) (< -1 y 20))
		   (aref grid (+ x (* 20 y)))
		   0))
	     (relatit (a b)
	       (lambda (coord)
		 (getit (+ a (car coord)) (+ b (second coord)))))
	     (lins (x y)
	       (lambda (coords)
		 (apply (function *) (mapcar (relatit x y) coords))))
	     (biggy (x y)
	       (if (< biggest x)
		   (progn (setf biggest x)
			  (setf ans (cons x y))))))
      (dotimes (x 20)
	(dotimes (y 20)
	  (let ((funk (lins x y)))
	    (biggy (funcall funk '((0 0) (1 1) (2 2) (3 3))) (list x y 1 1))
	    (biggy (funcall funk '((0 0) (0 1) (0 2) (0 3))) (list x y 0 1))
	    (biggy (funcall funk '((0 0) (1 0) (2 0) (3 0)))  (list x y 1 0))
	    (biggy (funcall funk '((0 0) (-1 1) (-2 2) (-3 3))) (list x y -1 1)))))
      ans)))

(defun fuc (n &optional (start 1))
  (let ((product 1))
    (dotimes (x n)
      (setf product (* product (+ start x))))
    product))

(defun permutes (j k)
  (fuc k (- j k -1)))

(defun combones (j k)
  (/ (permutes j k) (fuc k)))

(defun sum-of-digits (n)
  (let ((sum 0)
	(vec (write-to-string n)))
    (dotimes (n (length vec))
      (setf sum (+ sum (digit-char-p (aref vec n)))))
    sum))

(defun p17 (n)
  (let ((sum 0))
    (dotimes (foo n)
      (let* ((wot (1+ foo))
	     (num (remove #\- 
				  (remove #\space 
					  (format nil "~r" wot)))))
	(setf sum (+ sum (length num)))
	(print num)))
    sum))

(defun get-tritem (tri row column)
  (aref tri (+ column (trinum row))))

(defun set-tritem (tri row column newval)
  (setf (aref tri (+ column (trinum row))) newval))

(defun maximum-path-reduce (aray)
  (let* ((rows (round (munirt (length aray)))))
    (dotimes (r rows)
      (let* ((darow (- rows r 1))
	     (upro (1- darow)))
	(dotimes (c (1+ upro))
	  (let ((here (get-tritem aray upro c))
		(below (get-tritem aray darow c))
		(bottomright (get-tritem aray darow (1+ c))))
	    (if (< below bottomright)
		(set-tritem aray upro c (+ here bottomright))
		(set-tritem aray upro c (+ here below)))))))))

(defun lis2array (lis)
  (make-array (length lis) :initial-contents lis))

(defun p18 ()
  (let ((tri (lis2array (list
			 75
			 95 64
			 17 47 82
			 18 35 87 10
			 20 04 82 47 65
			 19 01 23 75 03 34
			 88 02 77 73 07 63 67
			 99 65 04 28 06 16 70 92
			 41 41 26 56 83 40 80 70 33
			 41 48 72 33 47 32 37 16 94 29
			 53 71 44 65 25 43 91 52 97 51 14
			 70 11 33 28 77 73 17 78 39 68 17 57
			 91 71 52 38 17 14 91 43 58 50 27 29 48
			 63 66 04 68 89 53 67 30 73 16 69 87 40 31
			 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23))))
    (maximum-path-reduce tri)
    tri))

(defparameter primes nil)

(defun naturalp (n)
  (if (< 0 n)
      (intp n)
      "fuck you"))

(defun make-primes (n)
  (setf primes (sieve n))
  (print "yay"))

(defun dorange (bottom top funk)
  (let ((bottom (ceiling bottom))
	(top (floor top)))
    (dotimes (n (- top bottom -1))
      (funcall funk (+ bottom n)))))

(defun find-right-tris (perimeter)
  (let* ((minp (* perimeter (- (sqrt 2) 1)))
	 (maxp (/ perimeter 2))
	 (stuffs nil)
	 (funk (lambda (n)
		 (let ((side-budget (- perimeter n)))
		   (dotimes (wowz (ceiling (/ (1- side-budget) 2)))
		     (let* ((a (1+ wowz))
			    (b (- side-budget a)))
		       (if (pythagp a b n)
			   (push (list a b n) stuffs))))))))
    (dorange minp maxp funk)
    (if stuffs
	(cons perimeter stuffs))))

(defun primep (n)
  (= 1 (length (factor n))))

(defun p39 (num)
  (dotimes (n num)
    (let ((a (1+ n)))
      (print (find-right-tris a)))))

(defun rt (num)
  (truncate (/ num 10)))

(defun lt (num)
  (mod num (expt 10 (floor (log num 10)))))

(defun la (num a)
  (+ num (* a (expt 10 (1+ (floor (log num 10)))))))

(defun ra (num a)
  (+ a (* 10 num)))

(defun p37 ()
  (let ((seeds (list 2 3 5 7))
	(avals nil)
	(bvals nil))
    (labels ((reca (num depth stop)
	       (if (= depth stop)
		   nil
		   (if (= 1 (length (factor num)))
		       (progn
			 (push num avals)
			 (dotimes (a 9)
			   (reca (La num (1+ a)) (1+ depth) stop))))))
	     (recb (num depth stop)
		 (if (= depth stop)
		     nil
		     (if (= 1 (length (factor num)))
			 (progn
			   (push num bvals)
			   (dotimes (a 10)
			     (recb (Ra num a) (1+ depth) stop)))))))
      (dolist (seed seeds)
	(recb seed 0 9)
	(reca seed 0 9)))
    (print avals)
    (print bvals)
    (intersection avals bvals)))

(defun fara (aray place)
  (if (< -1 place (length aray))
      (aref aray place)
      0))

(defun steps (size coins &optional (aray (make-array (1+ size))))  
  (dolist (coin coins)
    (if (< -1 coin (length aray))
	(setf (aref aray coin) 1)))
  (print aray)
  (let ((len (length aray)))
    (dotimes (x len)
      (let ((total 0))
	(dolist (coin coins)
	  (incf total (fara aray (- x coin))))
	(incf (aref aray x) total))))
  aray)

(defun clear-array (aray)
  (dotimes (x (length aray))
    (setf (aref aray x) 0)))

(defun add-to-array (aray delta)
  (dotimes (x (length delta))
    (incf (aref aray x) (aref delta x))))

(defun naive-coins (size monies)
  (let* ((coins (small-sort monies))
	 (reg-size (1+ size))
	 (aray (make-array reg-size))
	 (delta (make-array reg-size)))
    (setf (aref aray 0) 1)
    (dolist (coin coins)
      (clear-array delta)
      (dotimes (n reg-size)
	(let ((f (aref aray n))
	      (mores (floor (/ (- size n) coin))))
	  (dotimes (k mores)
	    (incf (aref delta (+ n (* (1+ k) coin))) f))))
      (add-to-array aray delta))
    aray))

(defun coins2 (size monies &optional debug)
  (let* ((coins (small-sort monies))
	 (reg-size (1+ size))
	 (aray (make-array reg-size)))
    (setf (aref aray 0) 1)
    (dolist (coin coins)
      (if debug
	  (print aray))
      (dotimes (n coin)
	;n is the start
	(let* ((mores (floor (/ (- size n) coin)))
	       (nums nil)
	       (diffs nil))
	  (if (< 0 mores) 
	      (progn
		(setf nums (make-array mores))
		(dotimes (k mores)
		  (setf (aref nums k) (aref aray (+ n (* k coin)))))
		(setf diffs (array-sum nums))
		(if debug
		    (print (list "diffs" diffs "nums" nums)))
		(dotimes (k mores)
		  (let ((place (+ n (* (1+ k) coin))))
		    (incf (aref aray place) (aref diffs (1+ k))))))))))
    aray))

(defun p20 (n)
  (sum-of-digits (fuc n)))

(defun spiral-corners (n)
  (let ((vals (list 1))
	(increments 0)
	(current 1))
    (dotimes (j n)
      (incf increments 2)
      (dotimes (k 4)
	(incf current increments)
	(push current vals)))
    (nreverse vals)))

(defun square (x)
  (* x x))

(defun cube (x)
  (* x x x))

(defun digit-splitter (n)
 (mapcar 'digit-char-p (coerce (write-to-string n) 'list)))


(defun p28 ()
  (apply (function +) (spiral-corners 500)))

(defun p30 (num)
  (let ((vals nil))
    (dotimes (n num)
      (if (= n (APPly (function +) (mapcar (lambda (x) (expt x 5)) (digit-splitter n))))
	  (push n vals)))
    vals))

(defun palindromep (s)
  (let ((len (length s)))
    (dotimes (x (floor (/ len 2)))
      (if (char= (aref s x) (aref s (- len x 1)))
	  69
	  (return-from palindromep nil)))
    t))

(defun binray (num)
  (format nil "~b" num))

(defun p34 (num)
  (let ((vals nil))
    (dotimes (n num)
      (if (= n (APPly (function +) (mapcar (lambda (x) (fuc x)) (digit-splitter n))))
	  (push n vals)))
    vals))

(defun p36 (num)
  (let ((vals nil))
    (dotimes (n num)
      (if (and (palindromep (write-to-string n)) (palindromep (binray n)))
	  (push n vals)))
    vals))

(defun array-sum (grid &optional (n (length grid)) (ans (make-array (1+ n))))
  (dotimes (x n)
    (setf (aref ans (1+ x)) (+ (aref ans x) (aref grid x))))
  ans)

(defparameter foo50 nil)

(defun p50 (len target)
  (let ((ans nil)
	(biggest 0))
    (dotimes (n len)
      (block fuckall
	(dotimes (p (- len n 1))
	  (let ((sum (- (aref foo50 (+ n p 1)) (aref foo50 n))))
	    (if (< target sum)
		(return-from fuckall))
	    (if (primep sum)
		(if (< biggest (+ 1 p))
		    (progn 
		      (setf biggest (+ 1 P))
		      (setf ans (list sum (1+ n) biggest)))))))))
    ans))

(defun period (n len)
  (dotimes (x len)
    (let ((nines (1- (expt 10 (1+ x)))))
      (if (zerop (mod nines n))
	  (return-from period (1+ x)))))
  0)

(defun decompose (num)
  (tally-occurences (factor num)))

(defun distinct-prime-factors (num)
  (length (decompose num)))

(defun p47 (amount)
  (dotimes (n amount)
    (if (= 4 (distinct-prime-factors (+ n 2)))
	(if (= 4 (distinct-prime-factors (+ n 3)))
	    (if (= 4 (distinct-prime-factors (+ n 4)))
		(if (= 4 (distinct-prime-factors (+ n 5)))
		    (progn
		      (print "YAYAH")
		      (print (+ n 2)))))))))

(defun sum-of-squares-of-digits (n)
  (apply (function +) (mapcar (lambda (x) (* x x)) (digit-splitter n))))



(defun square-digits-chain (num)
  (let* ((aray (make-array num))
	 (setra89 (lambda (x) (setf (aref aray x) 89))))
    (mapcar setra89 (list 145 42 20 4 16 37 58 89))
    (SETF (aref aray 1) 1)
    (setf (aref aray 0) "fucccck")
    (labels ((rec ()
	       (if (find 0 aray)
		   (progn
		     (dotimes (n num)
		       (let ((next (sum-of-squares-of-digits n)))
			 (setf (aref aray n) (aref aray next))))
		     (rec)))))
      (rec)) 
    aray))

(defun p92 (amount)
  (let ((lookup (square-digits-chain 600))
	(tot 0))
    (dotimes (n (1- amount))
      (let* ((k (1+ n)))
	(if (= 89 (aref lookup (sum-of-squares-of-digits k)))
	    (incf tot))))
    tot))

(defun z+list (n)
  (let ((chain nil))
    (dotimes (k n)
      (push (1+ k) chain))
    (nreverse chain)))

(defun product-of-parts (n k)
  (expt (/ n k) k))

(defun max-product-of-parts (n)
  (Let* ((k (/ n (exp 1)))
	 (top (ceiling k))
	 (bottom (floor k))
	 (tval (product-of-parts n top))
	 (bval (product-of-parts n bottom)))
    (if (< bval tval)
	(values (/ n top) top tval)
	(values (/ n bottom) bottom bval))))

(defun remove-factors (n blacklist)
  (if blacklist
      (Let* ((facto (first blacklist)))
	(if (zerop (mod n facto))
	    (remove-factors (/ n facto) blacklist)
	    (remove-factors n (cdr blacklist))))
      n))

(defun p183 (amount)
  (let ((total 0))
    (dotimes (x (- amount 4))
      (let* ((n (+ x 5))
	     (scrub (remove-factors (denominator (max-product-of-parts n)) '(2 5))))
	(if (= 1 scrub)	    
	    (decf total n)
	    (incf total n))))
    total))

(defun increment-clock (the-clock the-units)
  (if the-clock
      (let ((maximum (car the-units))
	    (val (car the-clock)))
	(if (= (1+ val) maximum)
	    (progn
	      (rplaca the-clock 0)
	      (increment-clock (cdr the-clock) (cdr the-units)))
	    (incf (car the-clock))))))

(defun clounter (units)
  (let ((the-times nil))
    (dotimes (n (length units))
      (push 0 the-times))
    (lambda () 
      (increment-clock the-times units)
      the-times)))

(defun take (apples farm &optional chain)
  (if apples
      (progn
	(let ((pick (nth (car apples) farm)))
	  (take (cdr apples) (remove pick farm) (cons pick chain))))
      chain))

(defun p41 (x)
  (let* ((nice (z+list x))
	 (counter (clounter (nreverse (z+list x))))
	 (nums nil))
    (dotimes (n (fuc x))
      (let ((num (list-num (take (funcall counter) nice))))
	(if (primep num)
	    (push num nums))))
    (apply (function max) nums)))

(defun list-num (lissy &optional (tot 0))
  (if lissy
      (list-num (cdr lissy) (+ (car lissy) (* 10 tot)))
      tot))

(defun ssds (x)
  (small-sort (digit-splitter x)))

(defun lentil (num)
  (1+ (floor (log num 10))))

(defun concat-nums (args &optional (chain 0))
  (if args
      (concat-nums (cdr args) (+ (* chain (expt 10 (lentil (car args)))) (car args)))
      chain))

(defun pandigitalp (num)
  (equal (small-sort (digit-splitter num)) (z+list (lentil num))))

(defun concatenated-product (num n)
  (concat-nums (mapcar (lambda (x) (* num x)) (z+list n))))

(defun p49 ()
  (let ((primes (remove-if (lambda (x) (< x 1000)) (sieve 10000))))
    (dotimes (n (length primes))
      (dotimes (k (- (length primes) n 1))
	(let* ((a (aref primes n))
	       (b (aref primes (+ n 1 k)))
	       (diff (- b a))
	       (c (+ b diff)))
	  (if (find c primes)
	      (if (and (equal (ssds a) (ssds b))
		       (equal (ssds a) (ssds c)))
		  (print (list a b c)))))))))

(defun p38 (n amount)
  (dotimes (num amount)
    (let ((product (concatenated-product (1+ num) n)))
      (if (pandigitalp product)
	  (print (list (1+ num) n product))))))

(defun jump-partitions (n)
  (let ((total 0))
    (dotimes (k n)
      (incf total (combones (+ n k) k)))
    total))



(defun p-for-partition (n)
  (let ((land (make-array (1+ n))))
    (setf (aref land 0) 1)
    (dotimes (wow n)
      (let* ((index (1+ wow))
	     (tot 0))
	(dotimes (wot (flubs (nubs index)))
	  (if (< (mod wot 4) 2)
	      (incf tot (aref land (- index  (pentnum  (sbulf (+ 1 wot))))))
	      (decf tot (aref land (- index  (pentnum  (sbulf (+ 1 wot))))))))
	(setf (aref land index) tot)))
    land))

(defun NUBS (n)
  (multiple-value-bind (a b) (muntnep n)
    (let ((ax (floor a))
	  (bx (ceiling b)))
      (if (< (abs bx) ax)
	  ax
	  bx))))

(defun flubs (n)
  (if (< 0 n)
      (+ n n -1)
      (* n -2)))

(defun sbulf (n)
  (if (evenp n)
      (/ n -2)
      (/ (1+ n) 2)))

(defun buns (n)
  (let ((wo (nubs n)))
    (print (pentnum wo))))

(defun p78 (n cap)
  (let ((land (make-array (1+ n))))
    (setf (aref land 0) 1)
    (dotimes (wow n)
      (let* ((index (1+ wow))
	     (tot 0))
	(dotimes (wot (flubs (nubs index)))
	  (if (< (mod wot 4) 2)
	      (incf tot (aref land (- index  (pentnum  (sbulf (+ 1 wot))))))
	      (decf tot (aref land (- index  (pentnum  (sbulf (+ 1 wot))))))))
	(if (zerop (setf (aref land index) (mod tot cap)))
	    (progn
	      (print index)
	      (print "FUCK YES FINAL FUCKINGLY")
	      (return-from p78)))))
    land))


(defun load-libs ()
  (load "~/desktop/res/split-sequence.lisp"))

(defun split-string (&rest args)
  (apply (function split-sequence:split-sequence) args))

(defun string-to-vals (yay)
  (mapcar (lambda (x) (mapcar (function parse-integer) x)) 
			(mapcar (lambda (x) (split-sequence:split-sequence #\, x)) 
				(split-string #\newline yay))))

(defun p99 (string)
  (let ((bigg 0)
	(lizzy (string-to-vals string))
	(ans nil))
    (labels ((rec (todo count)
	       (if todo
		   (Let* ((num (car todo))
			  (n (* (log (car num)) (second num))))
		     (if (< bigg n)
			 (progn
			   (setf ans count)
			   (setf bigg n)))
		     (rec (cdr todo) (1+ count))))))
      (rec lizzy 0)
      ans)))

(defun foob (x)
  (values (floor x 51) (mod x 51)))

(defun p91 ()
  (declare (optimize speed))
  (let ((total 0))
    (dotimes (n1 (1- (* 51 51)))
      (dotimes (n2 n1)
	(let* ((x1 (floor (1+ n1) 51))
	       (y1 (mod (1+ n1) 51))
	       (x2 (floor (1+ n2) 51))
	       (y2 (mod (1+ n2) 51)))
	  (if (or (zerop (+ (* x1 x2) (* y1 y2)))
		  (zerop (+ (* x1 (- x1 x2)) (* y1 (- y1 y2))))
		  (zerop (+ (* x2 (- x2 x1)) (* y2 (- y2 y1)))))
	      (incf total)))))
    total))

(defun p94a (amount)
  (declare (optimize speed))
  (let ((total 0)
	(nums (list "k*(4+3k)")))
    (dotimes (a amount)
      (Let* ((k (+ 2 a))
	     (n (* k (+ 4 k k k))))
	(if (= (expt (isqrt n) 2) n)
	    (progn
	      (incf total)
	      (print (cons k (+ 4 k k k)))
	      (push (cons k (+ 4 k k k)) nums)))))
    (values nums total)))

(defun p94b (amount)
  (declare (optimize speed))
  (let ((total 0)
	(nums (list "k*(3k-4)")))
    (dotimes (a amount)
      (Let* ((k (+ 2 a))
	     (n (* k (+ -4 k k k))))
	(if (= (expt (isqrt n) 2) n)
	    (progn
	      (incf total)
	      (print (cons k (+ -4 k k k)))
	      (push (cons k (+ -4 k k k)) nums)))))
    (values nums total)))

(defun P94auxaux ()
  (p94aux '(33895684 2433600 174724 12544 900 64 4)
	  '(126500418 9082322 652082 46818 3362 242 18 2)))

(defun p94aux (plus minus)
  (let* ((a (remove-if-not 'evenp plus))
	 (b (remove-if-not 'evenp minus))
	 (a2 (mapcar (lambda (x)
		       (- (+ x x x) 4)) a))
	 (b2 (mapcar (lambda (x)
		       (+ (+ x x x) 4)) b))
	 (c (append a2 b2)))
    (print c)
    (apply (function +) c)))

(defun p85 (a b)
  (* a (1+ a) b (1+ b) 1/4))

(defun p85aux ()
  (let ((nums nil))
    (dotimes (n (isqrt 8000000))
      (push (cons (* n (1+ n)) n)  nums))
    (let ((smallest 10000000000)
	  (ans nil))
      (dolist (num nums)
	(dolist (num2 nums)
	  (let ((product (* (car num) (car num2))))
	    (if (< (abs (- 8000000 product)) smallest)
		(progn
		  (setf smallest (abs (- 8000000 product)))
		  (setf ans (cons num num2)))))))
      (values smallest ans))))
