(defun proudct( a b)
	(cond
		((null a)nil)
	(t (list(list(car a)(car b))(product (car a) (cdr b))
			(product(cadr a)(car b)))))
)
		