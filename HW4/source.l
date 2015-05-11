(defun all-length(y)
	(cond
			((null y)0)
			((listp (car y)) (+ (all-length(car y))(all-length(cdr y))))
			(t (+ 1 (all-length(cdr y))))
	)
)

(defun range(l)
	(list (minl l) (maxl l))
)

(defun minl(l)
	(cond 
		((null (cdr l))(car l))
		(t (min(car l)(minl(cdr l))))
	)
)

(defun maxl(l)
	(cond
		((null (cdr l))(car l))
		(t (max(car l)(maxl(cdr l))))
	)
)

(defun before (a b l)
	(cond
		((null l) nil)
		((equal a (car l)) (append (list(car l)) (before_help b (cdr l))) )
		(t (before a b (cdr l)))
	)
)

(defun before_help (b l)
	(cond
		((null l) nil)
		((not(equal (car l) b)) (append (list (car l)) (before_help b (cdr l)) ))
		(t (list (car l)))
	)
)

(defun split-if (f l)
	(list (false f l) (true f l)  )
)

(defun false (f l)
	(cond
		((null l) nil)
		((funcall f (car l)) (false f (cdr l)) )
		(t (cons (car l) (false f (cdr l)))  )
	)
)

(defun true (f l)
	(cond
		((null l) nil)
		((funcall f (car l)) (cons (car l) (true f (cdr l))) )
		(t(true f (cdr l)))
	)
)

(defun group(l n)
	(group_help l '() n)
)

(defun group_help(l i n)
	(cond 
		((and(null l)(null i) )i)
		((eql (list-length i) n)(cons i (group_help l '() n)))
		((and (> (list-length i) 0)(null l))(cons i (group_help l '() n)))
		(t(group_help (cdr l) (append i (list (car l))) n))
	)
)

(defun mostn(f l)
	(append (list(mostn_help f l '() 0))(list(funcall f (car(mostn_help f l '() 0)))))
)

(defun mostn_help(f l i n)
	(cond
		((null l) i)
		((> (funcall f (car l)) n)(mostn_help f (cdr l) (list(car l)) (funcall f (car l))))
		((equal (funcall f (car l)) n)(mostn_help f (cdr l) (append i (list (car l))) n))
		(t (mostn_help f (cdr l) i n))
  )
)


(defun match(k l)
	(cond
		((or (null k)(null l)) (eq k l))
		((eq (car k) '?)(match (cdr k) (cdr l)))
		((and (eq (car k) '!) (match (cdr k) (cdr l))))
		((eq (car k) '!) (match k (cdr l)))
		(t (and(eq (car k) (car l)) (match (cdr k) (cdr l))))
	)
)

(defun match-var(k l)
	(unbound-symbols k)(match-var-help k l)
)

(defun match-var-help (k l)
	(cond 
		((or (null  k)(null  l)) (eq k l)) 
		((and (listp (car k)) (eq (caar k) '?))(set (cadar k) (car l) )(match-var-help (cdr k) (cdr l)))
	
		((and (listp (car k)) (eq (caar k) '!)(not(boundp (cadar k)))) (set (cadar k) nil) (match-var-help k l))
		((and (listp (car k)) (eq (caar k) '!)(listp (eval(cadar k)))(set (cadar k)(append (eval(cadar k)) (list(car l))))(match-var-help (cdr k) (cdr l))))

 		((and (listp (car k)) (eq (caar k) '!)(listp (eval(cadar k))))(match-var-help k (cdr l)))
		((and (listp (car k)) (eq (caar k) '?)) (match-var-help (cdr k) (cdr l)))
		((and (listp (car k)) (eq (caar k) '!)(match-var (cdr k) (cdr l))))
		((and (listp (car k)) (eq (caar k) '!))(match-var k (cdr l)))
		(t (and(eq (car k) (car l)) (match-var-help (cdr k) (cdr l))))
	)
)

(defun unbound-symbols (l)
	(cond 
		((null l) nil)
		((and(listp (car l)) (eq (caar l) '?))(makunbound (cadar l))(unbound-symbols (cdr l)))
		((and(listp (car l)) (eq (caar l) '!))(makunbound (cadar l))(unbound-symbols (cdr l)))
		((null (car l))(unbound-symbols (cdr l)))
		(t (makunbound (car l))(unbound-symbols (cdr l)))
	)
)




  