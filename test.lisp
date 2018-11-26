;same tests as Sumner Evan's group
(defmacro test-case (stx result)
  `(let ((actual ,stx))
     (if (equal actual ,result)
       (format t "PASS")
       (format t "FAIL"))
     (format t " ~a == ~a, got ~a~%" (quote ,stx) ,result actual)))

(defun test-set-eq (a b) (test-case (set-difference a b) NIL))

(defun run-tests ()
    ;test exp->nnf
    (test-case (exp->nnf '(not (not a))) 'a)
    (test-case (exp->nnf '(not (and a b))) '(OR (NOT b) (NOT a)))
    (test-case (exp->nnf '(not (or a (not b)))) '(AND B (NOT A)))
    (test-case (exp->nnf '(not (:iff a b))) '(OR (AND A (NOT B)) (AND B (NOT A))))
    (test-case (exp->nnf '(:iff a b)) '(and (or (not a) b) (or (not b) a)))
    (test-case (exp->nnf '(:implies A b)) '(oR (NOT a) B))
    ;test exp->cnf
    (test-case (exp->cnf '(:iff A B)) '(and  (or (NOT B) A)(or (NOT A) B))) 
    (test-case (exp->cnf '(:implies A B)) '(and (or (not A) B)))
    (test-case (exp->cnf '(not (not a))) '(and (or A)))
    (test-case (exp->cnf '(not (and a b))) '(AND (OR (NOT B) (NOT A))))
    (test-case (exp->cnf '(or a (and b g))) '(and (or A B) (or A G)))
    (test-case (exp->nnf '(:implies (not (or a b)) c)) '(or (or b a) c))

    ;test dpll
    (test-case (dpll (cnf-maxterms '(and (or a)))) t)
    (test-case (dpll (cnf-maxterms '(and (or a) (or (not a))))) nil)
    (test-case (dpll (cnf-maxterms '(and (or a b) (or (not a) b)))) t)
    (test-case (dpll (cnf-maxterms (exp->cnf '(and a (not a))))) nil)
    (test-case (dpll (cnf-maxterms (exp->cnf '(or a (not a))))) t)
    (test-case (dpll (cnf-maxterms '(and (or a (not a))))) t)
    (test-case (dpll (cnf-maxterms '(and (or a b) (or (not a) (not b))))) t)
    (test-case (dpll (cnf-maxterms '(and (or a (not a)) (or b (not b) c)))) t) 
    (test-case (dpll (cnf-maxterms '(and (or a a a b b b c c c (not a) (not a) (not b) d)))) t)
    (test-case (dpll (cnf-maxterms '(and (or a b c d (not a) (not a) a)))) t)
    (test-case (dpll (cnf-maxterms '(and (or a) (or (not a))))) nil)
    (test-case (dpll (cnf-maxterms '(and (or a) (or b) (or (not b)) (or c d e f g)))) nil)
    ;test sat-p




  )
