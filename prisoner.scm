;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))


;;problem1
(define extract-entry (lambda (play game-list) 
	(if (equal? play (car (car game-list))) (car game-list) (extract-entry play (cdr game-list)))))

;;problem2
;;different scores based on different play strategies

;;problem3
;;algorithm keeps history of previous plays
;;yes it has local variables that are incrmented at each play instead of checking the entire history before each play

;;problem4
(define (EYE-FOR-TWO-EYES my-history other-history)
	(if (or (empty-history? my-history) (empty-history? (rest-of-plays my-history)))
      		"c"
		(if (and
			(equal? "d" (most-recent-play other-history))
			(equal? "d" (most-recent-play (rest-of-plays other-history)))) "d" "c")))




(play-loop EYE-FOR-TWO-EYES NASTY)
;;player1	0.9791666
;;player2	1.0833

(play-loop EYE-FOR-TWO-EYES PATSY)
;;player1	3
;;player2	3

(play-loop EYE-FOR-TWO-EYES SPASTIC)
;;player1	1.8686
;;player2	3.2323


(play-loop EYE-FOR-TWO-EYES EGALITARIAN)
;;player1	3
;;player2	3

(play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE)
;;player1	3
;;player2	3


;;problem5
(define EYE-FOR-N-EYES (lambda (n) (lambda (my-history other-history)
	(cond
		((or (empty-history? my-history) (<= (length my-history) n)) "c")
		((and (equal? "d" (most-recent-play other-history)) (equal? n 1)) "d")
		((and (equal? "d" (most-recent-play other-history)) (> n 1))
			((EYE-FOR-N-EYES (- n 1)) (rest-of-plays my-history) (rest-of-plays other-history)))
		(else "c")))))

(play-loop (EYE-FOR-N-EYES 3) NASTY)
;;player1 .96
;;player2 1.15

(play-loop (EYE-FOR-N-EYES 3) PATSY)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 3) SPASTIC)
;;player1 1.66
;;player2 3.48

(play-loop (EYE-FOR-N-EYES 3) EGALITARIAN)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 3) EYE-FOR-EYE)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 3) EYE-FOR-TWO-EYES)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 4) NASTY)
;;player1 .94
;;player2 1.2

(play-loop (EYE-FOR-N-EYES 4) PATSY)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 4) SPASTIC)
;;player1 1.48
;;player2 3.86

(play-loop (EYE-FOR-N-EYES 4) EGALITARIAN)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 4) EYE-FOR-EYE)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 4) EYE-FOR-TWO-EYES)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 5) NASTY)
;;player1 .93
;;player2 1.26

(play-loop (EYE-FOR-N-EYES 5) PATSY)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 5) SPASTIC)
;;player1 1.67
;;player2 3.88

(play-loop (EYE-FOR-N-EYES 5) EGALITARIAN)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 5) EYE-FOR-EYE)
;;player1 3
;;player2 3

(play-loop (EYE-FOR-N-EYES 5) EYE-FOR-TWO-EYES)
;;player1 3
;;player2 3



;;problem6, not done yet
(define make-rotating-strategy (lambda (strat0 strat1 freq0 freq1) (lambda (my-history other-history)
	(if (< (remainder (number-of-plays other-history) (+ freq0 freq1))) freq0) (strat0 my-history other-history) (strat1 my-history other-history))))


(define number-of-plays (lambda (history)
	(if (null? history) 0 (length history))))

(play-loop (make-rotating-strategy PATSY NASTY 3 5) SPASTIC)
;;player1 2.82
;;player2 .54

(play-loop (make-rotating-strategy PATSY NASTY 2 1) SPASTIC)
;;player1 3.15
;;player2 .46

;;problem7
(define make-higher-order-spastic (lambda (lst) (lambda (my-history other-history)
	((list-ref lst (remainder (number-of-plays other-history) (length lst))) my-history other-history))))

(define plays (list NASTY PATSY SPASTIC EGALITARIAN EYE-FOR-EYE))

(play-loop (make-higher-order-spastic plays) NASTY)
;;player1 .71
;;player2 2.13
(play-loop (make-higher-order-spastic plays) PATSY)
;;player1 3.61
;;player2 2.07
(play-loop (make-higher-order-spastic plays) SPASTIC)
;;player1 1.91
;;player2 2.17
(play-loop (make-higher-order-spastic plays) EGALITARIAN)
;;player1 .97
;;player2 2.04
(play-loop (make-higher-order-spastic plays) EYE-FOR-EYE)
;;player1 2.47
;;player2 2.42

;;problem8
(define gentle (lambda (strat gentleness-factor) (lambda (my-history other-history)
	(if (equal? "d" (strat my-history other-history))
		(if (< (random 1.0) gentleness-factor) "d" "c") "c"))))

(define (slightly-gentle-NASTY my-history other-history)
	((gentle nasty 0.1) my-history other-history))

(define (slightly-gentle-EYE-FOR-EYE my-history other-history)
	((gentle EYE-FOR-EYE 0.1) my-history other-history))


(play-loop (gentle nasty 0.5) NASTY)
;;player1 .463
;;player2 3.14
(play-loop (gentle patsy 0.5) NASTY)
;;player1 0
;;player2 5
(play-loop (gentle spastic 0.5) NASTY)
;;player1 .22
;;player2 4.09
(play-loop (gentle EGALITARIAN 0.5) NASTY)
;;player1 .42
;;player2 3.29
(play-loop (gentle EYE-FOR-EYE 0.5) NASTY)
;;player1 .47
;;player2 3.08

(play-loop (gentle nasty 0.5) PATSY)
;;player1 3.95
;;player2 1.57
(play-loop (gentle patsy 0.5) PATSY)
;;player1 3
;;player2 3
(play-loop (gentle spastic 0.5) PATSY)
;;player1 3.52
;;player2 2.22
(play-loop (gentle EGALITARIAN 0.5) PATSY)
;;player1 3
;;player2 3
(play-loop (gentle EYE-FOR-EYE 0.5) PATSY)
;;player1 3
;;player2 3

(play-loop (gentle nasty 0.5) spastic)
;;player1 2.14
;;player2 2.33
(play-loop (gentle patsy 0.5) spastic)
;;player1 1.32
;;player2 4.11
(play-loop (gentle spastic 0.5) spastic)
;;player1 1.99
;;player2 2.8
(play-loop (gentle EGALITARIAN 0.5) spastic)
;;player1 1.66
;;player2 3.55
(play-loop (gentle EYE-FOR-EYE 0.5) spastic)
;;player1 1.84
;;player2 2.84

(play-loop (gentle nasty 0.5) EGALITARIAN)
;;player1 3.69
;;player2 1.87
(play-loop (gentle patsy 0.5) EGALITARIAN)
;;player1 3
;;player2 3
(play-loop (gentle spastic 0.5) EGALITARIAN)
;;player1 3.56
;;player2 2.14
(play-loop (gentle EGALITARIAN 0.5) EGALITARIAN)
;;player1 3
;;player2 3
(play-loop (gentle EYE-FOR-EYE 0.5) EGALITARIAN)
;;player1 3
;;player2 3

(play-loop (gentle nasty 0.5) EYE-FOR-EYE)
;;player1 2.31
;;player2 2.31
(play-loop (gentle patsy 0.5) EYE-FOR-EYE)
;;player1 3
;;player2 3
(play-loop (gentle spastic 0.5) EYE-FOR-EYE)
;;player1 2.74
;;player2 2.74
(play-loop (gentle EGALITARIAN 0.5) EYE-FOR-EYE)
;;player1 3
;;player2 3
(play-loop (gentle EYE-FOR-EYE 0.5) EYE-FOR-EYE)
;;player1 3
;;player2 3

;;problem9, not done yet
(define (play-loop-3players strat0 strat1 strat2) ;;modified
	(define (play-loop-iter3 strat0 strat1 strat2 count history0 history1 history2 limit) ;;modified
    (cond ((= count limit) (print-out-results-3players history0 history1 history2 limit)) ;;modified
	  (else (let ((result0 (strat0 history0 history1 history2)) ;;modified
		      (result1 (strat1 history1 history0 history2)) ;;modified
		      (result2 (strat2 history2 history0 history1))) ;;modified
		  (play-loop-iter3 strat0 strat1 strat2 (+ count 1) ;;modified
				  (extend-history result0 history0) ;;no need to change
				  (extend-history result1 history1) ;;no need to change
				  (extend-history result2 history2) ;;added this line
				  limit))))) ;;no need to change
  (play-loop-iter3 strat0 strat1 strat2 0 the-empty-history the-empty-history the-empty-history ;;modified
		  (+ 90 (random 21))))

(define (print-out-results-3players history0 history1 history2 number-of-games) ;;modified
(let ((scores (get-scores-3player history0 history1 history2))) ;;modified
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ") ;;added line
    (display (* 1.0 (/ (car (cddr scores)) number-of-games))) ;;added line & modified
    (newline)))

(define (get-scores-3player history0 history1 history2) ;;modified
	(define (get-scores-helper history0 history1 history2 score0 score1 score2) ;;modified
    	(cond ((empty-history? history0) ;;unchanged
	   	(list score0 score1 score2)) ;;modified
	  	(else (let ((game (make-play (most-recent-play history0) ;;unchanged
				       (most-recent-play history1) ;;unchanged
					(most-recent-play history2)))) ;;added line
		  (get-scores-helper (rest-of-plays history0) ;;unchanged
				     (rest-of-plays history1) ;;unchanged
				     (rest-of-plays history2) ;;added line
				     (+ (get-player-points 0 game) score0) ;;unchanged
				     (+ (get-player-points 1 game) score1) ;;unchanged
				     (+ (get-player-points 1 game) score2)))))) ;;added line
  (get-scores-helper history0 history1 history2 0 0 0))

;;problem10, not done yet
(define (NASTY-3 my-history other-history0 other-history1)
	"d")

(define (PATSY-3 my-history other-history0 other-history1)
	"c")

(define (SPASTIC-3 my-history other-history0 other-history1)
  (if (= (random 2) 0)
      "c"
      "d"))

(play-loop-3players nasty-3 nasty-3 nasty-3)
;;player1 1
;;player2 1
;;player3 1

(play-loop-3players patsy-3 patsy-3 patsy-3)
;;player1 4
;;player2 4
;;player3 4

(play-loop-3players SPASTIC-3 SPASTIC-3 SPASTIC-3)
;;player1 2.44
;;player2 2.69
;;player3 2.69


(define (tough-EYE-FOR-EYE my-history other-history0 other-history1)
	(if (empty-history? my-history) "c"
		(if (or
			(equal? "d" (most-recent-play other-history0))
			(equal? "d" (most-recent-play other-history1)))
			"d" "c")))


(define (soft-EYE-FOR-EYE my-history other-history0 other-history1)
	(if (empty-history? my-history) "c"
		(if (and
			(equal? "d" (most-recent-play other-history0))
			(equal? "d" (most-recent-play other-history1)))
			"d" "c")))

(play-loop-3players tough-EYE-FOR-EYE nasty-3 nasty-3)
;;player1 .99
;;player2 1.01
;;player3 1.01

(play-loop-3players tough-EYE-FOR-EYE nasty-3 patsy-3)
;;player1 2.98
;;player2 3.02
;;player3 3.02

(play-loop-3players tough-EYE-FOR-EYE nasty-3 spastic-3)
;;player1 2.04
;;player2 2.07
;;player3 2.07


(play-loop-3players tough-EYE-FOR-EYE patsy-3 nasty-3)
;;player1 2.98
;;player2 .02
;;player3 .02

(play-loop-3players tough-EYE-FOR-EYE patsy-3 patsy-3)
;;player1 4
;;player2 4
;;player3 4

(play-loop-3players tough-EYE-FOR-EYE nasty-3 spastic-3)
;;player1 2.03
;;player2 2.06
;;player3 2.06


(play-loop-3players tough-EYE-FOR-EYE spastic-3 nasty-3)
;;player1 1.97
;;player2 .53
;;player3 .53

(play-loop-3players tough-EYE-FOR-EYE spastic-3 patsy-3)
;;player1 3.57
;;player2 3.59
;;player3 3.59

(play-loop-3players tough-EYE-FOR-EYE spastic-3 spastic-3)
;;player1 2.93
;;player2 2.16
;;player3 2.16

(play-loop-3players soft-EYE-FOR-EYE nasty-3 nasty-3)
;;player1 .99
;;player2 1.01
;;player3 1.01

(play-loop-3players soft-EYE-FOR-EYE nasty-3 patsy-3)
;;player1 2
;;player2 5
;;player3 5

(play-loop-3players soft-EYE-FOR-EYE nasty-3 spastic-3)
;;player1 1.51
;;player2 3.06
;;player3 3.06


(play-loop-3players soft-EYE-FOR-EYE patsy-3 nasty-3)
;;player1 2
;;player2 2
;;player3 2

(play-loop-3players soft-EYE-FOR-EYE patsy-3 patsy-3)
;;player1 4
;;player2 4
;;player3 4

(play-loop-3players soft-EYE-FOR-EYE patsy-3 spastic-3)
;;player1 2.98
;;player2 2.98
;;player3 2.98


(play-loop-3players soft-EYE-FOR-EYE spastic-3 nasty-3)
;;player1 1.42
;;player2 1.42
;;player3 1.42

(play-loop-3players soft-EYE-FOR-EYE spastic-3 patsy-3)
;;player1 3.15
;;player2 4.42
;;player3 4.42

(play-loop-3players soft-EYE-FOR-EYE spastic-3 spastic-3)
;;player1 2.16
;;player2 2.72
;;player3 2.72

;;problem11, not done yet
(define (make-combined-strategies strat0 strat1 proc) (lambda (my-history other-history0 other-history1)
	(proc (strat0 my-history other-history0) (strat1 my-history other-history1)


(define foo0 (make-combined-strategies
Eye-for-Eye Eye-for-Eye
(lambda (r1 r2) (if (or (string=? r1 "d") (string=? r2 "d")) "d" "c"))))

(define foo1 (make-combined-strategies
Eye-for-Eye Egalitarian
(lambda (r1 r2) (if (= (random 2) 0) r1 r2))))



(play-loop-3players nasty-3 nasty-3 foo0)
;;player1 1.02
;;player2 1.02
;;player3 1.02
(play-loop-3players nasty-3 patsy-3 foo0)
;;player1 3.02
;;player2 2.06
;;player3 2.06
(play-loop-3players nasty-3 spastic-3 foo0)
;;player1 2.02
;;player2 .51
;;player3 .51
(play-loop-3players patsy-3 nasty-3 foo0)
;;player1 .01
;;player2 3.01
;;player3 3.01
(play-loop-3players patsy-3 patsy-3 foo0)
;;player1 4
;;player2 4
;;player3 4
(play-loop-3players patsy-3 spastic-3 foo0)
;;player1 2.04
;;player2 3.52
;;player3 3.52
(play-loop-3players spastic-3 nasty-3 foo0)
;;player1 .51
;;player2 2.03
;;player3 2.03
(play-loop-3players spastic-3 patsy-3 foo0)
;;player1 3.43
;;player2 1.67
;;player3 1.67
(play-loop-3players spastic-3 spastic-3 foo0)
;;player1 2.03
;;player2 1.96
;;player3 1.96


(play-loop-3players nasty-3 nasty-3 foo1)
;;player1 1.01
;;player2 1.01
;;player3 1.01
(play-loop-3players nasty-3 patsy-3 foo1)
;;player1 3.91
;;player2 .91
;;player3 .91
(play-loop-3players nasty-3 spastic-3 foo1)
;;player1 2.31
;;player2 .92
;;player3 .92
(play-loop-3players patsy-3 nasty-3 foo1)
;;player1 1.07
;;player2 4.07
;;player3 4.07
(play-loop-3players patsy-3 patsy-3 foo1)
;;player1 4
;;player2 4
;;player3 4
(play-loop-3players patsy-3 spastic-3 foo1)
;;player1 3.03
;;player2 4.48
;;player3 4.48
(play-loop-3players spastic-3 nasty-3 foo1)
;;player1 .93
;;player2 2.56
;;player3 2.56
(play-loop-3players spastic-3 patsy-3 foo1)
;;player1 3.9
;;player2 2.56
;;player3 2.56
(play-loop-3players spastic-3 spastic-3 foo1)
;;player1 2.46
;;player2 2.52
;;player3 2.52


;;problem12, not done yet
(define (make-history-summary hist-0 hist-1 hist-2)
	(list (make-cc hist-0 hist-1 hist-2) (make-cd hist-0 hist-1 hist-2) (make-dd hist-0 hist-1 hist-2)))

(define (make-cc hist-0 hist-1 hist-2)
	(list (make-ccc hist-0 hist-1 hist-2) (make-ccd hist-0 hist-1 hist-2) (make-ccttl hist-0 hist-1 hist-2)))

(define (make-cd hist-0 hist-1 hist-2)
	(list (make-cdc hist-0 hist-1 hist-2) (make-cdd hist-0 hist-1 hist-2) (make-cdttl hist-0 hist-1 hist-2)))

(define (make-dd hist-0 hist-1 hist-2)
	(list (make-ddc hist-0 hist-1 hist-2) (make-ddd hist-0 hist-1 hist-2) (make-ddttl hist-0 hist-1 hist-2)))

(define (make-ccc hist-0 hist-1 hist-2)
	(if (null? (cdr hist-1)) 0
		(if (and (equal? "c" (car hist-0)) (equal? "c" (cadr hist-1)) (equal? "c" (cadr hist-2)))
			(+ 1 (make-ccc (cdr hist-0) (cdr hist-1) (cdr hist-2)))
			(+ 0 (make-ccc (cdr hist-0) (cdr hist-1) (cdr hist-2))))))

(define (make-ccd hist-0 hist-1 hist-2)
	(if (null? (cdr hist-1)) 0
		(if (and (equal? "d" (car hist-0)) (equal? "c" (cadr hist-1)) (equal? "c" (cadr hist-2)))
			(+ 1 (make-ccd (cdr hist-0) (cdr hist-1) (cdr hist-2)))
			(+ 0 (make-ccd (cdr hist-0) (cdr hist-1) (cdr hist-2))))))

(define (make-ccttl hist-0 hist-1 hist-2)
	(+ (make-ccc hist-0 hist-1 hist-2) (make-ccd hist-0 hist-1 hist-2)))

(define (make-ddc hist-0 hist-1 hist-2)
	(if (null? (cdr hist-1)) 0
		(if (and (equal? "c" (car hist-0)) (equal? "d" (cadr hist-1)) (equal? "d" (cadr hist-2)))
			(+ 1 (make-ddc (cdr hist-0) (cdr hist-1) (cdr hist-2)))
			(+ 0 (make-ddc (cdr hist-0) (cdr hist-1) (cdr hist-2))))))

(define (make-ddd hist-0 hist-1 hist-2)
	(if (null? (cdr hist-1)) 0
		(if (and (equal? "d" (car hist-0)) (equal? "d" (cadr hist-1)) (equal? "d" (cadr hist-2)))
			(+ 1 (make-ddd (cdr hist-0) (cdr hist-1) (cdr hist-2)))
			(+ 0 (make-ddd (cdr hist-0) (cdr hist-1) (cdr hist-2))))))

(define (make-ddttl hist-0 hist-1 hist-2)
	(+ (make-ddc hist-0 hist-1 hist-2) (make-ddd hist-0 hist-1 hist-2)))

(define (make-cdc hist-0 hist-1 hist-2)
	(if (null? (cdr hist-1)) 0
		(if (and (equal? "c" (car hist-0))
			(or
				(and (equal? "c" (cadr hist-1)) (equal? "d" (cadr hist-2)))
				(and (equal? "d" (cadr hist-1)) (equal? "c" (cadr hist-2)))))
			(+ 1 (make-cdc (cdr hist-0) (cdr hist-1) (cdr hist-2)))
			(+ 0 (make-cdc (cdr hist-0) (cdr hist-1) (cdr hist-2))))))

(define (make-cdd hist-0 hist-1 hist-2)
	(if (null? (cdr hist-1)) 0
		(if (and (equal? "d" (car hist-0))
			(or
				(and (equal? "c" (cadr hist-1)) (equal? "d" (cadr hist-2)))
				(and (equal? "d" (cadr hist-1)) (equal? "c" (cadr hist-2)))))
			(+ 1 (make-cdd (cdr hist-0) (cdr hist-1) (cdr hist-2)))
			(+ 0 (make-cdd (cdr hist-0) (cdr hist-1) (cdr hist-2))))))

(define (make-cdttl hist-0 hist-1 hist-2)
	(+ (make-cdc hist-0 hist-1 hist-2) (make-cdd hist-0 hist-1 hist-2)))



(define (summary-get-cc summary)
	(car summary))
(define (summary-get-cd summary)
	(cadr summary))
(define (summary-get-dd summary)
	(caddr summary))

(define (summary-get-ccc summary)
	(car (summary-get-cc summary)))
(define (summary-get-ccd summary)
	(cadr (summary-get-cc summary)))
(define (summary-get-ccttl summary)
	(caddr (summary-get-cc summary)))

(define (summary-get-cdc summary)
	(car (summary-get-cd summary)))
(define (summary-get-cdd summary)
	(cadr (summary-get-cd summary)))
(define (summary-get-cdttl summary)
	(caddr (summary-get-cd summary)))

(define (summary-get-ddc summary)
	(car (summary-get-dd summary)))
(define (summary-get-ddd summary)
	(cadr (summary-get-dd summary)))
(define (summary-get-ddttl summary)
	(caddr (summary-get-dd summary)))


;;problem13, not done yet
(define (get-probability-of-c summary)
	(list (make-probability-cc summary) (make-probability-cd summary) (make-probability-dd summary)))

(define (make-probability-cc summary)
	(if (equal? (summary-get-ccttl summary) 0) ()
		(* 1.0 (/  (summary-get-ccc summary) (summary-get-ccttl summary)))))

(define (make-probability-cd summary)
	(if (equal? (summary-get-cdttl summary) 0) ()
		(* 1.0 (/  (summary-get-cdc summary) (summary-get-cdttl summary)))))

(define (make-probability-dd summary)
	(if (equal? (summary-get-ddttl summary) 0) ()
		(* 1.0 (/  (summary-get-ddc summary) (summary-get-ddttl summary)))))


;;problem14, not done yet
(define (soft-eye-for-eye?  hist0 hist1 hist2)
	(test-entry (list 1 1 0)
		(get-probability-of-c (make-history-summary hist0 hist1 hist2))))

(define (dont-tolerate-fools hist0 hist1 hist2)
	(if (< (number-of-plays hist0) 10) "c"
		(if (and (could-he-be-a-fool? hist1 hist0 hist2) (could-he-be-a-fool? hist2 hist0 hist1)) "d" "c")))

(play-loop-3players dont-tolerate-fools patsy-3 patsy-3)
;;player1 4.89
;;player2 2.2
;;player3 2.2

(define *game-association-list*
(list (list (list "c" "c" "c") (list 4 4 4))
(list (list "c" "c" "d") (list 2 2 5))
(list (list "c" "d" "c") (list 2 5 2))
(list (list "d" "c" "c") (list 5 2 2))
(list (list "c" "d" "d") (list 0 3 3))
(list (list "d" "c" "d") (list 3 0 3))
(list (list "d" "d" "c") (list 3 3 0))
(list (list "d" "d" "d") (list 1 1 1))))

(define *game-association-list*
	(list (list (list "c" "c") (list 3 3))
	(list (list "c" "d") (list 0 5))
	(list (list "d" "c") (list 5 0))
	(list (list "d" "d") (list 1 1))))


(define a-play (make-play "c" "d"))
(extract-entry a-play *game-association-list*)