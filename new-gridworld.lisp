;;; Martin Helmrich and Sisipho Zinja

;;; Agent is placed in a model of the University of Rochester campus.
;;; The campus will include a Dormitory that the agent is initially placed in, Wilson Commons which is a student center,
;;; Douglas Dining Hall for eating, and Hylan Hall lecture hall to learn new information and to take exams,
;;; Rhush Rhees Library to study and prepare for the exams.
;;; Goergen Athletic Center to keep fit and boost serotonin levels.

;;; We do not need Wegmans Food Market to buy groceries because we satisfy eating state-value at Douglas or the movie theater for entertainment because we have WilCo

;;; Actions performed at each location include:
;;; Dormitory; the agent is able to sleep and fulfill the basic need for rest.
;;; WilCo; the agent will interact and socialize with other students for entertainment.
;;; Douglas; the agent will eat a burger here when they are hungry to gain energy.
;;; Hylan Hall; the agent will learn new information through attending classes taught by a professor and will also take exams here.
;;;             The agent will do better when they have studied for the exams in Rhush Rhees and attended all the lectures.
;;; Goergen Athletic Center; to exercise and keep energy levels up.
;;; Rhush Rhees Library; the agent will study for exams here, so that taking exams will be highly rewarding.
(def-roadmap '(dorm wilco douglas hylan goergen rr) '((path1 dorm 1 wilco) (path2 dorm 1 douglas) (path3 douglas 1 rr) (path4 dorm 1 rr)
                                                                           (path5 douglas 1 wilco) (path6 wilco 1 hylan)
                                                                           (path7 wilco 1 rr) (path8 rr 1 hylan) (path9 goergen 1 wilco)))
(def-object 'robot '(is_animate can_talk))
(def-object 'professor '(is_animate can_talk)) ;;; professor in Hylan can talk and can perform/has action of grading the exam
(def-object 'book '(is_inanimate is_potable)) ;;; when picked up the AG has studied
(def-object 'burger '(is_inanimate is_edible (has_cost 5.0)))
(def-object 'threadmill '(is_inanimate is_playable)) ;;; threadmill at Goergen
(def-object 'coffee '(is_inanimate is_potable (has_cost 3.0))) ;;; coffee when studying in Rhush Rhees

;;; General knowledge of AG from the onset
;;; Initially AG is in the dorm, not tired, and has hunger level of 5.0 and a thirst level of 3.0.
;;; In addition to the knowledge of the road network, the existence of objects at the dorm and their general properties.,
;;; AG knows that burger is in Douglas, coffee is in Rhush Rhees, and the threadmill is in Goergen.
;;; The AG knows that the professor can talk, is in Hylan, and knows what will be on the exam.
;;; The professor will grade the exam written by the agent, and will know whether the agent passes or not.

;;; Note that we create some "current facts" about
;;; AG that are really about the state of the University campus
;;; this is just a way of ensuring that AG knows these
;;; facts right at the outset.
(place-object 'AG 'robot 'dorm 0
  nil ; no associated-things
  ; current facts
  '((is_hungry_to_degree AG 5.0)
	(is_thirsty_to_degree AG 3.0)
    (is_tired_to_degree AG 0.0)
    (can_talk professor)
    (is_at professor hylan)
    (is_at coffee2 rr)
    (is_at burger3 douglas)
    (is_at threadmill4 goergen)
    (is_at book5 rr)
     ; Note AG knows (is_potable coffee2), (is_edible burger3), (is_playable threadmill4) and (is_writable exam5) right after the call to function initialize-state-node
     ; therefore, these are all hidden from AG from onset to prevent
     ; AG from knowing about the writability of the exam2 and edibility of the burger, portability of the coffee etc on the outset until it goes to locations.
     ; AG will also know about the playability of the threadmill, (is_playable threadmill4) later and will be hidden as well.
     ; one would need to specify general inference rules from AG to use,
     ; separate from *general knowledge*
  )
  ; propositional attitudes
  '((knows AG (whether (is_playable threadmill4)))
    (knows AG (whether (is_edible burger3)))
    (knows AG (that (knows robot (whether (is_potable coffee2)))))
   )
)

;;; initial placement of objects
(place-object 'burger3 'burger 'douglas 0
	nil ; no associated-things
	; current facts
	'((is_edible burger3)
	 )
    nil ; propositional attitudes
)

(place-object 'book5 'book 'rr 0
	nil ; no associated-things
	; current facts
	'((is_potable book5)
	 )
    nil ; propositional attitudes
)

(place-object 'threadmill4 'threadmill 'home 0
	nil ; no associated-things
	'((is_playable threadmill4)
	 )
    nil ; propositional attitudes
)

;(setq *occluded-preds*
;    '(is_playable knows is_edible is_potable)
; We omit this, as *occluded-preds* is currently already set in
; "gridworld-definitions.lisp".
; the search beam use?

(setq *operators* '(walk eat answer_user_ynq answer_user_whq sleep drink study take_exam ask+whether play))
(setq *search-beam*
      ;(list (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) (cons 3 *operators*) ))
     	(list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk, AG walks from point ?x to point ?y on road ?z, with
;; initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk
  (make-op :name 'walk :pars '(?x ?y ?z ?f)
	:preconds '((is_at AG ?x)
         				(is_on ?x ?z)
         				(is_on ?y ?z) (point ?y)
         				(navigable ?z)
                (is_tired_to_degree AG ?f) )
  :effects '((is_at AG ?y)
       		   (not (is_at AG ?x))
            ;(is_tired_to_degree AG (+ ?f 0.5))
            (is_tired_to_degree AG (+ ?f (* 0.5 (distance_from+to+on? ?x ?y ?z))))
            (not (is_tired_to_degree AG ?f)) )
  :time-required '(distance_from+to+on? ?x ?y ?z)
  :value '(- 3 ?f)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This evaluation function returns the numeric distance from location arg
;; x to location arg y along the path arg z. This function is called by
;; functions walk.actual and walk.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun distance_from+to+on? (x y z)
	(let	(result pt1 pt2 units index1 index2 str)
			; If both x and y are named road points, simply do a look-up.
		(if (and (evalFunctionPredicate (cons 'point (list x))) (evalFunctionPredicate (cons 'point (list y))))
			(dolist (triple (get x 'next))
				(when (and (eq z (first triple)) (eq y (second triple)))
					(setq result (third triple))

					(return-from distance_from+to+on? result)
				)
			)
			; Otherwise, x is of the form (the_pt+units_from+towards+on_road? ?d ?a ?b ?r),
			; and parse the result to get the distance.
			(progn
				(if (atom x)
					(setq str (string x))
					(setq str (apply (car x) (cdr x))); (string x))
				)
				(setq index1 (search "PT_" str))
				(setq index2 (search "_UNITS" str))
				(setq units (parse-integer (subseq str (+ index1 3) index2)))
				(setq index1 (search "FROM_" str))
				(setq index2 (search "_TOWARDS" str))
				(setq pt1 (INTERN (string-upcase (subseq str (+ index1 5) index2))))
				(setq index1 (+ index2 9))
				(setq index2 (search "_ON" str))
				(setq pt2 (INTERN (string-upcase (subseq str index1 index2))))
				(if (and (eq pt1 x) (eq pt2 y))
					(return-from distance_from+to+on? (- (distance_from+to+on? pt1 pt2 z) units))
					(return-from distance_from+to+on? units)
				)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk.actual, AG walks from point ?x to point ?y on road ?z,
;; with initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk.actual
	(make-op.actual :name 'walk.actual :pars '(?x ?y ?z ?f)
	:startconds '((is_at AG ?x)
				  (is_on ?x ?z)
				  (is_on ?y ?z) (point y)
				  (navigable ?z)
                  (is_tired_to_degree AG ?f) )
    :stopconds '((not (navigable ?z))
    			 (is_at AG ?y) )
    :deletes '((is_at AG ?#1)
    		   (is_tired_to_degree AG ?#2))
    :adds '((is_at AG (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z))
    		(is_at AG (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z))
    	    (is_on (the_pt+units_from+towards+on_road? (* 1 (elapsed_time?)) ?x ?y ?z) ?z)
    	    (is_on (the_pt+units_from+towards+on_road? (- (distance_from+to+on? ?x ?y ?z) (* 1 (elapsed_time?))) ?y ?x ?z) ?z)
    		(is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?)))) )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep, AG sleeps to relieve his fatigue ?f, but experiences
;; an increase in his hunger ?h.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep
	(make-op :name 'sleep :pars '(?f ?h) ; level of fatigue ?f
                                         ; {0, 0.5, 1.0, 1.5, ...}
                                         ; similarly for hunger ?h
    :preconds '((is_at AG home)
                (is_tired_to_degree AG ?f)
                (>= ?f 2.5);(>= ?f 0.5)
                (is_hungry_to_degree AG ?h)
                (> ?f ?h) ; more tired than hungry
                (not (there_is_a_fire))
                (not (there_is_a_flood)) )
    :effects '((is_tired_to_degree AG 0.0)
               (not (is_tired_to_degree AG ?f))
               (is_hungry_to_degree AG (+ ?h (* 0.5 ?f))) )
    :time-required '(* 4 ?f)
    :value '(* 1 ?f)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep.actual, AG sleeps to relieve his fatigue ?f, but
;; experiences an increase in his hunger ?h.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep.actual
	(make-op.actual :name 'sleep.actual :pars '(?f ?h) ; level of fatigue ?f
                                                	   ; level of hunger ?h
    :startconds '((is_at AG home)
                  (is_tired_to_degree AG ?f)
                  (>= ?f 2.5)
                  (is_hungry_to_degree AG ?h)
                  (> ?f ?h) ); more tired than hungry
    :stopconds '((there_is_a_fire)
    						 (is_tired_to_degree AG 0.0))
    :deletes '((is_tired_to_degree AG ?#1)
               (is_hungry_to_degree AG ?#2) )
    :adds '((is_tired_to_degree AG (- ?f (* 0.5 (elapsed_time?))))
            (is_hungry_to_degree AG (+ ?h (* 0.25 (elapsed_time?)))) )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If hungry, at the same location ?y as is an is_edible food item ?x, and
;; aware of the item being is_edible, then AG can eat the item to assuage his
;; hunger ?h provided there is no fire or flood. Currently, food items are
;; inexhaustible.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat
	(make-op :name 'eat :pars '(?h ?x ?y) ; level of hunger ?h
	:preconds '( (is_hungry_to_degree AG ?h)
				 (>= ?h 2.0)
				 (is_at AG ?y)
				 (is_at ?x ?y)
				 (is_edible ?x)
				 (knows AG (whether (is_edible ?x)))
				 (not (there_is_a_fire))
                 (not (there_is_a_flood)) )
	:effects '( (is_hungry_to_degree AG 0.0)
				(not (is_hungry_to_degree AG ?h)) )
	:time-required 1
	:value '(* 2 ?h)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is an is_edible food item ?x and aware of
;; the item being is_edible, and as long as he is hungry, then AG can eat the
;; item to assuage his hunger ?h provided there is no fire or flood.
;; Currently, food items are inexhaustible.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat.actual
	(make-op.actual :name 'eat.actual :pars '(?h ?x ?y)
	:startconds '( (is_hungry_to_degree AG ?h)
				   (>= ?h 2.0)
				   (is_at AG ?y)
				   (is_at ?x ?y)
				   (is_edible ?x)
				   (knows AG (whether (is_edible ?x))) )
	:stopconds '( (there_is_a_fire)
				  (there_is_a_flood)
				  (is_hungry_to_degree AG 0.0) )
	:deletes '( (is_hungry_to_degree AG ?#1) )
	:adds '( (is_hungry_to_degree AG 0.0) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If thirsty, at the same location ?y as is a is_potable drink item ?x, and
;; aware of it being is_potable, then AG can drink ?x to sate his thirst ?h.
;; Currently, drink items are inexhaustible.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq drink
	(make-op :name 'drink :pars '(?h ?x ?y) ; level of thirst ?h
	:preconds '( (is_thirsty_to_degree AG ?h)
				 (> ?h 0.0)
				 (is_at AG ?y)
				 (is_at ?x ?y)
				 (is_potable ?x)
				 (knows AG (whether (is_potable ?x)))
				 (not (there_is_a_fire))
         (not (there_is_a_flood)) )
	:effects '( (is_thirsty_to_degree AG 0.0)
				(not (is_thirsty_to_degree AG ?h)) )
	:time-required 1
	:value '(* 2 ?h)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is a is_potable drink item ?x and aware of
;; it being is_potable, and as long as he is thirsty, then AG can drink ?x to
;; sate his thirst ?h. Currently, drink items are inexhaustible.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq drink.actual
	(make-op.actual :name 'drink.actual :pars '(?h ?x ?y)
	:startconds '( (is_thirsty_to_degree AG ?h)
				   (> ?h 0.0)
				   (is_at AG ?y)
				   (is_at ?x ?y)
				   (is_potable ?x)
				   (knows AG (whether (is_potable ?x))) )
	:stopconds '( (there_is_a_fire)
				  		(there_is_a_flood)
				  		(is_thirsty_to_degree AG 0.0) )
	:deletes '( (is_thirsty_to_degree AG ?#1) )
	:adds '( (is_thirsty_to_degree AG 0.0) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; action for studying
;; agent picks up a book in RR as preconds AND
;; agent speaks to the professor as preconds for studying to evaluate to true
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq study
  (make-op :name 'study :pars '(?b ?f ?h) ; study when AG not tired/hungry
  :preconds '( (is_at AG rr)
          (is_exam_relevant ?b)
          (is_tired_to_degree AG ?f)
          (>= ?f 0.0) ; not tired
          (is_hungry_to_degree AG ?h)
          (>= ?h 0.0) ; not hungry
          (is_at ?b hylan)
          (not (there_is_a_fire))
          (not (there_is_a_flood)) )
	:effects '( (has_studied AG)
          (is_hungry_to_degree AG (+ ?h 1.0))
          (is_tired_to_degree AG (+ ?h 1.0)) ) ; effects that make sure agent has studied
	:time-required 1
	:value '(2) ; increase in studying preparedness maybe as variable p
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; action for studying.actual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq study.actual
	(make-op.actual :name 'study.actual :pars '(?b ?f ?h)
	:startconds '( (is_at AG rr)
           (is_exam_relevant ?b)
           (is_tired_to_degree AG ?f)
           (>= ?f 0.0)
           (is_hungry_to_degree AG ?h)
				   (>= ?h 0.0)
           (is_at ?b rr) )
	:stopconds '( (has_studied AG)
          (there_is_a_fire)
				  (there_is_a_flood)
          (is_tired_to_degree AG 0.0)
				  (is_hungry_to_degree AG 0.0) )
	:deletes '( (is_hungry_to_degree AG ?#1)
            (is_tired_to_degree AG ?#2) )
	:adds '( (is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?))))
           (is_hungry_to_degree AG (+ ?h (* 0.5 (elapsed_time?)))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; action for exam taking
;; when AG has studying as true, they pass the exam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setq testing
;   (make-op :name 'testing :pars '(?e ?f ?h) ; exam e testing when AG not tired/hungry
;   :preconds '( (is_at AG hylan)
;           (has_studied AG)
;           (is_tired_to_degree AG ?f)
;           (>= ?f 0.0) ; not tired
;           (is_hungry_to_degree AG ?h)
;           (>= ?h 0.0) ; not hungry
;           (is_at ?e hylan)
;           (not (there_is_a_fire))
;           (not (there_is_a_flood)) )
; 	:effects '( ()
;           (is_hungry_to_degree AG (+ ?h 1.0))
;           (is_tired_to_degree AG (+ ?h 1.0)) ) ; effects that make sure agent has studied
; 	:time-required 1
; 	:value '(2) ; increase in studying preparedness maybe as variable p
; ))
