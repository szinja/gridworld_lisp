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
(def-roadmap '(dorm douglas rr wilco hylan goergen)
    '((path1 dorm 1 goergen) (path2 dorm 1 douglas)
      (path3 dorm 1 rr) (path4 dorm 1 wilco)
      (path5 douglas 1 goergen) (path6 douglas 1 wilco)
      (path7 douglas 1 rr) (path8 rr 1 wilco)
      (path9 rr 1 hylan) (path10 wilco 1 hylan)))

(def-object 'student '(is_animate can_talk))
(def-object 'professor '(is_animate can_talk))
(def-object 'burger '(is_inanimate is_edible (has_cost 5.0)))
(def-object 'coffee '(is_inanimate is_potable (has_cost 3.0)))
(def-object 'book '(is_inanimate is_potable))
(def-object 'exam '(is_inanimate is_takeable))

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
(place-object 'AG 'student 'dorm 0
  nil ; no associated-things
  ; current facts
  '((is_hungry_to_degree AG 4.0)
    (is_thirsty_to_degree AG 2.0)
    (is_tired_to_degree AG 0.0)
    (is_at coffee1 rr)
    (is_at burger1 douglas)
    (is_at book1 rr)
    (is_at exam1 hylan)
    (can_talk professor1)
    (is_at professor1 hylan)
     ; Note AG knows (is_potable coffee2), (is_edible burger3), (is_playable threadmill4) and (is_writable exam5) right after the call to function initialize-state-node
     ; therefore, these are all hidden from AG from onset to prevent
     ; AG from knowing about the writability of the exam2 and edibility of the burger, portability of the coffee etc on the outset until it goes to locations.
     ; AG will also know about the playability of the threadmill, (is_playable threadmill4) later and will be hidden as well.
     ; one would need to specify general inference rules from AG to use,
     ; separate from *general knowledge*
  )
  ; propositional attitudes
  '((knows AG (whether (is_edible burger1)))
    (knows AG (whether (is_potable coffee1))))
  )
)

;;; Placing objects in the world
(place-object 'professor1 'professor 'hylan 0
  nil
  nil
  nil
)

(place-object 'burger1 'burger 'douglas 0
	nil
	'((is_edible burger1))
  nil
)

(place-object 'coffee1 'coffee 'rr 0
	nil
	'((is_potable coffee1))
  nil
)

(place-object 'book1 'book 'rr 0
  nil
  '((is_exam_relevant book1))
  nil
)

(place-object 'exam1 'exam 'hylan 0
  nil
  '((is_exam_relevant book1))
  nil
)

;; setting operators
(setq *operators* '(walk eat answer_user_ynq answer_user_whq sleep
                    drink ask+whether play study take_exam) )

;; setting search-beam
(setq *search-beam*
  (list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*)) )


;;; Defining custom actions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD DESCRIPTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq study
  (make-op :name 'study :pars '(?f ?h ?x ?b) ; ?f fatigue, ?h hunger
  :preconds '( (not (has_studied AG))
          (is_at AG ?x)
          (is_at ?b ?x) ; needs book ?b to study
          (is_exam_relevant ?b)
          (is_tired_to_degree AG ?f)
          (< ?f 3.0) ; cannot study when tired
          (is_hungry_to_degree AG ?h)
          (< ?h 3.0) ; cannot study when hungry
          (not (there_is_a_fire))
          (not (there_is_a_flood)) )
  :effects '( (has_studied AG) ; agent has now studied
          (is_hungry_to_degree AG (+ ?h 0.5)
          (is_tired_to_degree AG (+ ?h 0.5))) )
  :time-required 1
  :value 1
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD DESCRIPTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq study.actual
  (make-op.actual :name 'study.actual :pars '(?f ?h ?x ?b)
  :startconds '( (not (has_studied AG))
          (is_at AG ?x)
          (is_at ?b ?x)
          (is_exam_relevant ?b)
          (is_tired_to_degree AG ?f)
          (< ?f 3.0)
          (is_hungry_to_degree AG ?h)
          (< ?h 3.0) )
  :stopconds '( (has_studied AG)
          (there_is_a_fire)
          (there_is_a_flood) )
  :deletes '( (is_tired_to_degree AG ?#1)
        (is_thirsty_to_degree AG ?#2) )
  :adds '( (has_studied AG)
      (is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?))))
      (is_hungry_to_degree AG (+ ?h (* 0.5 (elapsed_time?)))) )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD DESCRIPTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq take_exam
  (make-op :name 'take-exam :pars '(?f ?h ?x ?e)
  :preconds '( (has_studied AG)
          (is_at AG ?x)
          (is_at ?e ?x)
          (is_takeable ?e)
          (is_tired_to_degree AG ?f)
          (< ?f 3.0)
          (is_hungry_to_degree AG ?h)
          (< ?h 3.0)
          (not (there_is_a_fire))
          (not (there_is_a_flood)) )
  :effects '( (not (is_at ?e ?x)
          (is_hungry_to_degree AG (+ ?h 0.5)
          (is_tired_to_degree AG (+ ?h 0.5))) )
  :time-required 1
  :value 5
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADD DESCRIPTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq take_exam.actual
  (make-op.actual :name 'take_exam.actual :pars '(?f ?h ?x ?e)
  :startconds '( (has_studied AG)
          (is_at AG ?x)
          (is_at ?e ?x)
          (is_takeable ?e)
          (is_tired_to_degree AG ?f)
          (< ?f 3.0)
          (is_hungry_to_degree AG ?h)
          (< ?h 3.0) )
  :stopconds '( (not (is_at ?#4 ?#3))
          (there_is_a_fire)
          (there_is_a_flood) )
  :deletes '( (is_at ?#4 ?#3)
        (is_tired_to_degree AG ?#1)
        (is_thirsty_to_degree AG ?#2) )
  :adds '(
      (is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?))))
      (is_hungry_to_degree AG (+ ?h (* 0.5 (elapsed_time?)))) )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CODE BELOW FROM PROVIDED SAMPLE GRIDWORLD-WORLD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq *occluded-preds*
;    '(is_playable knows is_edible is_potable)
; We omit this, as *occluded-preds* is currently already set in
; "gridworld-definitions.lisp".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator fire.actual is the exogenous fire operator.  As long as there
;; is no rain, a spontaneous fire has a 5% chance of starting; once
;; it has started, it has a 50% chance of stopping, and it also goes out
;; as soon as there is rain.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq fire.actual
	(make-op.actual :name 'fire.actual :pars '()
    :startconds '((not (there_is_rain))
				  (= 3 (random 20))) ; 5% chance of fire starting
    :starredStopConds '((= 1 (random 2)) ; 50% chance of stopping after starting
						(there_is_rain))
    :starredDeletes '((there_is_a_fire))
    :starredAdds '((navigable PATH1) (navigable PATH2) (navigable PATH3) (navigable PATH4))
    :deletes '((navigable PATH1) (navigable PATH2) (navigable PATH3) (navigable PATH4))
    :adds '((there_is_a_fire))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator rain.actual is the exogenous rain operator.  Spontaneous rain
;; has a 33% chance of starting; once it has started, it has a 25% chance
;; of stopping.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq rain.actual
	(make-op.actual :name 'rain.actual :pars '()
    :startconds '((= 1 (random 3))) ; 33% chance of rain starting
    :starredStopConds '((= 2 (random 4))) ; 25% chance of stopping after starting
    :starredDeletes '((there_is_rain))
    :adds '((there_is_rain))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq? returns a well-formed formula indicating whether
;; or not the arg wff is currently in AG's KB, under the closed world
;; assumption. For example, if AG is currently hungry according to AG's KB,
;; then (is_hungry AG) is returned as the response to
;; (answer_to_ynq? '(is_hungry AG)); else, (not (is_hungry AG)) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq? (wff)
	(check-yn-fact-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_ynq.actual? returns a well-formed formula indicating
;; whether the arg wff is currently in AG's KB, under the closed world
;; assumption. In addition, the answer is translated into a proper English
;; sentence and printed on screen.  For example, if AG is currently hungry
;; according to AG's KB, then (is_hungry AG) is returned as the response to
;; (answer_to_ynq.actual? '(is_hungry AG)), and ``AG is hungry'' without the
;; double quotes is printed.  Otherwise, (not (is_hungry AG)) is
;; returned and ``it is not the case that AG is hungry'' is printed without
;; the double quotes.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_ynq.actual? (wff)
	(check-yn-fact-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq? returns a collection of well-formed formula(s)
;; as the answer to the arg wff reflecting what are currently in AG's KB,
;; under the closed world assumption. Arg wff is a wh-question that has
;; variables prefixed with ? appearing in slots filled by wh-words.
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; then ((likes AG APPLE1) (likes AG BANANA2)) is returned as response to
;; (answer_to_whq? '(likes AG ?wh)). If no answer is found,
;; then '(not (knows (AG the-answer))) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq? (wff)
	(check-whq-answer-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq.actual? returns a collection of well-formed
;; formula(s) as the answer to the arg wff reflecting what are currently in
;; AG's KB, under the closed world assumption. Arg wff is a wh-question
;; with variables prefixed with ? appearing in slots filled by wh-words.
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; ((likes AG APPLE1) (likes AG BANANA2)) is returned as the response to
;; (answer_to_whq.actual? '(likes AG ?wh)), and ``AG likes APPLE1'' and ``AG likes
;; BANANA2'' without double quotes are printed on two lines.  If no answer
;; is found, '(not (knows (AG the-answer))) is returned and ``it is not the
;; case that AG knows the answer'' without the double quotes is printed .
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq.actual? (wff)
	(check-whq-answer-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq, AG answers the yes-no question ?q asked
;; by USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq
      (make-op :name 'answer_user_ynq :pars '(?q)
        :preconds '( (wants USER (that (tells AG USER (whether ?q)))) )
        :effects '( (not (wants USER (that (tells AG USER (whether ?q)))))
                    (knows USER (that (answer_to_ynq? ?q)))
			  		)
        :time-required 1
        :value 10
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_ynq.actual, AG answers the yes-no question
;; ?q asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_ynq.actual
	(make-op.actual :name 'answer_user_ynq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells AG USER (whether ?q)))) )
	:stopconds '( (not (wants USER (that (tells AG USER (whether ?q))))) )
	:deletes '( (wants USER (that (tells AG USER (whether ?q)))) )
	:adds '( ;(knows USER (that (answer_to_ynq?.actual ?q)))
					 (says+to+at_time AG (that (answer_to_ynq.actual? ?q)) USER (current_time?))
					 (not (wants USER (that (tells AG USER (whether ?q)))))
		   	 )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq, AG answers the wh-question ?q asked by
;; USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq
	(make-op :name 'answer_user_whq :pars '(?q)
	:preconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:effects '( (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
				(knows USER (that (answer_to_whq? ?q)))
			  )
	:time-required 1
	:value 10
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq.actual, AG answers the wh-question ?q
;; asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq.actual
	(make-op.actual :name 'answer_user_whq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:stopconds '( (not (wants USER (that (tells AG USER (answer_to_whq ?q))))) )
	:deletes '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:adds	'( ;(knows USER (that (answer_to_whq.actual? ?q)))
			   (says+to+at_time AG (that (answer_to_whq.actual? ?q)) USER (current_time?))
			   (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
			 )
	)
)

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
;; If at the same location ?z as is an agent ?x who knows whether ?y holds
;; which AG does not know, then AG can ask ?x and know whether ?y holds.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ask+whether
	(make-op :name 'ask+whether :pars '(?x ?y ?z)
	:preconds '( (is_at AG ?z)
				 (is_at ?x ?z)
				 (can_talk ?x)
				 (knows ?x (whether ?y))
				 (not (knows AG (whether ?y))) )
	:effects '( (knows AG (whether ?y)) )
	:time-required 1
	:value 5
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?z as is an agent ?x who knows whether ?y holds
;; which AG does not know, then AG can ask ?x and know whether ?y holds.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ask+whether.actual
	(make-op.actual :name 'ask+whether.actual :pars '(?x ?y ?z)
	:startconds '( (is_at AG ?z)
				   (is_at ?x ?z)
				   (can_talk ?x)
				   (knows ?x (whether ?y))
				   (not (knows AG (whether ?y))) )
	:stopconds '( (knows AG (whether ?y)) )
	:deletes '( )
	:adds '( (knows AG (whether ?y)) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If bored, at the same location ?y as is a is_playable item ?x, and
;; aware of it being is_playable, then AG can play ?x to relieve his boredom
;; but also experience an increase in both his hunger ?h and fatigue ?f.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play
	(make-op :name 'play :pars '(?h ?f ?x ?y) ; level of hunger ?h
	:preconds '( (is_bored AG) 				  ; level of fatigue ?f
				 (is_at AG ?y)
				 (is_at ?x ?y)
				 (is_playable ?x)
				 (is_thirsty_to_degree AG ?h)
                 (is_tired_to_degree AG ?f)
				 (knows AG (whether (is_playable ?x))) )
	:effects '( (not (is_bored AG))
				(not (is_thirsty_to_degree AG ?h))
                (not (is_tired_to_degree AG ?f))
				(is_thirsty_to_degree AG (+ ?h 0.5))
                (is_tired_to_degree AG (+ ?f 0.5)) )
	:time-required 1
	:value 3
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is a is_playable item ?x and aware of it
;; being is_playable, and as long as AG is bored, then AG can play ?x to
;; relieve his boredom but also experience an increase in both his hunger
;; ?h and fatigue ?f.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq play.actual
	(make-op.actual :name 'play.actual :pars '(?h ?f ?x ?y)
	:startconds '( (is_bored AG)
				   (is_at AG ?y)
				   (is_at ?x ?y)
				   (is_playable ?x)
				   (is_thirsty_to_degree AG ?h)
                   (is_tired_to_degree AG ?f)
				   (knows AG (whether (is_playable ?x))) )
	:stopconds '( (not (is_bored AG)) )
	:deletes '( (is_tired_to_degree AG ?#2)
                (is_thirsty_to_degree AG ?#1)
                (is_bored AG) )
    :adds '( (is_tired_to_degree AG (+ ?f (* 0.5 (elapsed_time?))))
             (is_thirsty_to_degree AG (+ ?h (* 0.5 (elapsed_time?)))) )
	)
)
