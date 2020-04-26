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
(def-object 'professor '(is_animate can-talk)) ;;; professor in Hylan can talk and can perform/has action of grading the exam
(def-object 'exam '(is_inanimate is_writable)) ;;; not sure how to quantify grades with its definition
(def-object 'burger '(is_inanimate is_edible (has_cost 5.0)))
(def-object 'threadmill '(is_inanimate is_playable)) ;;; threadmill at Goergen
(def-object 'coffee '(is_inanimate is_potable (has_cost 3.0))) ;;; coffee when studying in Rhush Rhees

;;; General knowledge of AG from the onset
;;; Initially AG is in the dorm, not tired, and has hunger level of 5.0 and a thirst level of 3.0.
;;; In addition to the knowledge of the road network, the existence of objects at the dorm and their general properties.,
;;; AG knows that burger is in Douglas, coffee is in Rhush Rhees, and the threadmill is in Goergen.
;;; The AG knows that the professor can talk, is in Hylan, and knows what will be on the exam.
;;; The professor will grade the exam written by the agent, and will know whether the agent passes or not.

;; Note that we create some "current facts" about
;; AG that are really about the state of the University campus
;; this is just a way of ensuring that AG knows these
;; facts right at the outset.
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
    (is_at exam5 hylan)
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
    (knows AG (that (knows professor (whether (is_writable exam5)))))
    ;professor's knowledge is occluded and so filtered out. So the bug fix for now is that when you want
    ;to initially assign to AG the knowledge of some other agent's knowledge, you should prefix
    ;that with `knows AG that', and hence the form (knows AG (that (knows another_agent ...))).
   )
)
