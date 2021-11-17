; set random seed
; define turtles
; surely this isn't the right way to do this...
(defn make-turtle [name]
  ; A turtle is a function with a unique name (otherwise it will overwrite
  ; another turtle, I guess)
  (defn name [cmd & args] ; simple cmds can return turtle parameter values
    (def boldness (rand-int 8))
    (def vengefulness (rand-int 8))
    (def meta-vengefulness (rand-int 8))
    (def utility 0)
    (cond 
      ((= cmd "move")
       (if (> boldness (rand-int))
         (
          (def utility (+ 3 utility))
          (other-turtles name (fn [turtle]
                                (turtle lose-utility 1))))))
;      ((= cmd "lose-utility") (lose-utility ))
      ((= cmd "boldness") boldness)
      ((= cmd "vengefulness") vengefulness)
      ((= cmd "meta-vengefulness") meta-vengefulness)
      ((= cmd "utility") utility)
      (:else)
    )
  ))
    ; ensure unique name?
    ; move mechanics
    ; update estimate of being punished?
    ; find all the other turtles?
; define world/clock/population
(def population
  (map 
    #(make-turtle %) 
    (map 
      #(clojure.string/replace % #"^" "T") 
      (range 100))))
; define interactions
(defn make-move [self]
  (if (< (rand-int 8) (self boldness))
    ((self add-wealth 3)
     (other-turtles self add-wealth -1))
    (
     ; do nothing
     )))
(defn other-turtles 
  ([self] (filter population #(not (= self))))
  ([self f] (map f (other-turtles self))))

; loop over them
(defn tick [agentset]
  (map make-move agentset))

to go
  ask turtles [
    ;; alternate form for next line:
    ;; if random 8 < boldness [
    if p-observe-defection < (boldness / 8) [
      set utility utility + temptation ; defection-reward
      ask other turtles [
        suffer-defection myself
      ]
    ]
  ]
  if ticks mod 10 = 0 [
    genetic-algorithm
  ]
  tick
end

to suffer-defection [by-turtle]
  set utility utility + hurt-by-others
  if norms? [
    if random-float 1 < p-observe-defection [
      ifelse random 8 < vengefulness [
        set utility utility + enforcement-cost
      ][
        if meta-norms? [
          ask other turtles with [who != by-turtle] [
            if random 8 < meta-vengefulness [
              set utility utility + enforcement-cost
              ask myself [
                set utility utility + punishment
              ]
            ]
          ]
        ]
      ]
    ]
  ]
end


to genetic-algorithm
  ask max-n-of 5 turtles [utility] [ reproduce ]
  ask min-n-of 5 turtles [utility] [ die ]
  ask turtles [ set utility 0 ]
end

to reproduce
  ; not exactly how Axelrod did it, but I'm storing strategies as ints instead of bits and want to
  ; get the model running before figuring out how to adjust the population to stay constant
  hatch 1 [
    if random 100 < 1 [
      set boldness random 8
    ]
    if random 100 < 1 [
      set vengefulness random 8
    ]
    if random 100 < 1 [
      set meta-vengefulness random 8
    ]
    setxy (4 * boldness + 2) (4 * vengefulness + 2)
    set size 2
  ]
end
