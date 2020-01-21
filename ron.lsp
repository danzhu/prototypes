(:window_size 800 600
 :window_title "PAC-MAN"
 :fullscreen false

 :mouse_sensitivity 1.4
 :key_bidnings
 (("up" up)
  ("down" down)
  ("left" left)
  ("right" right))

 :difficulty_options
 (:start_difficulty easy
  :adaptive false))

(:materials
 (("metal" (:reflectivity 1.0))
  ("plastic" (:reflectivity 0.5)))

 :entities
 ((:name "hero" :material "metal")
  (:name "monster" :material "plastic")))

(tagged "sum" (object
               (((some "tagged")
                 (tagged "Prod" (object
                                 (((some "tag")
                                   (symbol "Sym"))
                                  ((some "data")
                                   (tagged "Opt" (symbol "Data")))))))
                ((some "object")
                 (tagged "List" (tagged "Prod" (object
                                                (((some "key")
                                                  (tagged "Opt" (symbol "Sym")))
                                                 ((some "value")
                                                  (symbol "Data"))))))))))
