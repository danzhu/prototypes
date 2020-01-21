(def [(zero (app int "0"))
      (one (app int "1"))
      (fact (abs n (app (app (app if (app (app ge one) (app trace n))) one)
                        (app (app mul n)
                             (app fact (app (app sub n) one))))))]
     (app fact (app int "5")))
