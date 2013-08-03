(ns granjure.syntax)

(defn forall [format variables formula]
  (let   [ len       (count formula)
           variables (set variables)
         ]
  (fn [tokens] (cond
    (< (count tokens) len)
      tokens
    :else (let [ tokens' (drop len tokens)
                 tokens  (take len tokens)
               ]
    (cond
      (every? true? (map (fn [f t] (or (contains? variables f) (= f t))) formula tokens))
        (cons (apply format tokens) tokens')
      :else
        (concat tokens tokens')))))))
