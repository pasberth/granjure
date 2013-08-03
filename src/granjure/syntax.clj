(ns granjure.syntax)

(defn forall [format variables formula tokens]
  (let   [ len       (count formula) ] (cond
  (not (seq? tokens))
    tokens
  (< (count tokens) len)
    tokens
  :else (let [ tokens' (drop len tokens)
               tokens  (take len tokens)
               zipped  (map list formula tokens)
             ] (cond
  (every? (fn [[f t]] (or (contains? variables f) (= f t))) zipped)
    (let [ tokens (reduce (fn [r [f t]] (cond (contains? variables f) (conj r t) :else r)) [] zipped) ]
      (cons (apply format tokens) tokens'))
  :else
    (concat tokens tokens'))))))

(defn fixr [formula tokens]
  (reduce (fn [tokens i] (cond (> i (count tokens)) tokens :else (concat (take i tokens) (formula (drop i tokens)))))
          tokens
          (flatten (reverse (map #(repeat (inc %) %) (range (count tokens)))))))

(defn fixl [formula tokens]
  (reduce (fn [tokens i] (cond (> i (count tokens)) tokens :else (concat (take i tokens) (formula (drop i tokens)))))
          tokens
          (flatten (map #(repeat (- (count tokens) %) %) (range (count tokens))))))