(ns looping-is-recursion)

(defn power
  ([base exp]
    (power base exp 1))
  ([base exp acc]
    (if (zero? exp)
      acc
      (if (> exp 0)
        (recur base (dec exp) (* base acc))))))

(defn last-element [a-seq]
  (let [end (rest a-seq)]
    (if (empty? end)
      (first a-seq)
      (recur end))))

(defn seq= [seq1 seq2]
  (cond
    (not= (count seq1) (count seq2)) false
    (and (empty? seq1) (empty? seq2)) true
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [coll a-seq
         idx 0]
    (cond
      (empty? coll) nil
      (pred (first coll)) idx
      :else (recur (rest coll) (inc idx)))))

(defn avg [a-seq]
  (loop [coll a-seq
         acc 0
         idx 0]
    (cond
      (empty? coll) (/ acc idx)
      :else (recur (rest coll) (+ acc (first coll)) (inc idx)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [coll a-seq
         acc #{}]
    (if (empty? coll)
      acc
      (recur (rest coll) (toggle acc (first coll))))))

(defn fast-fibo [n]
  (loop [idx n
         n2 1
         n1 0]
    (cond
      (zero? idx) n1
      :else (recur (dec idx) n1 (+ n2 n1)))))

(defn cut-at-repetition [a-seq]
  (loop [coll a-seq
         acc []]
    (if (or (empty? coll)
            (true? (some #(= (first coll) %1) acc)))
      acc
      (recur (rest coll) (conj acc (first coll))))))


