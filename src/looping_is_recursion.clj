(ns looping-is-recursion)

(defn power [base exp]
  (apply * (repeat exp base)))

(defn last-element [a-seq]
  (loop [acc nil remaining a-seq]
    (if (empty? remaining) acc (recur (first remaining) (rest remaining)))))

(defn seq= [seq1 seq2]
  (loop [equal true remaining [seq1 seq2]]
    (let [[rem1 rem2] remaining]
      (cond
        (false? equal) equal
        (some empty? [rem1 rem2]) (every? empty? [rem1 rem2])
        :else (recur
          (= (first rem1) (first rem2))
          [(rest rem1) (rest rem2)])))))

(defn find-first-index [pred a-seq]
  (loop [index 0 remaining a-seq]
    (cond
      (empty? remaining) nil
      (true? (pred (first remaining))) index
      :else (recur (inc index) (rest remaining)))))

(defn avg [a-seq]
  (loop [acc 0 remaining a-seq counter 0]
    (if (empty? remaining)
      (/ acc (max 1 counter))
      (recur (+ acc (first remaining)) (rest remaining) (inc counter)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [result #{} remaining a-seq]
      (if (empty? remaining)
        result
        (recur (toggle result (first remaining)) (rest remaining))))))

(defn fast-fibo [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    :else (loop [fib-2 0 fib-1 1 cnt n]
      (let [fib-0 (+ fib-2 fib-1)]
        (if (<= cnt 2)
          fib-0
          (recur fib-1 fib-0 (dec cnt)))))))

(defn cut-at-repetition [a-seq]
  (loop [res-set #{} res [] remaining a-seq]
    (if (or (contains? res-set (first remaining)) (empty? remaining))
      res
      (recur (conj res-set (first remaining)) (conj res (first remaining)) (rest remaining)))))
