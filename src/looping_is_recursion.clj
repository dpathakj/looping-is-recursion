(ns looping-is-recursion)

(defn power [base exp]
  (cond (zero? exp) 1
        (even? exp) (recur (* base base) (quot exp 2))
        :else (* base (power base (dec exp)))))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (every? empty? [seq1 seq2]) true
        (or (empty? seq1) (empty? seq2)) false
        :else (if (= (first seq1) (first seq2))
                (recur (rest seq1) (rest seq2))
                false)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         sequence a-seq]
    (cond (empty? sequence) nil
          (pred (first sequence)) index
          :else (recur (inc index) (rest sequence)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         sequence a-seq]
    (if (empty? sequence) (/ sum count)
        (recur (+ sum (first sequence))
               (inc count)
               (rest sequence)))))

(defn toggle [a-set elem] ;; from earlier chapter
  ((if (contains? a-set elem) disj conj) a-set elem))

(defn parity [a-seq]
  (loop [els #{}
         sequence a-seq]
    (if (empty? sequence) els
        (recur (toggle els (first sequence))
               (rest sequence)))))

(defn fast-fibo [n]
  (loop [first 1
         second 0
         i 0]
    (if (= i n) second
        (recur second
               (+ first second)
               (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         seen #{}
         sequence a-seq]
    (let [element (first sequence)]
      (if (or (empty? sequence) (seen element))
        acc
        (recur (conj acc element)
               (conj seen element)
               (rest sequence))))))

