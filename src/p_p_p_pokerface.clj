(ns p-p-p-pokerface)

(def replacements {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [first-char (first card)]
    (if (Character/isDigit first-char)
      (Integer/valueOf (str first-char))
      (get replacements first-char))))

(defn suit [card]
  (str (second card)))

;; HELPERS
(defn ranks-of-hand [hand]
  (map rank hand))

(defn frequencies-of? [ranks-or-suits hand]
  (set (vals (frequencies (ranks-or-suits hand)))))

(defn contains-?-of-a-kind [hand amount]
  (contains? (frequencies-of? ranks-of-hand hand) amount))

(defn suits-of-hand [hand]
  (map suit hand))

(defn contains-?-of-a-suit [hand amount]
  (contains? (frequencies-of? suits-of-hand hand) amount))

(defn sorted-ranks [hand]
  (let [ranks (ranks-of-hand hand)
        lowest-rank (apply min ranks)]
    (if (and (contains? (set ranks) 14) (< lowest-rank 10))
      (vec (sort (replace {14 1} ranks)))
      (vec (sort ranks)))))

;; EXERCISES
(defn pair? [hand]
  (contains-?-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (contains-?-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (contains-?-of-a-kind hand 4))

(defn flush? [hand]
  (contains-?-of-a-suit hand 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [set-of-ranks (set (ranks-of-hand hand))]
    (and 
      (not (three-of-a-kind? hand))
      (or (= 3 (count set-of-ranks))
          (four-of-a-kind? hand)))))

(defn straight? [hand]
  (let [sorted (sorted-ranks hand)
        lowest-rank (first sorted)]
    (= sorted (range lowest-rank (+ lowest-rank 5)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def checkers #{[high-card? 0] [pair? 1]
                [two-pairs? 2] [three-of-a-kind? 3]
                [straight? 4] [flush? 5]
                [full-house? 6] [four-of-a-kind? 7]
                [straight-flush? 8]})

(defn value [hand]
  (let [checks (filter #((first %) hand) checkers)
        values (map #(second %) checks)]
    (apply max values)))



