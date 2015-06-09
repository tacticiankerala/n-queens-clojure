(ns n-queens.core
  (:gen-class))

(def solutions (atom []))

(defn row-conflict?
  [queen board]
  (let [queen-row (queen :row)]
    (some #(= (%1 :row) queen-row) board)))

(defn column-conflict?
  [queen board]
  (let [queen-column (queen :column)]
    (some #(= (%1 :column) queen-column) board)))

(defn diag-conflict?
  [queen board]
  (let [queen-row (queen :row)
        queen-column (queen :column)]
    (some
      #(=
        (Math/abs (- queen-row (%1 :row)))
        (Math/abs (- queen-column (%1 :column))))
      board)))

(defn can-place?
  [queen board]
  (not-any? #(%1 queen board) [row-conflict? column-conflict? diag-conflict?]))

(defn next-queen
  [last-queen]
  {:row (inc (last-queen :row)) :column 0})

(defn move-queen
  [queen]
  (update-in queen [:column] inc))

(defmulti solve
  (fn
    [board queen size]
    (cond
      (= (count board) size) :SolutionFound
      (and (empty? board) (>= (queen :column) size)) :Halt
      (and (not (empty? board)) (>= (queen :column) size)) :Backtrack)))

(defmethod solve :SolutionFound
  [board queen size]
  (swap! solutions conj board)
  #(solve (pop board) (move-queen (peek board)) size))

(defmethod solve :Halt
  [board queen size]
  @solutions)

(defmethod solve :Backtrack
  [board queen size]
  #(solve (pop board) (move-queen (peek board)) size))

(defmethod solve :default
  [board queen size]
  (if (can-place? queen board)
    #(solve (conj board queen) (next-queen queen) size)
    #(solve board (move-queen queen) size)))

(defn -main
  [& args]
  (println (count (trampoline solve [] {:row 0 :column 0} (Integer. (first args))))))
