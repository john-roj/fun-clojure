(ns sudoku
  (:require [clojure.set :as set]))

(def sample-board
  [[3 0 0 2 0 0 0 0 0]
   [0 0 0 1 0 7 0 0 0]
   [7 0 6 0 3 0 5 0 0]
   [0 7 0 0 0 9 0 8 0]
   [9 0 0 0 2 0 0 0 4]
   [0 1 0 8 0 0 0 5 0]
   [0 0 9 0 4 0 3 0 1]
   [0 0 0 7 0 2 0 0 0]
   [0 0 0 0 0 8 0 0 6]])

(defn- val->
  "Returns the value in the sudoku board at [row column]"
  [sudoku [row column]]
  (-> sudoku
      (get row)
      (get column)))

(defn set-val->
  "Updates the value in the sudoku bard at [row column] with a new-value"
  [sudoku [row column] new-value]
  (assoc-in sudoku
            [row column]
            new-value))

(defn quadrant->
  "Returns all the values in the sudoku board for the quadrant in [row column]
  a quadrant is the 3 x 3 sudoku region.

  e.g: sudoku [[8 1 2 | 7 5 3 | 6 4 9]
               [9 4 3 | 6 8 2 | 1 7 5]
               [6 7 5 | 4 9 1 | 2 8 3]
               [---------------------]
               [1 5 4 | 2 3 7 | 8 9 6]
               [3 6 9 | 8 4 5 | 7 2 1]
               [2 8 7 | 1 6 9 | 5 3 4]
               [---------------------]
               [5 2 1 | 9 7 4 | 3 6 8]
               [4 3 8 | 5 2 6 | 9 1 7]
               [7 9 6 | 3 1 8 | 4 5 2]]

  [r c] [0 0] -> '(8 1 2 9 4 3 6 7 5)
  [r c] [8 8] -> '(3 6 8 9 1 7 4 5 2)"
  [sudoku [row column]]
  (let [size (/ (count sudoku) 3)]
    (for [r (take size (iterate inc (* (int (/ row size)) size)))
          c (take size (iterate inc (* (int (/ column size)) size)))]
      (val-> sudoku [r c]))))

(defn- quadrants
  "Returns a sequence with the values of all quadrants of a sudoku board"
  [sudoku]
  (let [q-size (/ (count sudoku) 3)
        coords (for [r (range 0 (count sudoku) q-size)
                     c (range 0 (count sudoku) q-size)] [r c])]
      (map #(quadrant-> sudoku %) coords)))

(defn- column->
  "Returns the values in the sudoku bard at a given column"
  [sudoku column]
  (map #(val-> sudoku [% column]) (range 0 (count sudoku))))

(defn- columns
  "Returns a sequence with the values of all columns of a sudoku board"
  [sudoku]
  (let [idxs (range 0 (count sudoku))]
    (map #(column-> sudoku %) idxs)))

(defn- row->
  "Returns the values in the sudoku bard at a given row"
  [sudoku row]
  (map #(val-> sudoku [row %]) (range 0 (count sudoku))))

(defn- rows
  "Returns a sequence with the values of all rows of a sudoku board"
  [sudoku]
  (let [idxs (range 0 (count sudoku))]
    (map #(row-> sudoku %) idxs)))

(defn- has-all-numbers?
  [coll]
  (let [all-numbers (range 1 (inc (count coll)))]
    (->> (set coll)
         (set/difference (set all-numbers))
         (empty?))))

(defn- solved?
  "Verifies if a sudoku board is solved.
  - All rows have no duplicates
  - All columns have no duplicates
  - All quadrants have no duplicates
  returns the sudoku board or nil if it is not solved"
  [sudoku]
  (when (and (every? has-all-numbers? (rows sudoku))
             (every? has-all-numbers? (columns sudoku))
             (every? has-all-numbers? (quadrants sudoku)))
    sudoku))


(defn- sudoku->candidates
  "For all 0 in the sudoku returns the possible values a [row column] can be assigned to"
  [sudoku]
  (for [row   (range 0 (count sudoku))
        col   (range 0 (count sudoku))
        :let  [value (val-> sudoku [row col])]
        :when (= value 0)]
    [[row col] (set/difference (set (range 1 (inc (count sudoku))))
                               (set (concat (row-> sudoku row)
                                            (column-> sudoku col)
                                            (quadrant-> sudoku [row col]))))]))

(defn- expand->
  "Generates a sequence of possible sudoku boards by making a choice in one of the [row column]
  with a zero.
  It use the candidate with least amount of choices. if a given [row col] has no choices means
  that the sudoku is in an inconsistent state so no need look further that path."
  [sudokus]
  (let [current             (first sudokus)
        total-choices       (comp count second)
        candidates          (sort-by total-choices (sudoku->candidates current))
        [[row col] choices] (first candidates)]
    (lazy-cat
     (map (partial set-val-> current [row col]) choices)
     (rest sudokus))))

(defn solve
  [sudoku]
  (some (comp solved? first) (iterate expand-> [sudoku])))

(defn str->sudoku
  "parses a string into a sudoku board"
  [string]
  (let [as-int (fn[c] (-> c (int) (- (int \0))))]
    (->> string
         (map as-int)
         (partition 9)
         (map vec)
         (vec))))

(comment
  (solve sample-board)
  (solve (str->sudoku "005002000200000007010400300060010409800000001103070050004005080900000003000600900"))

  ,)
