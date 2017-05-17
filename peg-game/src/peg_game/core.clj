(ns peg-game.core
  (:gen-class)
  (:require [clojure.set :as set]))

(declare successful-move prompt-move game-over prompt-rows)

;;
;; Creating the Board
;;

(defn tri*
  "Generate a lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))


(defn triangular?
  "Determines if the given number is triangular, e.g. 1, 3, 6, 10, 15, ..."
  [n]
  (= n (last (take-while #(>= n %) tri))))


; for the 'peg-game', each row must end in a triangular number
(defn row-tri
  "Determines the triangular number appearing at the end of row n"
  [n]
  (last (take n tri)))


(defn row-num
  "Returns the row number that containes the given position number: e.g. pos 5 -> row 3"
  [pos]
  (inc (count (take-while #(> pos %) tri))))


(defn connect
  "Establish connections - i.e. valid moves - between positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))


(defn connect-right
  "Determine rightward connections - if any - for the given position"
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  "Determine left-downward connections - if any - for the given position"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  "Determine right-downward connections - if any - for the given position"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))


(defn add-pos
  "Adds a peg, with all of its connections, in the specified position"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))


(defn new-board
  "Generate a new (pegged) board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))


;;
;; Moving Pegs
;;

(defn pegged?
  "Determines if the given position currently holds a peg"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Remove the peg in a given position"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Place a peg in the given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Remove peg from p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))


(defn valid-moves
  "Return a map of all valid moves for the given position like:
  {key, value} := {destination-position, jumped-position}"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))


(defn valid-move?
  "If move from p1 to p2 is valid, return the jumped position; else, return nil"
  [board p1 p2]
  (get (valid-moves board p1) p2))


(defn make-move
  "If it is a valid move, move peg from p1 to p2 and remove the jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))


(defn can-move?
  "Determines whether any valid moves remain for the given board"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))


;;
;; Render/Display the Board
;;

; define some constants used for the display
(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

; definitions for producing colored/styled output
(def ansi-styles
  {:red   "[31m"
   :green "[32m"
   :blue  "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string to apply the given ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to the given text"
  [text color]
  (str (ansi color) text (ansi :reset)))


; the 'render-pos', 'row-positions', 'row-padding', and 'render-row' functions
; assist in generating string representations of the board

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "0" :blue)
         (colorize "-" :red))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "Generate a string of spaces of the necessary length to pad the given row so
  that it is centered"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  "Generate a string representation of a row in the given board"
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))


(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))


;;
;; Interacting With the Player
;;

(defn letter->pos
  "Convert letter to its corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))


(defn get-input
  "Wait for user to hit <Enter>, then extract and clean any input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))


(defn characters-as-strings
  "Converts a given string to a collection with each character as its own
  element"
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn user-entered-invalid-move
  "Handle an invalid move entered by user"
  [board]
  (println "\nThat was an invalid move!\n")
  (prompt-move board))

(defn user-entered-valid-move
  "Handle required logic following a valid move made by the user"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from 'where' to 'where'? (Enter as two letters):")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn game-over
  "Display a 'game over' message to the user and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Would you like to play again? (Y/n) ")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))


(defn prompt-empty-peg
  "Prompt the user to select which position will be empty at the beginning of
  the game"
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? (default: e):")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  "Prompt the user to specify how many rows he/she would like for their board"
  []
  (println "How many rows? (default: 5):")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))


(defn -main
  [& args]
  (println "Get ready to play the PEG GAME!")
  (prompt-rows))
