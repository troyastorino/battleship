(ns battleship.core
  (:require [clojure.string :as str]
            [swank.core.connection]))

(def dim 10)
(def ship-lengths [2 3 3 4 5])

(defrecord Ship [cells])
(defrecord Cell [ship shot])

(defn new-cell []
  (Cell. false false))

(defn create-board []
  (apply vector 
         (map (fn [_] 
                (apply vector (map (fn [_] (ref (new-cell))) 
                                   (range dim)))) 
              (range dim))))

(def boards {:1 (create-board)
             :2 (create-board)})

(def ships {:1 (ref [])
            :2 (ref [])})

(defn opponent [player]
  (case player :1 :2 :2 :1))

(defn place [board [x y]]
  (-> board (nth x) (nth y)))

(defn final-loc [[xi yi] len dir]
  (let [len (dec len)]
    (case dir
      :up [xi (- yi len)]
      :down [xi (+ yi len)]
      :left [(- xi len) yi]
      :right [(+ xi len) yi])))

(defn vec-seq [[xi yi] [xf yf]]
  (let [x1 (min xi xf)
        x2 (max xi xf)
        y1 (min yi yf)
        y2 (max yi yf)]
    (for [x (range x1 (inc x2)) y (range y1 (inc y2))]
     [x y])))

;; dir can be :up :down :left :right
;; assumes ship can sit on board
(defn add-ship! [player [x y] len dir]
  (dosync
   (let [[xf yf] (final-loc [x y] len dir)]
     (doseq [p (map (partial place (player boards)) (vec-seq [x y] [xf yf]))]
       (alter p assoc :ship true))
     (alter (player ships) conj
            (Ship. (map (partial place (player boards)) (vec-seq [x y] [xf yf])))))))

(defn contains-loc? [loc player ship]
  (let [p (place (player boards) loc)]
    (not (empty? (filter #(= p %1) (:cells ship))))))

(defn ship-through-loc [player loc]
  (first (filter #(contains-loc? loc player %1)
                 (deref (player ships)))))

(defn valid-loc? [[x y]]
  (not (or (< x 0) (< y 0) (>= x dim) (>= y dim))))

(defn valid-ship? [player [x y] len dir]
  (let [[xf yf] (final-loc [x y] len dir)]
    (if (and (valid-loc? [x y]) (valid-loc? [xf yf]))
      (not (reduce #(or %1 %2) false
                   (map #(:ship (deref (place (player boards) %1)))
                        (vec-seq [x y] [xf yf]))))
      false)))

(defn valid-shot? [player loc]
  (let [opp (opponent player)]
    (and (valid-loc? loc)
         (not (:shot (deref (place (opp boards) loc)))))))
        
;; returns true for a hit, false for a miss
(defn shoot-cell! [player loc]
  "Player shoots a the opponent's board.  Returns true for a hit and
  false for a miss."
  (:ship 
   (let [opp (opponent player)
         board (opp boards)
         p (place board loc)]
     (dosync
      (alter p assoc :shot true)))))

(defn sunk? [ship]
  (reduce #(and %1 %2) true (map #(:shot (deref %1)) (:cells ship))))

(defn all-sunk? [player]
  (reduce #(and %1 %2) true (map sunk? (deref (player ships)))))

;; create a string representation of the cell
(defn cell-str [cell]
  (str (let [ship? (:ship cell)
             shot? (:shot cell)]
         (if ship?
          (if shot?
            "H"
            "S")
          (if shot?
            "M"
            "-")))
       " "))

(defn opponent-cell-str [cell]
  (str (if (:shot cell)
         (if (:ship cell)
           "H"
           "M")
         "-")
       " "))

(defn print-board [player whose]
  "Prints the board. whose can be :own or :opponent"
  (let [own (= :own whose)
        opp (opponent player)]
    (println (str (if own "Own" "Opponent")
            " board:"))
    (println (str "  " (str/join " " (range 0 dim))))
    (dotimes [y dim]
      (print (str y " "))
      (dotimes [x dim]
        (print ((if own cell-str opponent-cell-str)
                (deref (place ((if own player opp) boards) [x y])))))
      (println))))

(defn print-lines [n]
  (dotimes [_ n]
    (println)))

(defn print-space []
  (print-lines 100))

(defn add-ship-loc! [player len]
  (print-board player :own)
  (println (format (str "Where would you like to place a length "
                        "%s ship? Please use the format \"[x y] "
                        "dir\" to specifiy where you want the ship to "
                        "go (x and y are 0 indexed with [0 0] in the "
                        "top left, and dir can be up, down, left, "
                        "or right). This is a %dx%d grid.")
                   len dim dim))
  (try
    (let [[loc dir-str]
          (map read-string (re-seq #"\[\d+ \d+\]|\w+"
                                   (str/trim (read-line))))
          args [player loc len (keyword dir-str)]]
      (if (apply valid-ship? args)
        (apply add-ship! args)
        (do
          (println "That is an invalid ship location.")
          (print-space)
          (add-ship-loc! player len))))
    (catch Exception e
      (print-space)
      (println "Sorry, I couldn't parse that input.")
      (add-ship-loc! player len))))

(defn switch-player [player-to]
  (print-space)
  (let [from (opponent player-to)]
    (println (format (str "Player %s, look away from the screen. "
                          "Player %s, press enter to take your turn.")
                     (name from) (name player-to))))
  (read-line)
  (print-space))

(defn add-all-ships! [player]
  (switch-player player)
  (println (format (str "Player %s, it is time to place your ships. You "
                        "have ships of the following lengths: %s.")
                 (name player) (str ship-lengths)))
  (doseq [len ship-lengths]
    (print-space)
    (add-ship-loc! player len))
  (println "Thank you. Below is your board setup:")
  (print-board player :own))

(defn take-turn! [player]
  (print-board player :own)
  (print-board player :opponent)
  (println (format (str "Player %s, choose a cell to shoot at. Enter "
                        "your choice in the format \"[x y]\".")
                   (name player)))
  (try
    (let [loc (read-string (read-line))]
      (if (valid-shot? player loc)
        (let [opp (opponent player)]
          (if (shoot-cell! player loc)
            (if (all-sunk? opp)
              (do
                (print-space)
                (print (format "Player %s has won!" player)))
              (do
                (print-space)
                (if (sunk? (ship-through-loc opp loc))
                  (println "Sunk!")
                  (println "Hit!"))
                (take-turn! player)))
            (do
              (switch-player opp)
              (take-turn! opp))))
        (do
          (print-space)
          (println "Invalid shot.")
          (take-turn! player))))
    (catch Exception e
      (println "Sorry, I couldn't parse your input.")
      (take-turn! player))))

(defn -main [& args]
  (print-space)
  (println "Welcome to Battleship! Press enter to start.")
  (read-line)
  (add-all-ships! :1)
  (add-all-ships! :2)
  (switch-player :1)
  (take-turn! :1))