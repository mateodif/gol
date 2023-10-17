(ns main
  (:require [quil.core :as q]))

(def col 100)

(defn take-repeatedly [f]
  (vec (take col (repeatedly f))))

(defn random-row []
  (take-repeatedly #(rand-int 2)))

(def grid
  (atom (take-repeatedly random-row)))

(defn neighbour-range [x]
  (range (max (dec x) 0) (min (+ x 2) col)))

(defn neighbours-coords [x y]
  (let [cells-in-range (for [y (neighbour-range y)
                             x (neighbour-range x)]
                         [x y])]
    (remove #{[x y]} cells-in-range)))

(defn neighbours [grid [x y]]
  (let [neighbour-at (fn [grid [x y]] (nth (nth grid y) x))]
    (map #(neighbour-at grid %) (neighbours-coords x y))))

(defn live-neighbours [grid [x y]]
  (count (filter pos? (neighbours grid [x y]))))

(defn underpopulated? [grid [x y]]
  (< (live-neighbours grid [x y]) 2))

(defn overpopulated? [grid [x y]]
  (> (live-neighbours grid [x y]) 3))

(defn dies? [grid [x y]]
  (or (underpopulated? grid [x y])
      (overpopulated? grid [x y])))

(defn lives? [grid [x y]]
  (= (live-neighbours grid [x y]) 3))

(defn next-generation [grid]
  (for [[x row] (map-indexed vector grid)]
    (for [[y cell] (map-indexed vector row)]
      (cond
        (and (zero? cell) (lives? grid [x y])) 1
        (and (pos? cell) (dies? grid [x y])) 0
        :else cell))))

(defn setup []
  (q/frame-rate 1)
  (q/background 255))

(defn draw []
  (swap! grid next-generation)

  (doseq [[x row] (map-indexed vector @grid)]
    (doseq [[y cell] (map-indexed vector row)]
      (if (pos? cell)
        (q/fill 220 200 255)
        (q/fill 255))
      (q/rect (* x 10) (* y 10) 10 10))))

(q/defsketch gol
  :title "GoL"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [1000 1000])
