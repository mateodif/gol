(ns main
  (:require [quil.core :as q]))

(def col 100)

(defn random-row []
  (vec (take col (repeatedly (fn [] (rand-int 2))))))

;;   0. [ 0 1 0 1 1 ]
;;   1. [ 1 1 0 0 1 ]
;;   2. [ 0 1 0 1 1 ]
;;   3. [ 1 0 0 1 1 ]
;;   4. [ 1 1 0 0 1 ]
(def grid
  (atom (vec (take col (repeatedly random-row)))))

(defn neighbours-coords [x y]
  (remove #{[x y]}
          (for [y (range (max (dec y) 0) (min (+ y 2) col))
                x (range (max (dec x) 0) (min (+ x 2) col))]
            [x y])))

(defn neighbours [grid [x y]]
  (let [neighbour-at (fn [grid [x y]] (nth (nth grid y) x))]
    (map #(neighbour-at grid %) (neighbours-coords x y))))

;; 1 1 0
;; 0   0
;; 0 1 1
(neighbours grid [2 2])
(neighbours-coords 2 2)

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
  (q/frame-rate 20)
  (q/background 255))

(defn draw []
  (q/fill 220 200 255)
  (swap! grid next-generation)

  (doseq [[x row] (map-indexed vector @grid)]
    (doseq [[y cell] (map-indexed vector row)]
      (when (pos? cell)
        (q/rect (* x 4) (* y 4) 4 4)))))

(q/defsketch gol
  :title "GoL"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [400 400])
