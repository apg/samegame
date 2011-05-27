(ns samegame.core
  (:use [clojure.set :only (union)]
         [clojure.contrib.math :only (floor)]
         [clojure.contrib.seq-utils :only (indexed)])
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener MouseListener)))

(def colors [(Color. 255 0 0) (Color. 0 255 0) (Color. 0 0 255)])
(def blank-color (Color. 0 0 0))

(def board-width 20)
(def board-height 10)
(def cell-size 20)

(defn create-board 
  "Creates a new board given data, or not given data"
  ([w h]
     (create-board w h 
                   (vec
                    (for [y (range h)]
                      (vec (take w (repeat nil)))))))
  ([w h data]
     {:width w
      :height h
      :data data
      :cell-size 20}))

(defn print-board [b]
  (doseq [a (b :data)]
    (println a)))

(defn color-for [i]
  (if (nil? i) blank-color (colors i)))

(defn random-board [w h]
  (let [c (count colors)]
    (create-board w h 
                  (vec
                   (for [y (range h)]
                     (vec (repeatedly w #(rand-int c))))))))

(defn point-at 
  ([board pt] 
     (point-at board (pt 0) (pt 1)))
  ([board x y]
     (let [v (:data board)]
       ((v y) x))))


;           (for [i [-1 0 1] j [-1 0 1] 
;                 :when (let [nx (+ i x)
;                             ny (+ j y)]
;                         (and (not (= pt [nx ny]))
;                              (and (< nx w) (>= nx 0))
;                              (and (< ny h) (>= ny 0))))]
;             )

(defn expand-points
  "Get list of neighbors given a point that do not exceed w h"
  [pt w h]
  (let [x (pt 0)
        y (pt 1)]
    (apply sorted-set
           (for [[i j] [[0 1] [1 0] [-1 0] [0 -1]] 
                 :when (let [nx (+ i x)
                             ny (+ j y)]
                         (and (not (= pt [nx ny]))
                              (and (< nx w) (>= nx 0))
                              (and (< ny h) (>= ny 0))))]
             [(+ x i) (+ y j)]))))


(defn find-points-to-remove [board pt]
  (let [kind (point-at board pt)
        height (:height board)
        width (:width board)]
    (loop [explored? #{}
           found #{}
           left #{pt}]
      (let [cpt (first left)]
        (cond
          ;; base case -> nothing left to explore
          (or (nil? left) (nil? cpt)) found
          ;; have we already explored this point?
          (explored? cpt) (recur explored? found (rest left))
          ;; is the point we're looking at one to remove?
          (= (point-at board cpt) kind) 
             (let [newexplored (conj explored? cpt)
                   newfound (conj found cpt)
                   ; union expand-points so we get an actual #{set}
                   newleft (union (expand-points cpt width height)
                                      (rest left))]
               (recur newexplored newfound newleft))
          :else (recur (conj explored? cpt)
                       found
                       (rest left)))))))

(defn remove-connected-component
  "Removes the connected items of kind at x, y"
  [board x y]
  (let [height (:height board)
        width (:width board)
        points (find-points-to-remove board [x y])]
    (if (> (count points) 1)
      (create-board width height 
                    (vec (for [j (range height)]
                           (vec (for [i (range width)]
                                  (if (nil? (points [i j]))
                                    (point-at board i j)
                                    nil))))))
      board)))

(defn find-next-non-nil-row
  "Moves up the board in column `col` starting at `row` until non-nil"
  [board col row]
  (loop [r row]
    (if (and (>= r 0) (nil? (point-at board [col r])))
      (recur (dec r))
      (if (< r 0) -1 r))))

(defn find-ups
  "Find positions of next non nil thing given old found positions of it"
  [board old row]
  (vec (for [c (range (:width board))]
         (let [oldc (old c)
               r (cond
                   (nil? oldc) row
                   (<= oldc row) (dec oldc)
                   :else row)]
           (find-next-non-nil-row board c r)))))

(defn merge-down 
  "Drops populated cells into the nil spaces"
  [board]
  (let [h (:height board)
        w (:width board)]
    (println (str "Merge down: " w " x " h))
    (loop [accum []
           row (dec h)
           ups (find-ups board (vec (take w (repeat nil))) row)]
      (println (str "Row: " row " ups: " ups " w: " w))
      (if (>= row 0)
        (recur (cons (vec (for [i (range w)]
                            (do (println i)
                                (if (>= (ups i) 0)
                                  (point-at board i (ups i))
                                  nil))))
                     accum)
               (dec row) 
               (find-ups board ups (dec row)))
        (create-board w h (vec accum))))))

(defn find-all
  [coll x]
  (keep-indexed #(if (= x %2) %1) coll))

(defn merge-left-row [coll idx]
  (let [front (map second
                   (filter #(not (contains? idx (first %)))
                           (take (count coll) (indexed coll))))]
    (concat front (take (- (count coll) (count front))
                        (repeat nil)))))

(defn merge-left 
  "Replaces nils with the thing to the right of it"
  [board]
  (let [coll (:data board)
        idx (apply sorted-set (find-all (last coll) nil))]
    (create-board (:width board)
                  (:height board)
                  (vec (map (comp vec merge-left-row) coll (repeat idx))))))

(def merge-board (comp merge-left merge-down))

(defn remove-at-point 
  "Removes connected component of color"
  [board x y]
  (let [kind (point-at board x y)]
    (println (str "Kind at: " kind " x: " x " y: " y))
    (if (nil? kind) 
      board
      (let [removed (remove-connected-component board x y)]
        (println (str "Connected component removed: " removed))
        (merge-board removed)))))

;;; make these referentially transparent
(defn point-to-screen-rect [pt cs]
  (map #(* cs %)
       [(pt 0) (pt 1) cs cs]))

(defn screen-to-point [pt cs]
  (map #(floor (/ % cs)) pt))

(defn fill-cell [g pt color cs]
  (let [[x y width height] (point-to-screen-rect pt cs)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defn paint-board
  [g board]
  (doseq [y (range (:height board)) x (range (:width board))]
    (fill-cell g [x y] (color-for (point-at board [x y])) (:cell-size board))))

(defn game-panel [frame board]
  (proxy [JPanel ActionListener KeyListener MouseListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint-board g @board))
    (mouseClicked [e]
      (let [pt (vec (screen-to-point [(.getX e) (.getY e)] (:cell-size @board)))
            item (point-at @board pt)]
        (println (str "CLICK AT: " pt))
        (when (not (nil? item))
          (dosync
           (alter board remove-at-point (pt 0) (pt 1)))
          (.repaint this))))
    (getPreferredSize []
      (Dimension. (* (:width @board) (:cell-size @board))
                  (* (:height @board) (:cell-size @board))))
    (actionPerformed [e]
      (.repaint this))
    (keyReleased [e])
    (keyTyped [e])
    (keyPressed [e])
    (mousePressed [e])
    (mouseReleased [e])
    (mouseEntered [e])
    (mouseExited [e])))

(defn game [b]
  (let [board (ref b)
        frame (JFrame. "SameGame")
        panel (game-panel frame board)]
    (doto panel
      (.setFocusable true)
      (.addMouseListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    [board]))

