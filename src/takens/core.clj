(ns takens.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.edn :as edn]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            ))



;(def data-txt (slurp "../clortex/resources/hotgym.edn"))

;(def data-edn (edn/read-string data-txt))

(defn cube [n]
  (vec (repeat n (vec (repeat n (vec (repeat n 0)))))))
(def n-boxes 12)
;(cube 3)

(defn load-csv [f]
  (with-open [in-file (io/reader f)]
	   (vec (doall (csv/read-csv in-file)))))

(def data-txt (take 80000 (load-csv "../clortex/resources/hotgym.csv")))
;(def data-txt (load-csv "short.csv"))

(def xs (->> (drop 3 data-txt)
             (map #(nth % 3))
             (map read-string)
             (map double)
             vec))

(def n-items (count xs))

(def gap 23)
(def dim 3)
(def trail 120)
(def decay 0.9)
(def measure-box-side 25.0)
(def set-camera {:position [651.9498273260655 959.133844302811 1261.2972227577627],
                 :straight [0.08282047462718081 -0.59924598027108 -1.726684444151115],
                 :up [0.13567127248396518 1.002022044075279 0.30484456069617716],
                 :camera [300 300 300]})

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 10)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  (q/text-size 100)
  (q/background 0)

  ; setup function returns initial state. It contains
  ; circle color and position.
  {:framer 30
   :paused? false
   :graphs? false
   :boxes? false
   :cube? false
   :cube (cube n-boxes)
   :gap gap
   :data-step 0
   :color 0
   :angle 0
   :navigation-3d set-camera})

(defn key-press [state event]
  (let [raw (q/raw-key)
        raw (:key event)]
    #_(println (str "Key pressed:" raw
                  " p? " (= raw "p")
                  " type: " (type raw)))
	  ;(reset! (state :key-pressed) raw)
    (case raw
      :0 (do
           (println "restarting")
           (merge state {:data-step 0 :cube (cube n-boxes)}))
      :m (do
           (if (:smooth state) (q/no-smooth) (q/smooth))
           (update state :smooth? not))
      :p (do
           (println (str "pausing? was " (state :paused?)))
           (update state :paused? not))
      :c (do
           (println (str "cube? was " (state :cube?)))
           (update state :cube? not))
      :g (do
           (println (str ":graphs? was " (state :graphs?)))
           (update state :graphs? not))
      :b (do
           (println (str ":boxes? was " (state :boxes?)))
           (update state :boxes? not))
      :- (do
           (println (str ":framer? was " (state :framer)))
           (q/frame-rate (dec (:framer state)))
           (update state :framer dec))
      := (do
           (println (str ":framer? was " (state :framer)))
           (q/frame-rate (inc (:framer state)))
           (update state :framer inc))
      :i (do
           (println (state :navigation-3d))
           state)
      ;\. (update state :stepping? true)
      state
  )))


(defn transform-pos [[x y z]]
  (let [mag 50.0]
    [(+ (/ (q/width) 2) (* x mag))
   (+ (/ (q/width) 2) (* y mag))
   (* z mag)]))


(defn val->box
  [val]
  (let [val-in-measure-box (/ val measure-box-side)]
    (mod (long (* val-in-measure-box n-boxes)) n-boxes)))

(defn box->val
  [box]
  (* measure-box-side (/ box n-boxes)))

(defn update-state [state]
  (if (not (state :paused?))
    (let [gap (state :gap)
        cube (state :cube)
        step (state :data-step)
        now (mod (:data-step state) n-items)
        prev (mod (- now gap) n-items)
        prev2 (mod (- prev gap) n-items)
        coords (mapv (fn [i] (val->box (nth xs i))) [now prev prev2])
        ;_ (println (str "coords " coords))
        old-stat (get-in cube coords)
        ;_ (println (str "old " old-stat))
        ;new-stat (+ (* old-stat decay) (- 1.0 decay))
          new-stat (+ old-stat (/ n-items))
          new-stat (inc old-stat)
        new-cube (assoc-in cube coords new-stat)]
  (merge state {
   :cube new-cube
   :data-step (inc (state :data-step))
   :step-color (mod (state :data-step) 255)
   :color (mod (+ (:color state) 0.7) 255)
   :angle (- (:angle state) 0.1)}))
    state))

(defn dash [from to t]
  (mapv (fn [from to] (+ from (* (- to from) t))) from to))

(defn draw-cube [cube now]
  ;(println "draw-cube")
  (q/text-size 10)
  (doall (for [i (range n-boxes)
        j (range n-boxes)
        k (range n-boxes)
        :let [v (/ (get-in cube [i j k]) (inc now))
              pos (mapv box->val [i j k])]
               :when (> v 0.001)]
    (do ; (println (str "cube: " [i j k] " -> " (transform-pos [i j k]) " = " val))
    (q/with-translation (transform-pos pos)
      (q/text (format "%.3f"
                  (double v)) 10 10)
      (q/with-fill [(+ 127 (* v 127)) 0.3]
        (q/box (* v 1500))))))))

(defn draw-state [state]
  (when (not (state :paused?))
  ;(q/print-camera)
  (q/background 0)
  (q/fill (:color state) 255 255)
  (q/color-mode :rgb)
  (q/stroke 127)
  (let [origin (transform-pos [0 0 0])
        x-axis (transform-pos [100 0 0])
        y-axis (transform-pos [0 100 0])
        z-axis (transform-pos [0 0 100])]
    (q/stroke 255 127 127)
    (q/line origin x-axis)
    (q/stroke 127 255 127)
    (q/line origin y-axis)
    (q/stroke 127 127 255)
    (q/line origin z-axis)
    (q/stroke 127))
    (when (state :cube?)
      (draw-cube (state :cube) (mod (:data-step state) n-items)))
  (q/color-mode :hsb)

  (when (< (* dim (state :gap)) (:data-step state))
    (let [now (mod (:data-step state) n-items)
          gap (state :gap)
          start (max (* dim gap) (- now trail))
          len (- now start)
          rows (vec (take len (drop start xs)))
          ;_ (println (str rows " " start " + " len))
          len (dec (count rows))
          tuples (mapv #(vector (nth rows %) (nth rows (+ gap %)) (nth rows (+ % gap gap)))
                       (range (- len (* dim gap))))
          n-tuples (count tuples)]
  (q/text (format "Frame: %d/%d"
                  now n-items) 50 10)
      (doseq [pos tuples]
    #_(q/with-translation
      (transform-pos pos)
      (q/box 1)))
      (when (> n-tuples 0)
        (let [nose (last tuples)
              p-nose (transform-pos nose)
              xy (transform-pos (assoc nose 2 0))
              xz (transform-pos (assoc nose 1 0))
              yz (transform-pos (assoc nose 0 0))
            x-nose (transform-pos [(nose 0) 0 0])
            y-nose (transform-pos [0 (nose 1) 0])
            z-nose (transform-pos [0 0 (nose 2)])]
        (q/fill 255)
        (q/with-translation x-nose (q/box 5))
        (q/with-translation y-nose (q/box 5))
        (q/with-translation z-nose (q/box 5))
          (when (:boxes? state)
            (q/line x-nose xy)
            (q/line x-nose xz)
            (q/line y-nose yz)
            (q/line y-nose xy)
            (q/line z-nose xz)
            (q/line z-nose yz)

            (q/line p-nose xy)
            (q/line p-nose xz)
            (q/line p-nose yz)
            (q/line p-nose xy)
            (q/line p-nose xz)
            (q/line p-nose yz))

          ))
      (loop [i (max 0 (- n-tuples trail))
             w 1.0]
        (if (< i (- n-tuples 1))
          (let [;_ (println (str "i = " i ", len = " len))
                color (mod (* i 2) 255)
                brightness (mod (+ 127 (* (/ i n-tuples) 127)) 255)
                p-from (nth tuples i)
                p-to (nth tuples (inc i))
                from (transform-pos p-from)
                to (transform-pos p-to)
                from-dash (transform-pos (dash p-from p-to 0.8))
                x-from (transform-pos [(p-from 0) (- i n-tuples) 0])
                x-to (transform-pos [(p-to 0) (- (inc i) n-tuples) 0])
                y-from (transform-pos [(- i n-tuples) (p-from 1) 0])
                y-to (transform-pos [(- (inc i) n-tuples) (p-to 1) 0])
                z-from (transform-pos [(- i n-tuples) 0 (p-from 2)])
                z-to (transform-pos [(- (inc i) n-tuples) 0 (p-to 2)])
                ]
            (q/stroke color 127 brightness)
            (q/stroke-weight w)
            ;(println (str "[" i "/" len "]:" from " -> " to))
            (q/line from-dash to)
            (q/stroke-weight 1)
            (when (:graphs? state)
            (q/line x-from x-to)
            (q/line y-from y-to)
            (q/line z-from z-to))
            (q/stroke-weight 0.5)
            (q/line from to)
            (q/stroke-weight 1)
            (recur (inc i) (* w 1.01)))))))))

(q/defsketch takens
  :title "Hotgym Takens Test"
  :size [800 800]
  ;:size [1920 1080]
  :renderer :p3d
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :key-pressed key-press
  :middleware [m/fun-mode m/navigation-3d]
  ;:camera [300 300 300] :straight [-1 -1 -1]
  ;:middleware [m/fun-mode]
  )
