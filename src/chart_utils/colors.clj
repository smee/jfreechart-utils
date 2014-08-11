(ns chart-utils.colors
  (:import java.awt.Color))

;;;;;;;;;; color scale functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn magnification 
  "Magnification. See http://www.caida.org/~youngh/colorscales/nonlinear.html.
x is in [0,1], alpha should vary between -1 and 1, and a good range for beta is [0.5, 5.5]."
  [x alpha beta]
  (/ 1 (Math/pow (Math/cosh (* beta (+ alpha (dec (* 2 x))))) 2)))

(defn transformation [x alpha beta]
  (/ (Math/tanh (* beta (+ alpha (dec (* 2 x))))) beta))

(comment
  (use '[incanter core charts]) 
  (let [x (range 0 1.05 0.025)
      chart (doto (incanter.charts/xy-plot x (repeat 0)) 
              (incanter.charts/add-lines x (repeat 0))
              incanter.core/view)] 
  (sliders [alpha (range -1 1.025 0.025)
                beta (range 0.5 6 0.1)]
               (let [t-min (transformation (apply min x) alpha beta)
                     t-max (transformation (apply max x) alpha beta)] 
                 (println [t-min t-max])
                 (incanter.core/set-data chart [x (map #(magnification % alpha beta) x)] 0)
                 (incanter.core/set-data chart [x (map #(/ (- (transformation % alpha beta) t-min) (- t-max t-min)) x)] 1))))
  )

;;;;;;;;;;;;;; number interpolators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(defprotocol Interpolator
;  (interpolate [this domain-value])
;  (uninterpolate [this interpolated-value]))
;
;(defrecord ^:private LinearInterpolator [x0 x1 x-range y0 y-range clamp?]
;  Interpolator
;  (interpolate [_ v]
;     (let [v (if clamp?
;               (cond (> v x1) x1
;                     (< v x0) x0
;                     :else v)
;               v)
;           v-percent (/ (- v x0) x-range)]
;       (+ y0 (* v-percent y-range))))
;  (uninterpolate [_ v]
;    (let [v-percent (/ (- v y0) y-range)]
;      (+ x0 (* v-percent x-range))))
;  clojure.lang.IFn
;  (invoke [this v]
;    (interpolate this v)))
;
;
;(defrecord ^:private ComposedInterpolator [ranges interpolators clamp?]
;  Interpolator
;  (interpolate [_ v]
;     (let [v (if clamp? 
;               (max (ffirst ranges) (min v (last (last ranges))))
;               v)
;           idx (first (keep-indexed (fn [idx [x0 x1]] (when (<= x0 v x1) idx)) ranges))]
;       (println v idx ranges) 
;       (when idx
;         (interpolate (nth interpolators idx) v))))
;  (uninterpolate [_ v]
;    (throw (ex-info "not implemented yet")))
;  clojure.lang.IFn
;  (invoke [this v]
;    (interpolate this v)))
;
;(defn linear-interpolator [xs ys & {:keys [clamp?] :or {clamp? true}}]
;  (let [interpolators (for [[[x0 x1] [y0 y1]] (map list (partition 2 1 xs) (partition 2 1 ys))
;                            :let [x-range (double (- x1 x0))
;                             y-range (double (- y1 y0))]]  
;                        [[x0 x1] (->LinearInterpolator (double x0) x1 x-range y0 y-range clamp?)])]
;    (->ComposedInterpolator (map first interpolators) (map second interpolators) clamp?)))
; 

(defn create-color-scale [& val-col-pairs]
  (let [indices (map first val-col-pairs)
        min (apply min indices)
        max (apply max indices)
        val-col-pairs (partition 2 1 (sort-by first val-col-pairs))] 
    (fn [x]
      (if-let [pair (some #(when (<= (ffirst %) x (first (second %))) %) val-col-pairs)] 
        (let [[[from [r1 g1 b1]] [to [r2 g2 b2]]] pair
              scaled-x (/ (- x from) (- to from))
              dr (- r2 r1)
              dg (- g2 g1)
              db (- b2 b1)]
          [(int (+ r1 (* scaled-x dr)))
           (int (+ g1 (* scaled-x dg)))
           (int (+ b1 (* scaled-x db)))])
        (-> val-col-pairs last second second)))))

(defn tripel2color [[r g b]]
  (Color. r g b))

(defn fixed-color-scale [color-scale values]
  (map #(vector % (tripel2color (color-scale %))) values))

(comment 
  (let [f (create-color-scale [0 [0 0 0]] [10 [255 0 0]] [20 [0 255 255]])]
    (map (comp tripel2color f) (range 21))
    (fixed-color-scale f (range 21))))

;;;;;;;;;;;;;;;; experiments with colors and color scales ;;;;;;;;;;;;;;;;;;
(defn color-distance 
  "Natural color distance metric, see http:;www.compuphase.com/cmetric.htm"
  [^Color c1 ^Color c2]
  (let [rmean (* 0.5 (+ (.getRed c1) (.getRed c2)))
        r (- (.getRed c1) (.getRed c2))
        g (- (.getGreen c1) (.getGreen c2))
        b (- (.getBlue c1) (.getBlue c2))
        weight-r (+ 2 (/ rmean 256))
        weight-g 4.0
        weight-b (+ 2 (/ (- 255 rmean) 256))]
    (Math/sqrt (+ (* weight-r r r) (* weight-g g g) (* weight-b b b)))))
