(ns chart-utils.jfreechart
  (:import
    java.awt.Color
    [org.jfree.chart JFreeChart ChartPanel ChartFrame]
    org.jfree.chart.axis.DateAxis
    org.jfree.chart.axis.SegmentedTimeline
    org.jfree.chart.plot.IntervalMarker
    org.jfree.chart.plot.ValueMarker
    org.jfree.chart.plot.XYPlot
    org.jfree.chart.renderer.xy.StandardXYItemRenderer
    org.jfree.chart.util.RelativeDateFormat
    org.jfree.util.UnitType
    java.text.DecimalFormat
    [javax.swing JFrame JLabel JPanel JSlider BoxLayout]
    java.awt.BorderLayout))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti add-domain-marker "Mark a domain value with line and label" (fn [x & _] (class x)))

(defmethod add-domain-marker JFreeChart [chart x label]
  (add-domain-marker (.getPlot chart) x label))

(defmethod add-domain-marker org.jfree.chart.plot.CombinedDomainXYPlot [plot x label]
  (doseq [p (.getSubplots plot)]
    (add-domain-marker p x label)))

(defmethod add-domain-marker XYPlot [plot x label]
  (.addDomainMarker plot 
    (doto (ValueMarker. x) 
      (.setLabel label) 
      (.setPaint Color/RED))))


(defn add-value-marker [chart y label]
  (.addRangeMarker (.getPlot chart) 
    (doto (ValueMarker. y) 
      (.setLabel label) 
      (.setPaint Color/BLACK))
    org.jfree.ui.Layer/BACKGROUND))

(defmulti add-domain-interval-marker (fn [x & _] (class x)))

(defmethod add-domain-interval-marker XYPlot [plot x y label]
  (.addDomainMarker plot 
    (doto (IntervalMarker. x y) 
      (.setLabel label) 
      (.setPaint (Color. 233  194  166)))
    org.jfree.ui.Layer/BACKGROUND))

(defmethod add-domain-interval-marker JFreeChart [chart x y label]
  (add-domain-interval-marker (.getPlot chart) x y label))

(defmethod add-domain-interval-marker ChartPanel [chart x y label]
  (add-domain-interval-marker (.. chart getChart getPlot) x y label)) 

(defmethod add-domain-interval-marker ChartFrame [chart x y label]
  (add-domain-interval-marker (.. chart getChartPanel getChart getPlot) x y label))

(defmethod add-domain-interval-marker org.jfree.chart.plot.CombinedDomainXYPlot [plot x y label]
  (doseq [p (.getSubplots plot)]
    (add-domain-interval-marker p x y label)))


(defn use-relative-time-axis 
  "Replace domain axis by relative date/time axis."
  [plot]
  (let [rdf  (doto (RelativeDateFormat. (long 0)) 
               (.setSecondFormatter (DecimalFormat. "00"))
               (.setShowZeroDays false))
        axis (doto (DateAxis.) (.setDateFormatOverride rdf))]
    (.setDomainAxis plot axis)))

(defn use-time-axis 
  "Use DateAxis. If a start- and end-hour are specified only these hours will be shown."
  ([plot start-hour end-hour]
    (let [one-hour (* 60 60 1000)
          hours (- end-hour start-hour)
          timeline (doto (SegmentedTimeline. one-hour hours (- 24 hours))
                     (.setStartTime (+ (* start-hour one-hour) (SegmentedTimeline/firstMondayAfter1900))))
          axis (doto (DateAxis.) (.setTimeline timeline))]
      (.setDomainAxis plot axis)))
  ([plot]
    (.setDomainAxis plot (DateAxis.))))

(defn create-renderer
  "do not plot a line when at least 3 values are missing (for example during the night)" 
  []
  (doto (StandardXYItemRenderer.)
    (.setPlotDiscontinuous true)
    (.setGapThresholdType UnitType/RELATIVE)
    (.setGapThreshold 3.0)))

#_(defn add-deviation-plot [chart ys ylows yhighs]
  ;; use deviationrenderer, yintervalseries
  )
(defn add-sub-title 
  "Add a subtitle to the chart."
  [chart text]
  (.addSubTitle chart (org.jfree.chart.title.TextTitle. text)))

;;;;;;;;;;;;;;; dynamic charts with multiple sliders in one jframe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn slider 
"
Like incanter.charts/sliders* but returns the JSlider object only.

"
([updater-fn slider-values]
  (slider updater-fn slider-values nil))
([updater-fn slider-values slider-label]
  (let [max-idx (dec (count slider-values))
        label-txt (fn [v] (str (when slider-label (str slider-label " = ")) v))
        label (JLabel. (label-txt (first slider-values)) JLabel/CENTER)
        slider (doto (JSlider. JSlider/HORIZONTAL 0 max-idx 0)
                 (.addChangeListener 
                   (proxy [javax.swing.event.ChangeListener] []
                     (stateChanged [^javax.swing.event.ChangeEvent event]
                                   (let [source (.getSource event)
                                         value (nth slider-values (.getValue source))]
                                     (do
                                       (.setText label (label-txt value))
                                       (updater-fn value)))))))] 
    ;(.setValue slider (/ max-idx 2))
    (doto (JPanel. (BorderLayout.))
      (.add label BorderLayout/NORTH)
      (.add slider BorderLayout/CENTER)))))



(defn sliders*
"sliders*

Like incanter.charts/sliders* but creates one frame that contains all sliders.

"
  ([f [& slider-values]]
     (sliders* f (apply vector slider-values) [nil]))
  ([f [& slider-values] [& slider-labels]] 
    (let [init-values (map first slider-values)
          refs (map ref init-values)
          slider-fns (map #(fn [v] 
                             (do 
                               (dosync (ref-set (nth refs %) v)) 
                               (apply f (map deref refs))))
                          (range (count refs)))
          _ ((first slider-fns) (first init-values))
          panels (if slider-labels 
                   (map slider slider-fns slider-values slider-labels)
                   (map slider slider-fns slider-values))
          panel (JPanel.) 
          frame (JFrame. "Slider Control")
          width 500
          height (* 70 (count slider-fns))]
      (.setLayout panel (BoxLayout. panel BoxLayout/Y_AXIS))
      (dorun (doseq [p panels] (.add panel p)))
      (doto frame
        (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
        (.add panel BorderLayout/CENTER)
;        (.setSize width height)
        (.pack) 
        (.setVisible true))
      frame)))


(defmacro sliders
" Creates one slider control for each of the given sequence bindings. 
  Each slider calls the given expression when manipulated. 

  Like incanter.charts/sliders but creates on frame that contains all sliders.
"
  ([[& slider-bindings] body]
     `(let [slider-fn# (fn ~(apply vector (map symbol (take-nth 2 slider-bindings))) 
			 (do ~body))
	    slider-labels# ~(apply vector (map str (take-nth 2 slider-bindings)))]
	(sliders* slider-fn# ~(apply vector (take-nth 2 (rest slider-bindings))) slider-labels#))))


;;;;;;;;;;;;;;;;;; heat maps with arbitrary resolutions ;;;;;;;;;;;;;;;;;;;;
;; copied from incanter and modified to take arbitrary step widths
;;
(defn grid-apply
" Applies the given function f, that accepts two arguments, to a grid
  defined by rectangle bounded x-min, y-min, x-max, y-max and returns a
  sequence of three sequences representing the cartesian product of x and y
  and z calculated by applying f to the combinations of x and y with step widths
  x-step and y-step."
  ([f x-min x-max y-min y-max x-step y-step]
     (let [x-vals (range x-min x-max (/ (- x-max x-min) x-step)) 
           y-vals (range y-min y-max (/ (- y-max y-min) y-step)) 
           xyz (for [_x x-vals _y y-vals] [_x _y (f _x _y)]) 
           transpose #(list (conj (first %1) (first %2))
                            (conj (second %1) (second %2))
                            (conj (nth %1 2) (nth %2 2)))]
       (reduce transpose [[] [] []] xyz))))

(defn create-3d-dataset [^"[[D" data label]
  (doto (org.jfree.data.xy.DefaultXYZDataset.)
    (.addSeries label data)))

(defn create-paint-scale [colors min-z max-z ^java.awt.Color default-color]
  {:pre [(every? number? (map first colors)) (every? (partial instance? java.awt.Color) (map second colors))]}
  (let [scale (org.jfree.chart.renderer.LookupPaintScale. min-z max-z default-color)]
    (doseq [[i color] colors]
      (.add scale i color))
    scale))

(defn set-paint-scale [chart scale]
  (let [r (.. chart getPlot getRenderer)
        l (.. chart getSubtitles (get 0))] 
    (when (instance? org.jfree.chart.renderer.xy.XYBlockRenderer r) 
      (.setPaintScale r scale))
    (when (instance? org.jfree.chart.title.PaintScaleLegend l) 
      (.setScale l scale))))

(defn set-heat-map-data [chart ^org.jfree.data.xy.DefaultXYZDataset data]
  (let [ds (.. chart getPlot getDataset)
        sk (.getSeriesKey ds 0)]
    (doto ds 
      (.removeSeries sk)
      (.addSeries sk data)))) 

(defn heat-map*
  ([function x-min x-max y-min y-max & options]
     (let [opts (when options (apply assoc {} options))
	   color? (if (false? (:color? opts)) false true)
	   title (or (:title opts) "")
	   x-label (or (:x-label opts) "")
	   y-label (or (:y-label opts) "")
	   z-label (or (:z-label opts) "z scale")
     x-step (or (:x-step opts) 100)
     y-step (or (:y-step opts) 100)
	   theme (or (:theme opts) :default)
	   data (into-array (map double-array 
				 (grid-apply function x-min x-max y-min y-max x-step y-step)))
	   xyz-dataset (create-3d-dataset data "Series 1") 
	   min-z (or (:z-min opts) (reduce min (last data)))
	   max-z (or (:z-max opts) (reduce max (last data)))
	   x-axis (doto (org.jfree.chart.axis.NumberAxis. x-label)
		    (.setStandardTickUnits (org.jfree.chart.axis.NumberAxis/createIntegerTickUnits))
		    (.setLowerMargin 0.0)
		    (.setUpperMargin 0.0)
		    (.setAxisLinePaint java.awt.Color/white)
		    (.setTickMarkPaint java.awt.Color/white))
	   y-axis (doto (org.jfree.chart.axis.NumberAxis. y-label)
		    (.setStandardTickUnits (org.jfree.chart.axis.NumberAxis/createIntegerTickUnits))
		    (.setLowerMargin 0.0)
		    (.setUpperMargin 0.0)
		    (.setAxisLinePaint java.awt.Color/white)
		    (.setTickMarkPaint java.awt.Color/white))
	   colors (or (:colors opts) 
		      (map vector
             (range min-z max-z (/ (- max-z min-z) 16)) ;; FIXME 16 is the number of colors given
             (map (fn [[r g b]] (java.awt.Color. r g b)) 
                  [[0 0 127] [0 0 212] [0 42 255] [0 127 255] [0 127 255] 
                   [0 226 255] [42 255 212] [56 255 198] [255 212 0] [255 198 0]
                   [255 169 0] [255 112 0] [255 56 0] [255 14 0] [255 42 0]
                   [226 0 0]])))
	   scale (if color?
            (create-paint-scale colors min-z max-z java.awt.Color/cyan)
            (org.jfree.chart.renderer.GrayPaintScale. min-z max-z))
	   scale-axis (org.jfree.chart.axis.NumberAxis. z-label)
	   legend (org.jfree.chart.title.PaintScaleLegend. scale scale-axis)
	   renderer (org.jfree.chart.renderer.xy.XYBlockRenderer.)
	   
	   plot (org.jfree.chart.plot.XYPlot. xyz-dataset x-axis y-axis renderer)
	   chart (org.jfree.chart.JFreeChart. plot)]
       (do
	(.setPaintScale renderer scale)
  (.setBlockWidth renderer (/ (- x-max x-min) x-step))
  (.setBlockHeight renderer (/ (- y-max y-min) y-step))
	(.setBackgroundPaint plot java.awt.Color/lightGray)
	(.setDomainGridlinesVisible plot false)
	(.setRangeGridlinePaint plot java.awt.Color/white)
	(.setAxisOffset plot (org.jfree.ui.RectangleInsets. 5 5 5 5))
	(.setOutlinePaint plot java.awt.Color/blue)
	(.removeLegend chart)
	(.setSubdivisionCount legend 20)
	(.setAxisLocation legend org.jfree.chart.axis.AxisLocation/BOTTOM_OR_LEFT)
	(.setAxisOffset legend 5.0)
	(.setMargin legend (org.jfree.ui.RectangleInsets. 5 5 5 5))
	(.setFrame legend (org.jfree.chart.block.BlockBorder. java.awt.Color/red))
	(.setPadding legend (org.jfree.ui.RectangleInsets. 10 10 10 10))
	(.setStripWidth legend 10)
	(.setPosition legend org.jfree.ui.RectangleEdge/RIGHT)
	(.setTitle chart title)
	(.addSubtitle chart legend)
  (.. chart getPlot getDomainAxis (setRange x-min x-max))
  (.setRange y-axis y-min y-max)
	(org.jfree.chart.ChartUtilities/applyCurrentTheme chart)
	#_(set-theme chart theme))
       chart)))


(defmacro heat-map
  "see incanter.charts/heat-map for examples
   Takes optional parameters :x-step and :y-step for x and y resolutions and :z-min :z-max for min./max. values of z."
  ([function x-min x-max y-min y-max & options]
    `(let [opts# ~(when options (apply assoc {} options))
           x-lab# (or (:x-label opts#) (format "%s < x < %s" '~x-min '~x-max))
	   y-lab# (or (:y-label opts#) (format "%s < y < %s" '~y-min '~y-max))
           z-lab# (or (:z-label opts#) (str '~function))
           args# (concat [~function ~x-min ~x-max ~y-min ~y-max] 
			 (apply concat (seq (apply assoc opts# 
						   [:z-label z-lab#
						    :x-label x-lab# 
						    :y-label y-lab#]))))]
       (apply heat-map* args#))))

(defn bin-fn 
  "Create a function that returns the interval that a value x falls in.
Example:  
    ((bin-fn 0 10 2) 5.5) 
    => [4 6]"
  [min max step] 
  (fn [v] (let [bin-start (int (/ (- v min) step))] 
            [(+ min (* bin-start step)) (+ min (* (inc bin-start) step))])))

(defn bounded-bin-fn 
  "Create a function that returns the interval that a value x falls in. If the value is out of bounds,
returns the nearest valid bin.
Example:  
    ((bounded-bin-fn 0 10 2) 5.5) 
    => [4 6]
    ((bounded-bin-fn 0 10 2) -5) 
    => [0 2]
    ((bounded-bin-fn 0 10 2) 100) 
    => [8 10]
"
  [minimum maximum step] 
  {:pre [(every? number? [minimum maximum step]) 
         (< minimum maximum)
         (<= step (- maximum minimum))]} 
  (let [supremum-bin (dec (int (/ (- maximum minimum) step)))] 
    (fn [v] (let [bin-start (int (/ (- v minimum) step))
                  bin-start (-> bin-start (max 0) (min supremum-bin))] 
              [(+ minimum (* bin-start step)) (+ minimum (* (inc bin-start) step))]))))

(defn- map-values 
  "Change all values or all keys and values by applying a function to each of them."
  ([vf m] (map-values identity vf m))
  ([kf vf m]
  (into {} (for [[k v] m] [(kf k) (vf v)]))))

(defn ->bins [x-min x-bin-width y-min y-bin-width bin-w bin-p]
  (fn [xs ys] 
    (->> (map vector xs ys)
      (sort-by first)
      (partition-by #(int (/ (- (first %) x-min) x-bin-width)))
      (reduce #(assoc % (bin-w (ffirst %2)) %2) {}) 
      (map-values 
        (fn [bin] (->> bin 
                    (sort-by second)
                    (partition-by #(int (/ (- (second %) y-min) y-bin-width)))
                    (reduce #(assoc % (bin-p (second (first %2))) %2) {})
                    (map-values count)))))))

(defn heat-map-function [xs ys min-x max-x min-y max-y x-steps y-steps]
  (let [x-bin-width (double (/ (- max-x min-x) x-steps))
        y-bin-width (double (/ (- max-y min-y) y-steps))
        bin-x (bin-fn min-x max-x x-bin-width)
        bin-y (bin-fn min-y max-y y-bin-width)
        bins ((->bins min-x x-bin-width min-y y-bin-width bin-x bin-y) xs ys)] (def bins bins)
    (fn [x y]
      (if-let [v (get-in bins [(bin-x x) (bin-y y)])] v 0))))

(defn heat-map-data 
  "Create the data needed for calling `chart-utils.jfreechart/set-heatmap-data.
For details please refer to `chart-utils.jfreechart/heat-map`"
  [xs ys x-min x-max y-min y-max x-step y-step]
  (let [f (heat-map-function xs ys x-min x-max y-min y-max x-step y-step)]
    (into-array 
      (map double-array 
           (grid-apply f x-min x-max y-min y-max x-step y-step)))))
(comment 
  (doto (heat-map (heat-map-function (range 100) (take 100 (cycle (range 10))) 0 10 0 10 100 100) 
          0 10 0 10 :x-step 100 :y-step 100)
    incanter.core/view))

(defn combined-domain-plot 
  "Combine several XYPlots into one plot with a common domain axis (all charts in one column)."
  [& plots]
  (let [axis (if plots (.getDomainAxis (first plots)) (org.jfree.chart.axis.NumberAxis. ""))
        combined-plot (org.jfree.chart.plot.CombinedDomainXYPlot. axis)]
    (doseq [p plots] (.add combined-plot p))
    combined-plot))

(defn combined-range-plot 
  "Combine several XYPlots into one plot with a common range axis (all charts in one row)."
  [& plots]
  (let [axis (org.jfree.chart.axis.NumberAxis. "")
        combined-plot (org.jfree.chart.plot.CombinedRangeXYPlot. axis)]
    (doseq [p plots] (.add combined-plot p))
    combined-plot))

(defn set-y-ranges 
  "Set y range for all available y-axes."
  [chart lower upper]
  (let [plot (.. chart getPlot)] 
    (dotimes [i (.getRangeAxisCount plot)]
      (.. plot getRangeAxis (setRange lower upper))))
  chart)

(defn set-discontinuous [chart]
  (let [new-renderer (fn [old-renderer] 
                       (let [p (.getSeriesPaint old-renderer 0)]
                         (doto (org.jfree.chart.renderer.xy.StandardXYItemRenderer.) 
                           (.setSeriesPaint 0 p)
                           (.setPlotDiscontinuous true))))
        p (.getPlot chart)] 
    (dotimes [n (.getRendererCount p)]
      (.setRenderer p n (new-renderer (.getRenderer p n))))))

(defn set-step-renderer 
  "Set an instance of `org.jfree.chart.renderer.xy.XYStepRenderer` for data series `n`."
  [chart n]
  (.. chart getPlot (setRenderer n (org.jfree.chart.renderer.xy.XYStepRenderer.))))

(defn map-to-axis 
  "Map dataseries to separate y axis. Reuses fonts, colors, paints from the first axis."
  [chart series-idx axis-idx]
  (let [p (.getPlot chart)
        a0 (.. p (getRangeAxis 0)) 
        axis-count (.getRangeAxisCount p)
        axis (or (.getRangeAxis p axis-idx) (doto (org.jfree.chart.axis.NumberAxis.)
                                              (.setLabelFont (.getLabelFont a0)) 
                                              (.setTickLabelFont (.getTickLabelFont a0)) 
                                              (.setLabelPaint (.getLabelPaint a0))
                                              (.setAxisLinePaint (.getAxisLinePaint a0))
                                              (.setTickLabelPaint (.getTickLabelPaint a0))))]
    
    (when (not= axis (.getRangeAxis p axis-idx)) 
      (.setRangeAxis p axis-idx axis))
    (.mapDatasetToRangeAxis p series-idx axis-idx)))

(defn- get-series
  "get-series"
  ([chart]
    (-> chart .getPlot .getDataset .getSeries))
  ([chart series-idx]
    (first (seq (-> chart
                  .getPlot
                  (.getDataset series-idx)
                  .getSeries)))))

(defn perf-set-data 
  "Replace the data of a series without firing events for every single data point
by avoiding `addOrUpdate`."
  [chart data series-idx]
    (let [series (get-series chart series-idx)]
       (.clear series)
       (cond
         (= 2 (count (first data)))
           (doseq [row data]
             (.add series (first row) (second row) false))
         (= 2 (count data))
           (dorun (map #(.add series %1 %2 false) (first data) (second data)))
         :else
           (throw (Exception. "Data has wrong number of dimensions")))
       (.fireSeriesChanged series) 
       chart))