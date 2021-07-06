(ns chart-utils.jfreechart
  (:require [binning :refer [bin-fn]])
  (:import
   [java.io File]
   [javax.imageio ImageIO]
   [java.awt Color]
    [org.jfree.chart JFreeChart ChartPanel ChartFrame ChartUtilities ChartTheme StandardChartTheme]
    [org.jfree.data.general Series]
    [org.jfree.data.xy 
     YIntervalSeries YIntervalSeriesCollection YIntervalDataItem
     XYSeries XYSeriesCollection AbstractIntervalXYDataset]
    [org.jfree.chart.axis DateAxis LogAxis NumberAxis SegmentedTimeline]
    org.jfree.chart.plot.CombinedDomainXYPlot
    org.jfree.chart.plot.IntervalMarker
    org.jfree.chart.plot.ValueMarker
    org.jfree.chart.plot.XYPlot
    [org.jfree.chart.renderer.xy StandardXYItemRenderer DeviationRenderer XYLineAndShapeRenderer XYStepRenderer]
    org.jfree.chart.util.RelativeDateFormat
    org.jfree.chart.plot.DefaultDrawingSupplier
    [org.jfree.data.statistics HistogramDataset]
    [org.jfree.data.category DefaultCategoryDataset]
    org.jfree.util.UnitType
    java.text.DecimalFormat
    [javax.swing JFrame JLabel JPanel JSlider BoxLayout]
    java.awt.BorderLayout))

(load "from_incanter")
(defmulti get-plots "Extract all instances of org.jfree.chart.XYPlot" class)

(defmethod get-plots XYPlot [^XYPlot c]
  [c])
(defmethod get-plots ChartPanel [^ChartPanel c]
  (get-plots (.getChart c)))
(defmethod get-plots ChartFrame [^ChartFrame cf]
  (get-plots (.getChartPanel cf)))
(defmethod get-plots JFreeChart [^JFreeChart c]
  (get-plots (.getPlot c)))
(defmethod get-plots CombinedDomainXYPlot [^CombinedDomainXYPlot c]
  (mapcat get-plots (.getSubplots c)))

;;;;;;;;;;;;;;;;;; Add and remove markers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn add-domain-marker [chart x label]
  (let [marker (doto (ValueMarker. x) 
                 (.setLabel label) 
                 (.setPaint Color/RED))]
    (doseq [plot (get-plots chart)] 
      (.addDomainMarker plot marker))    
    marker))

(defn remove-domain-marker [chart marker]
  (doseq [plot (get-plots chart)]
    (.removeDomainMarker plot marker)) 
  chart)

(defn add-value-marker [chart y label & [idx]]
  (let [idx (or idx 0)
        layer org.jfree.ui.Layer/BACKGROUND
        marker (doto (ValueMarker. y) 
                 (.setLabel label) 
                 (.setPaint Color/BLACK))] 
    (doseq [plot (get-plots chart)] 
      (.addRangeMarker plot idx marker layer))
    marker))

(defn add-domain-interval-marker [chart x y label]
  (let [marker (doto (IntervalMarker. x y) 
                 (.setLabel label) 
                 (.setPaint (Color. 233  194  166)))]
    (doseq [plot (get-plots chart)]
      (.addDomainMarker plot marker org.jfree.ui.Layer/BACKGROUND))    
    marker))

(defn remove-domain-interval-marker [chart marker]
  (doseq [plot (get-plots chart)]
    (.removeDomainMarker plot marker org.jfree.ui.Layer/BACKGROUND))
  chart)

(defn clear-domain-markers [chart]
  (doseq [plot (get-plots chart)]
    (.clearDomainMarkers plot)))

(defn clear-value-markers [chart]
  (doseq [plot (get-plots chart)]
    (.clearRangeMarkers plot)))

(defn use-relative-time-axis 
  "Replace domain axis by relative date/time axis."
  [chart]
  (let [rdf  (doto (RelativeDateFormat. (long 0)) 
               (.setSecondFormatter (DecimalFormat. "00"))
               (.setShowZeroDays false))
        axis (doto (DateAxis.) (.setDateFormatOverride rdf))]
    (doseq [plot (get-plots chart)] 
      (.setDomainAxis plot axis))
    chart))

(defn use-time-axis 
  "Use DateAxis. If a start- and end-hour are specified only these hours will be shown."
  ([chart start-hour end-hour]
    (let [one-hour (* 60 60 1000)
          hours (- end-hour start-hour)
          timeline (doto (SegmentedTimeline. one-hour hours (- 24 hours))
                     (.setStartTime (+ (* start-hour one-hour) (SegmentedTimeline/firstMondayAfter1900))))
          axis (doto (DateAxis.) (.setTimeline timeline))]
      (doseq [plot (get-plots chart)]
        (.setDomainAxis plot axis))
      chart))
  ([chart]
    (doseq [plot (get-plots chart)]
      (.setDomainAxis plot (DateAxis.)))
    chart))

(defn create-renderer
  "do not plot a line when at least 3 values are missing (for example during the night)" 
  []
  (doto (StandardXYItemRenderer.)
    (.setPlotDiscontinuous true)
    (.setGapThresholdType UnitType/RELATIVE)
    (.setGapThreshold 3.0)))

(defn set-plot-discontinuous 
  "If there are more than 3 missing values in a series, 
do not connect points at this gap. Creates new renderers for the given series!
If no series index is given, do this for all series of the plot."
  ([^org.jfree.chart.plot.XYPlot plot] (dotimes [i (.getRendererCount plot)] 
                                         (set-plot-discontinuous plot i))) 
  ([^org.jfree.chart.plot.XYPlot plot idx] 
    (.setRenderer plot idx (create-renderer))
    plot))

(defn set-step-renderer
  "Render horizontal lines per interval instead of connecting points linearly."
  [chart n]
  (doseq [plot (get-plots chart)]
    (.setRenderer plot n (org.jfree.chart.renderer.xy.XYStepRenderer.))))

(defn add-sub-title 
  "Add a subtitle to the chart."
  [chart text]
  (.addSubTitle chart (org.jfree.chart.title.TextTitle. text))
  chart)

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
          width 800
          height (* 70 (count slider-fns))]
      (.setLayout panel (BoxLayout. panel BoxLayout/Y_AXIS))
      (dorun (doseq [p panels] (.add panel p)))
      (doto frame
        (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
        (.add panel BorderLayout/CENTER)
        (.setSize width height)
        ;;(.pack)
        (.setLocationRelativeTo nil)
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
      (.setScale l scale))
    chart))

(defn set-heat-map-data [chart ^org.jfree.data.xy.DefaultXYZDataset data]
  (let [ds (.. chart getPlot getDataset)
        sk (.getSeriesKey ds 0)]
    (doto ds 
      (.removeSeries sk)
      (.addSeries sk data))
    chart)) 

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
        bins ((->bins min-x x-bin-width min-y y-bin-width bin-x bin-y) xs ys)]
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
        combined-plot (CombinedDomainXYPlot. axis)]
    (doseq [p plots] (.add combined-plot p))
    combined-plot))

(defn combined-range-plot 
  "Combine several XYPlots into one plot with a common range axis (all charts in one row)."
  [& plots]
  (let [axis (org.jfree.chart.axis.NumberAxis. "")
        combined-plot (org.jfree.chart.plot.CombinedRangeXYPlot. axis)]
    (doseq [p plots] (.add combined-plot p))
    combined-plot))

(defn combined-domain-chart
  [charts & {:keys [keep-titles] :or {keep-titles true}}]
  (when keep-titles
    (doseq [chart charts 
            :let [title (.getTitle chart)]
            :when title
            plot (get-plots chart)]
           (.addAnnotation plot (org.jfree.chart.annotations.XYTitleAnnotation. 0.5 0.95 title))))
  (let [plots (mapcat get-plots charts)]    
    (org.jfree.chart.JFreeChart. (apply combined-domain-plot plots))))

(defn set-y-range [chart idx lower upper]
  (let [plot (.. chart getPlot)]
    (.. plot (getRangeAxis idx) (setRange lower upper))
    chart))

(defn set-y-ranges 
  "Set y range for all available y-axes."
  [chart lower upper]
  (let [plot (.. chart getPlot)] 
    (dotimes [i (.getRangeAxisCount plot)]
      (.. plot (getRangeAxis i) (setRange lower upper))))
  chart)

(defn set-discontinuous [chart]
  (let [new-renderer (fn [old-renderer] 
                       (let [p (.getSeriesPaint old-renderer 0)]
                         (doto (org.jfree.chart.renderer.xy.StandardXYItemRenderer.) 
                           (.setSeriesPaint 0 p)
                           (.setPlotDiscontinuous true))))
        p (.getPlot chart)] 
    (dotimes [n (.getRendererCount p)]
      (.setRenderer p n (new-renderer (.getRenderer p n))))
    chart))

(defn set-step-renderer 
  "Set an instance of `org.jfree.chart.renderer.xy.XYStepRenderer` for data series `n`."
  [chart n]
  (.. chart getPlot (setRenderer n (org.jfree.chart.renderer.xy.XYStepRenderer.)))
  chart)

(defn map-to-axis 
  "Map dataseries to separate y axis. Reuses fonts, colors, paints from the first axis."
  [chart series-idx axis-idx & {:keys [label]}]
  (let [p (.getPlot chart)
        a0 (.. p (getRangeAxis 0)) 
        axis-count (.getRangeAxisCount p)
        axis (or (.getRangeAxis p axis-idx)
                 (doto (org.jfree.chart.axis.NumberAxis.)
                    (.setLabelFont (.getLabelFont a0)) 
                    (.setTickLabelFont (.getTickLabelFont a0)) 
                    (.setLabelPaint (.getLabelPaint a0))
                    (.setAxisLinePaint (.getAxisLinePaint a0))
                    (.setTickLabelPaint (.getTickLabelPaint a0))))]
        (when label (.setLabel axis label))
       (when (not= axis (.getRangeAxis p axis-idx)) 
             (.setRangeAxis p axis-idx axis))
       (.mapDatasetToRangeAxis p series-idx axis-idx)
       chart))

;; TODO use `get-plots`, ask each plot for number of series, then identify the correct one. This should enable updating combined-plots
(defn- get-series
  "get-series"
  ([chart] (get-series chart 0))
  ([chart series-idx]
   (let [ds (.. chart getPlot (getDataset series-idx))]
     (cond
       (instance? Series ds) ds
       (instance? XYSeriesCollection ds) (first (seq (.getSeries ds)))
       (instance? YIntervalSeriesCollection ds) (.getSeries ds series-idx)))))

(defn perf-set-data 
  "Replace the data of a series without firing events for every single data point
by avoiding `addOrUpdate`."
  [chart data series-idx]
  (let [series (get-series chart series-idx)]
       (.clear series)
       (cond
        (= 2 (count (first data)))
          (doseq [[a b] data]
                 (.add series a b false))
        (= 2 (count data))
          (dorun (map #(.add series %1 %2 false) (first data) (second data)))
        (and (instance? YIntervalSeries series)
             (= 4 (count (first data))))
        (doseq [[x y l u] data]
          (.add ^YIntervalSeries series (YIntervalDataItem. (double x) (double y) (double l) (double u)) false))
        (and (instance? YIntervalSeries series)
             (= 4 (count data)))
          (perf-set-data chart (apply map vector data) series-idx )
        :else (throw (Exception. "Data has wrong number of dimensions")))
       (.fireSeriesChanged series) 
       chart))

(defn deviation-plot 
  "Create xy-plot of xs and vs, render minima and maxima as an area behind the line."
  [xs vs lower upper & {:keys [series-label legend x-label y-label title fill-color-fn]
                        :or {legend false 
                             series-label "" 
                             title "Chart"
                             x-label "x" 
                             y-label "y"
                             fill-color-fn (fn fill-color-fn [^Color col] (.brighter col))}}] 
  (let [series (YIntervalSeries. series-label)
        collection (doto (YIntervalSeriesCollection.) (.addSeries series))
        _ (doseq [[x y l u] (map vector xs vs lower upper)]  
            (.add series x y l u))
        show-lines true
        show-shapes false
        renderer (doto 
                   (proxy [DeviationRenderer] [show-lines show-shapes]
                     (getSeriesFillPaint [series]
                       (fill-color-fn (proxy-super lookupSeriesPaint series))))
                   (.setAutoPopulateSeriesFillPaint true))
        chart (org.jfree.chart.ChartFactory/createXYLineChart
                title 
                x-label 
                y-label 
                collection
                org.jfree.chart.plot.PlotOrientation/VERTICAL
                false
                false
                false)]
    (.. chart getPlot (setRenderer renderer))
    chart))

(defmulti set-color-brewer-colors class)
(defmethod set-color-brewer-colors JFreeChart [chart]
  (set-color-brewer-colors (.getPlot chart))
  chart)

(defmethod set-color-brewer-colors org.jfree.chart.plot.Plot [plot]
  (let [brewer [(Color. 0x1f 0x77 0xb4)
                (Color. 0xae 0xc7 0xe8)
                (Color. 0xff 0x7f 0x0e)
                (Color. 0xff 0xbb 0x78)
                (Color. 0x2c 0xa0 0x2c)
                (Color. 0x98 0xdf 0x8a)
                (Color. 0xd6 0x27 0x28)
                (Color. 0xff 0x98 0x96)
                (Color. 0x94 0x67 0xbd)
                (Color. 0xc5 0xb0 0xd5)
                (Color. 0x8c 0x56 0x4b)
                (Color. 0xc4 0x9c 0x94)
                (Color. 0xe3 0x77 0xc2)
                (Color. 0xf7 0xb6 0xd2)
                (Color. 0x7f 0x7f 0x7f)
                (Color. 0xc7 0xc7 0xc7)
                (Color. 0xbc 0xbd 0x22)
                (Color. 0xdb 0xdb 0x8d)
                (Color. 0x17 0xbe 0xcf)
                (Color. 0x9e 0xda 0xe5)]
        orig-array DefaultDrawingSupplier/DEFAULT_PAINT_SEQUENCE
        colors (concat brewer orig-array)] 
    (.setDrawingSupplier plot
      (DefaultDrawingSupplier. (into-array colors)
                               DefaultDrawingSupplier/DEFAULT_OUTLINE_PAINT_SEQUENCE,
                               DefaultDrawingSupplier/DEFAULT_STROKE_SEQUENCE,
                               DefaultDrawingSupplier/DEFAULT_OUTLINE_STROKE_SEQUENCE
                               DefaultDrawingSupplier/DEFAULT_SHAPE_SEQUENCE))
    plot))


(defn stacked-area-chart
  ([xs values & options]
    (let [opts (when options (apply assoc {} options))
          title (or (:title opts) "")
          x-label (or (:x-label opts) "xs")
          y-label (or (:y-label opts) "values")
          series-labels (or (:series-labels opts) (vec (map str (range (count values)))))
          legend? (true? (:legend opts))
          dataset (org.jfree.data.xy.DefaultTableXYDataset.)
          chart (org.jfree.chart.ChartFactory/createStackedXYAreaChart
                  title
                  x-label
                  y-label
                  dataset
                  org.jfree.chart.plot.PlotOrientation/VERTICAL
                  legend?
                  true
                  false)]
      (do
        (doseq [i (range 0 (count values)) 
                :let [lbl (nth series-labels i)
                      ds (XYSeries. lbl false false)
                      vs (nth values i)]]
          (doseq [[x y] (map vector xs vs)]
            (.add ds x y false))
          (.addSeries dataset ds))
          ;(set-theme chart theme)
          chart))))
(defn polar-plot [angles values & {:keys [series-label title legend] 
                                   :or {series-label "values"
                                        title "Polar chart"
                                        legend false}}]
  (let [dataset (org.jfree.data.xy.XYSeriesCollection.)
        series (org.jfree.data.xy.XYSeries. series-label)]
    (.addSeries dataset series)
    (doseq [[angle value] (map vector angles values)]
      (.add series angle value false))
    (org.jfree.chart.ChartFactory/createPolarChart 
     title
     dataset
     legend
     false
     false)))

(defn set-color 
  "Set a java.awt.Color for each series with index `series-idx` in charts.
Assumes that each renderer is responsible for one series only."
  [chart series-idx color]
  (doseq [^XYPlot plot (get-plots chart)
          :let [renderer (.getRenderer plot series-idx)]]
    (.setSeriesPaint renderer 0 color)
    (.setLegendTextPaint renderer 0 color)))

#_(defn to-foreground [chart series-idx]
  (doseq [^XYPlot plot (get-plots chart)
          :let [n (.getDatasetCount plot)]
          :when (>= n series-idx)
          :let [old-last (.getDataset plot (dec n))
                new-last (.getDataset plot series-idx)]] 
    (.setDataset plot (dec n) new-last)
    (.setDataset plot series-idx old-last)))


(defn- dataset-index [chart dataset]
  (first (for [plot (get-plots chart)
               :let [idx (some #(when (= (.. plot (getDataset %)) dataset) %) (range (.. plot getDatasetCount)))]
               :when idx]
           idx)))

(defn add-mouse-listener 
  "Toggle color of a line by clicking on it's legend entry or the line itself."
  [^org.jfree.chart.ChartPanel chart]
  (let [state (atom {})
        highlight-color java.awt.Color/MAGENTA
        listener (reify org.jfree.chart.ChartMouseListener
                   (chartMouseMoved [_ evt])
                   (chartMouseClicked [_ evt]
                     (let [entity (.getEntity evt)
                           dataset (condp instance? entity  
                                     org.jfree.chart.entity.XYItemEntity (.getDataset entity)
                                     org.jfree.chart.entity.LegendItemEntity (.getDataset (.getEntity evt))
                                     nil)]
                       (when dataset 
                         (let [chart (.getChart evt)
                               idx (dataset-index chart dataset)                             
                               current-color (get @state idx)]
                           (if current-color
                             (do
                               (swap! state dissoc idx) ;remove it
                               (set-color chart idx current-color)) ; set stored color 
                             (do
                               (swap! state assoc idx (.. (first (get-plots chart)) (getRenderer idx) (getSeriesPaint 0)))
                               (set-color chart idx highlight-color)
                               #_(to-foreground chart idx))))))))]
    (.addChartMouseListener chart listener)
    listener))
