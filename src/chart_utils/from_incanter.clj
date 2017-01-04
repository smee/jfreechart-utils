(in-ns 'chart-utils.jfreechart)

(defn view
"From incanter https://github.com/incanter/incanter/blob/master/modules/incanter-charts/src/incanter/charts.clj#L3405"
  [chart & options]
    (let [opts (when options (apply assoc {} options))
          window-title (or (:window-title opts) "Incanter Plot")
          width (or (:width opts) 500)
          height (or (:height opts) 400)
          frame (ChartFrame. window-title chart)]
      (doto frame
        (.setSize width height)
        (.setVisible true))
      frame))

(defn save
"From incanter https://github.com/incanter/incanter/blob/master/modules/incanter-charts/src/incanter/charts.clj#L3418"
  [chart filename & options]
  (let [opts (when options (apply assoc {} options))
        width (or (:width opts) 500)
        height (or (:height opts) 400)]
    ;; if filename is not a string, treat it as java.io.OutputStream
    (if (string? filename)
      (ChartUtilities/saveChartAsPNG (File. filename) chart width height)
      (ImageIO/write (.createBufferedImage chart width height) "png" filename))
    nil))


(defn set-title
  "
  Sets the main title of the plot, returns the modified chart object.
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([chart title]
    (.setTitle chart title)
    chart))


(defn set-x-label
  "
  Sets the label of the x-axis, returns the modified chart object.
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([chart label]
    (.setLabel (.getDomainAxis (.getPlot chart)) label)
    chart))


(defn set-y-label
  "
  Sets the label of the y-axis, returns the modified chart object.
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([chart label]
    (.setLabel (.getRangeAxis (.getPlot chart)) label)
    chart))


(defn set-x-range
  "
  Sets the range of the x-axis on the given chart.
  Examples:
    (use '(incanter core charts datasets))
    (def chart (xy-plot :speed :dist :data (get-dataset :cars)))
    (view chart)
    (set-x-range chart 10 20)
  "
  ([chart lower upper]
     (-> chart
         .getPlot
         .getDomainAxis
         (.setRange lower upper))
     chart))


(defn set-y-range
  "
  Sets the range of the y-axis on the given chart.
  Examples:
    (use '(incanter core charts datasets))
    (def chart (xy-plot :speed :dist :data (get-dataset :cars)))
    (view chart)
    (set-y-range chart 10 60)
  "
  ([chart lower upper]
     (-> chart
         .getPlot
         .getRangeAxis
         (.setRange lower upper))
   chart))


(defmulti set-background-default
  (fn [chart] (type (.getPlot chart))))


(defmethod set-background-default org.jfree.chart.plot.XYPlot
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)
           plot (.getPlot chart)]
       (doto plot
         (.setRangeGridlineStroke grid-stroke)
         (.setDomainGridlineStroke grid-stroke)
         (.setBackgroundPaint java.awt.Color/lightGray)
         (.setBackgroundPaint (java.awt.Color. 235 235 235))
         (.setRangeGridlinePaint java.awt.Color/white)
         (.setDomainGridlinePaint java.awt.Color/white)
         (.setOutlineVisible false)
         (-> .getDomainAxis (.setAxisLineVisible false))
         (-> .getRangeAxis (.setAxisLineVisible false))
         (-> .getDomainAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getDomainAxis (.setTickLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setTickLabelPaint java.awt.Color/gray))
         ;; (.setDomainMinorGridlinesVisible true)
         ;; (.setRangeMinorGridlinesVisible true)
         (.setDomainZeroBaselineVisible false)
         )
       (if (= (-> plot .getDataset type)
              org.jfree.data.statistics.HistogramDataset)
         (-> plot .getRenderer (.setPaint java.awt.Color/gray)))
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))


(defmethod set-background-default org.jfree.chart.plot.CategoryPlot
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)]
       (doto (.getPlot chart)
         (.setRangeGridlineStroke grid-stroke)
         (.setDomainGridlineStroke grid-stroke)
         (.setBackgroundPaint java.awt.Color/lightGray)
         (.setBackgroundPaint (java.awt.Color. 235 235 235))
         (.setRangeGridlinePaint java.awt.Color/white)
         (.setDomainGridlinePaint java.awt.Color/white)
         (.setOutlineVisible false)
         (-> .getDomainAxis (.setAxisLineVisible false))
         (-> .getRangeAxis (.setAxisLineVisible false))
         (-> .getDomainAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setLabelPaint java.awt.Color/gray))
         (-> .getDomainAxis (.setTickLabelPaint java.awt.Color/gray))
         (-> .getRangeAxis (.setTickLabelPaint java.awt.Color/gray))
         ;; (.setRangeMinorGridlinesVisible true)
         )
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))


(defmethod set-background-default org.jfree.chart.plot.PiePlot
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1.5
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)]
       (doto (.getPlot chart)
         ;; (.setRangeGridlineStroke grid-stroke)
         ;; (.setDomainGridlineStroke grid-stroke)
         (.setBackgroundPaint java.awt.Color/white)
         (.setShadowPaint java.awt.Color/white)
         (.setLabelShadowPaint java.awt.Color/white)
         (.setLabelPaint java.awt.Color/darkGray)
         (.setLabelOutlinePaint java.awt.Color/gray)
         (.setLabelBackgroundPaint (java.awt.Color. 235 235 235))
         (.setLabelLinksVisible false)
         (.setOutlineVisible false))
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))


(defmethod set-background-default :default
  ([chart]
     (let [grid-stroke (java.awt.BasicStroke. 1.5
                                              java.awt.BasicStroke/CAP_ROUND
                                              java.awt.BasicStroke/JOIN_ROUND
                                              1.0
                                              (float-array 2.0 1.0)
                                              0.0)]
       (doto (.getPlot chart)
         (.setBackgroundPaint java.awt.Color/lightGray)
         (.setBackgroundPaint (java.awt.Color. 235 235 235))
         (.setOutlineVisible false))
       (-> chart .getTitle (.setPaint java.awt.Color/gray))
       chart)))

(defn- create-xy-plot
  [title x-lab y-lab dataset legend? tooltips? urls?]
  (org.jfree.chart.ChartFactory/createXYLineChart
    title
    x-lab
    y-lab
    dataset
    org.jfree.chart.plot.PlotOrientation/VERTICAL
    legend?
    tooltips?
    urls?))

(defn- create-time-series-plot
  [title x-lab y-lab dataset legend? tooltips? urls?]
  (org.jfree.chart.ChartFactory/createTimeSeriesChart
    title
    x-lab
    y-lab
    dataset
    legend?
    tooltips?
    urls?))

(defn- in-coll
  "
  in-coll [x]
  If x is a collection, return it
  Otherwise return x in a vector"
  [x]
  (if (coll? x)
    x
    [x]))

(defmulti set-theme-bw
  (fn [chart & options] (type (-> chart .getPlot .getDataset))))

(defmethod set-theme-bw org.jfree.data.xy.XYSeriesCollection
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))


(defmethod set-theme-bw org.jfree.data.statistics.HistogramDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))


(defmethod set-theme-bw org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))


(defmethod set-theme-bw org.jfree.data.category.DefaultCategoryDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)]
       (do
          (doto plot
           (.setBackgroundPaint java.awt.Color/white)
           (.setRangeGridlinePaint (java.awt.Color. 235 235 235))
           (.setDomainGridlinePaint (java.awt.Color. 235 235 235)))
         (doto renderer
           (.setOutlinePaint java.awt.Color/white)
           (.setPaint java.awt.Color/gray))
         chart))))

(defmulti set-theme-default
  (fn [chart & options] (type (-> chart .getPlot .getDataset))))

(defmethod set-theme-default org.jfree.data.category.DefaultCategoryDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)
           bar-painter (org.jfree.chart.renderer.category.StandardBarPainter.)]
       (when (some #{(type (.getRenderer (.getPlot chart)))}
                #{org.jfree.chart.renderer.category.BarRenderer
                  org.jfree.chart.renderer.category.StackedBarRenderer})
         (doto renderer
           (.setBarPainter bar-painter)
           (.setSeriesOutlinePaint 0 java.awt.Color/lightGray)
           (.setShadowVisible false)
           (.setDrawBarOutline false)))
       (set-background-default chart)
       chart)))


(defmethod set-theme-default org.jfree.data.statistics.HistogramDataset
  ([chart]
     (let [plot (.getPlot chart)
           renderer (.getRenderer plot)
           bar-painter (org.jfree.chart.renderer.xy.StandardXYBarPainter.)]
       (doto renderer
         (.setBarPainter bar-painter)
         (.setSeriesOutlinePaint 0 java.awt.Color/lightGray)
         (.setShadowVisible false)
         (.setDrawBarOutline true))
       (set-background-default chart)
       chart)))



(defmethod set-theme-default :default
  ([chart]
     (set-background-default chart)))

(defn set-theme
  "
  Changes the chart theme.
  Arguments:
    chart -- an Incanter/JFreeChart object
    theme -- either a keyword indicating one of the built-in themes, or a JFreeChart ChartTheme object.
  Built-in Themes:
    :default
    :dark
  Examples:
    (use '(incanter core charts))
    (def chart (function-plot sin -4 4))
    (view chart)
    ;; change the theme of chart to :dark
    (set-theme chart :dark)
    ;; change it back to the default
    (set-theme chart :default)
    ;; Example using JFreeTheme
    (use '(incanter core stats charts datasets))
    (import '(org.jfree.chart StandardChartTheme)
            '(org.jfree.chart.plot DefaultDrawingSupplier)
            '(java.awt Color))
    (def all-red-theme
      (doto
        (StandardChartTheme/createJFreeTheme)
        (.setDrawingSupplier
        (proxy [DefaultDrawingSupplier] []
          (getNextPaint [] Color/red)))))
    (def data (get-dataset :airline-passengers))
    (def chart (bar-chart :month :passengers :group-by :year :legend true :data data))
    (doto chart
      ;; has no effect
      (set-theme all-red-theme)
      view)
  References:
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/StandardChartTheme.html
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/ChartTheme.html
  "
  ([chart theme]
     (let [built-in-theme? (some #{theme} #{:dark :legacy :gradient})
           _theme (if built-in-theme?
                    (cond
                     (= theme :dark)
                       (StandardChartTheme/createDarknessTheme)
                     (= theme :legacy)
                       (StandardChartTheme/createLegacyTheme)
                     :default
                       (StandardChartTheme/createJFreeTheme))
                    (cond
                     (= theme :bw)
                       set-theme-bw
                     (instance? ChartTheme theme)
                       #(.apply theme %)
                     :default
                       set-theme-default))
           ;; bar-painter
           ;; (org.jfree.chart.renderer.xy.StandardXYBarPainter.)
           ]
       (do
         (if built-in-theme?
           (do
             (.setShadowVisible _theme false)
             (.apply _theme chart))
           (do
             ;; (doto (-> chart .getPlot .getRenderer)
;;             (.setBarPainter bar-painter)
;;             (.setSeriesOutlinePaint 0 java.awt.Color/lightGray)
;;             (.setShadowVisible false)
;;             (.setDrawBarOutline true))
             (_theme chart)))
         chart))))

(defn- create-xy-series-plot
  ([x y create-plot & options]
    (let [opts (when options (apply assoc {} options))
          data (:data opts)
          _x x
          _y y
          _group-by (when (:group-by opts)
                      (:group-by opts))
          __x (in-coll _x)
          __y (in-coll _y)
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (str 'x))
          y-lab (or (:y-label opts) (str 'y))
          series-lab (or (:series-label opts)
                         (format "%s, %s" 'x 'y))
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          points? (true? (:points opts))
          data-series (XYSeries. (cond
                                   _group-by
                                     (first _group-by)
                                   :else
                                     series-lab)
                                 (:auto-sort opts true))
          dataset (XYSeriesCollection.)
          chart (do
                  (dorun
                   (map (fn [x y]
                        (if (and (not (nil? x))
                                 (not (nil? y)))
                          (.add data-series (double x) (double y))))
                        __x __y))
                  (.addSeries dataset data-series)
                  (create-plot
                   title
                   x-lab
                   y-lab
                   dataset
                   legend?
                   true  ; tooltips
                   false))]
      (.setRenderer (.getPlot chart) 0 (XYLineAndShapeRenderer. true points?))
      (set-theme chart theme)
      chart)))


(defn xy-plot* [x y & options]
  (apply create-xy-series-plot x y create-xy-plot options))

(defmacro xy-plot
  "
  Returns a JFreeChart object representing a xy-plot of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.
  Options:
    :data (default nil) If the :data option is provided a dataset,
                        column names can be used instead of sequences
                        of data as arguments to xy-plot.
    :title (default 'XY Plot') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
    :group-by (default nil) -- a vector of values used to group the x and y values into series.
    :points (default false) includes point-markers
    :auto-sort (default true) sort data by x
  See also:
    view, save, add-points, add-lines
  Examples:
    (use '(incanter core stats charts))
    ;; plot the cosine function
    (def x (range -1 5 0.01))
    (def y (cos (mult 2 Math/PI x)))
    (view (xy-plot x y))
    ;; plot gamma pdf with different parameters
    (def x2 (range 0 20 0.1))
    (def gamma-plot (xy-plot x2 (pdf-gamma x2 :shape 1 :scale 2)
                               :legend true
                               :title \"Gamma PDF\"
                               :y-label \"Density\"))
    (view gamma-plot)
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 2 :scale 2))
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 3 :scale 2))
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 5 :scale 1))
    (add-lines gamma-plot x2 (pdf-gamma x2 :shape 9 :scale 0.5))
    ;; use :group-by option
    (use '(incanter core charts datasets))
    (with-data (get-dataset :chick-weight)
      (view (xy-plot :Time :weight :group-by :Chick)))
    ;; see INCANTER_HOME/examples/probability_plots.clj for more examples of plots
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([]
     `(xy-plot [] [] :x-label "x" :y-label "y"))
  ([x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~x))
           y-lab# (or (:y-label opts#) (str '~y))
           series-lab# (or (:series-label opts#)
                           (if group-by#
                             (format "%s, %s (0)" '~x '~y)
                             (format "%s, %s" '~x '~y)))
           args# (concat [~x ~y] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
       (apply xy-plot* args#))))

(defn time-series-plot* [x y & options]
  (apply create-xy-series-plot x y create-time-series-plot options))

(defmacro time-series-plot
  "
  Returns a JFreeChart object representing a time series plot of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file. Sequence passed in for the x axis should be
  number of milliseconds from the epoch (1 January 1970).
  Options:
    :data (default nil) If the :data option is provided a dataset,
                        column names can be used instead of sequences
                        of data as arguments to xy-plot.
    :title (default '') main title
    :x-label (default x expression)
    :y-label (default y expression)
    :legend (default false) prints legend
    :series-label (default x expression)
    :group-by (default nil) -- a vector of values used to group the x and y values into series.
  See also:
    view, save, add-points, add-lines
  Examples:
    (use '(incanter core stats charts))
    (require '[clj-time.core :refer [date-time]])
    ;; plot numbers against years starting with 1900
    (def dates (map #(-> (date-time (+ 1900 %))
                         .getMillis)
                    (range 100)))
    (def y (range 100))
    (view (time-series-plot dates y
                            :x-label \"Year\"))
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           legend# (or (:legend opts#) false)
           x-lab# (or (:x-label opts#) (str '~x))
           y-lab# (or (:y-label opts#) (str '~y))
           series-lab# (or (:series-label opts#) (format "%s, %s" '~x '~y))
           args# (concat [~x ~y] (apply concat (seq (apply assoc opts#
                                                           [:title title#
                                                            :legend legend#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply time-series-plot* args#))))

(defn histogram*
  ([x & options]
    (let [opts (if options (apply assoc {} options) {})
          data (:data opts)
          _x x
          nbins (or (:nbins opts) 10)
          theme (or (:theme opts) :default)
          density? (true? (:density opts))
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (str 'x))
          y-lab (or (:y-label opts)
                     (if density? "Density" "Frequency"))
          series-lab (or (:series-label opts) (str 'x))
          legend? (true? (:legend opts))
          dataset (HistogramDataset.)]
      (do
        (.addSeries dataset series-lab (double-array _x) nbins)
        (when density? (.setType dataset org.jfree.data.statistics.HistogramType/SCALE_AREA_TO_1))
        (let [chart (-> (org.jfree.chart.ChartFactory/createHistogram
                          title
                          x-lab
                          y-lab
                          dataset
                          org.jfree.chart.plot.PlotOrientation/VERTICAL
                          legend?		; no legend
                          true			; tooltips
                          false)
                        (set-theme theme))]
          chart)))))


(defmacro histogram
  "
  Returns a JFreeChart object representing the histogram of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.
  Options:
    :nbins (default 10) number of bins
    :density (default false) if false, plots frequency, otherwise density
    :title (default 'Histogram') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
  See also:
    view, save, add-histogram
  Examples:
    (use '(incanter core charts stats))
    (view (histogram (sample-normal 1000)))
    # plot a density histogram
    (def hist (histogram (sample-normal 1000) :density true))
    (view hist)
    # add a normal density line to the plot
    (def x (range -4 4 0.01))
    (add-lines hist x (pdf-normal x))
    # plot some gamma data
    (def gam-hist (histogram (sample-gamma 1000) :density true :nbins 30))
    (view gam-hist)
    (def x (range 0 8 0.01))
    (add-lines gam-hist x (pdf-gamma x))
    (use 'incanter.datasets)
    (def iris (get-dataset :iris))
    (view (histogram :Sepal.Width :data iris))
    (with-data (get-dataset :iris)
      (view (histogram :Petal.Length)))
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([x & options]
    `(let [opts# ~(if options (apply assoc {} options) {})
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~x))
           series-lab# (or (:series-label opts#) (str '~x))
           args# (concat [~x] (apply concat (seq (apply assoc opts#
                                                        [:title title#
                                                         :x-label x-lab#
                                                         :series-label series-lab#]))))]
        (apply histogram* args#))))


(defn line-chart*
  ([categories values & options]
    (let [opts (when options (apply assoc {} options))
          data (:data opts)
          _values values
          _categories categories
          title (or (:title opts) "")
          group-by data
          x-label (or (:x-label opts) (str 'categories))
          y-label (or (:y-label opts) (str 'values))
          series-label (:series-label opts)
          vertical? (if (false? (:vertical opts)) false true)
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          dataset (DefaultCategoryDataset.)
          chart (org.jfree.chart.ChartFactory/createLineChart
                 title
                 x-label
                 y-label
                 dataset
                 (if vertical?
                   org.jfree.chart.plot.PlotOrientation/VERTICAL
                   org.jfree.chart.plot.PlotOrientation/HORIZONTAL)
                 legend?
                 true
                 false)]
      (do
        (doseq [i (range 0 (count _values))] (.addValue dataset
                                                       (nth _values i)
                                                       (cond
                                                        group-by
                                                          (nth group-by i)
                                                        series-label
                                                          series-label
                                                        :else
                                                          (str 'values))
                                                       (nth _categories i)))
        (set-theme chart theme)
        chart))))


(defmacro line-chart
  "
  Returns a JFreeChart object representing a line-chart of the given values and categories.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.
  Arguments:
    categories -- a sequence of categories
    values -- a sequence of numeric values
  Options:
    :title (default '') main title
    :x-label (default 'Categories')
    :y-label (default 'Value')
    :legend (default false) prints legend
    :series-label
    :group-by (default nil) -- a vector of values used to group the values into
                               series within each category.
    :gradient? (default false) -- use gradient on bars
  See also:
    view and save
  Examples:
    (use '(incanter core stats charts datasets))
    (def data (get-dataset :airline-passengers))
    (def years (sel data :cols 0))
    (def months (sel data :cols 2))
    (def passengers (sel data :cols 1))
    (view (line-chart years passengers :group-by months :legend true))
    (view (line-chart months passengers :group-by years :legend true))
    (def seasons (mapcat identity (repeat 3 [\"winter\" \"spring\" \"summer\" \"fall\"])))
    (def years (mapcat identity (repeat 4 [2007 2008 2009])))
    (def x (sample-uniform 12 :integers true :max 100))
    (view (line-chart years x :group-by seasons :legend true))
    (view (line-chart [\"a\" \"b\" \"c\" \"d\" \"e\" \"f\"] [10 20 30 10 40 20]))
    (view (line-chart (sample \"abcdefghij\" :size 10 :replacement true)
                         (sample-uniform 10 :max 50) :legend true))
    ;; add a series label
    (def plot (line-chart [\"a\" \"b\" \"c\"] [10 20 30] :legend true :series-label \"s1\"))
    (view plot)
    (add-categories plot [\"a\" \"b\" \"c\"] [5 25 40] :series-label \"s2\")
    (view (line-chart :year :passengers :group-by :month :legend true :data data))
    (view (line-chart :month :passengers :group-by :year :legend true :data data))
    (with-data data
      (view (line-chart :month :passengers :group-by :year :legend true)))
    (with-data (->> ($rollup :sum :passengers :year (get-dataset :airline-passengers))
                    ($order :year :asc))
      (view (line-chart :year :passengers)))
    (with-data (->> ($rollup :sum :passengers :month (get-dataset :airline-passengers))
                    ($order :passengers :asc))
      (view (line-chart :month :passengers)))
    (with-data ($rollup :sum :passengers :month (get-dataset :airline-passengers))
      (view (line-chart :month :passengers)))
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([categories values & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~categories))
           y-lab# (or (:y-label opts#) (str '~values))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s, %s (0)" '~categories '~values)
                                                   (format "%s, %s" '~categories '~values)))
           args# (concat [~categories ~values] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
       (apply line-chart* args#))))

(defmulti add-lines* (fn [chart x y & options] (type (-> chart .getPlot .getDataset))))

(defmethod add-lines* org.jfree.data.xy.XYSeriesCollection
  ([chart x y & options]
     (let [opts (when options (apply assoc {} options))
           data (:data opts)
           _x x
           _y y
           data-plot (.getPlot chart)
           n (.getDatasetCount data-plot)
           series-lab (or (:series-label opts) (format "%s, %s" 'x 'y))
           data-series (XYSeries. series-lab (:auto-sort opts true))
           points? (true? (:points opts))
           line-renderer (XYLineAndShapeRenderer. true points?)
           ;; data-set (.getDataset data-plot)
           data-set (XYSeriesCollection.)]
       (dorun
        (map (fn [x y]
               (if (and (not (nil? x))
                        (not (nil? y)))
                 (.add data-series (double x) (double y))))
             _x _y))
      (.addSeries data-set data-series)
      (doto data-plot
        (.setSeriesRenderingOrder org.jfree.chart.plot.SeriesRenderingOrder/FORWARD)
        (.setDatasetRenderingOrder org.jfree.chart.plot.DatasetRenderingOrder/FORWARD)
        (.setDataset n data-set)
        (.setRenderer n line-renderer))
      chart)))

(defmacro add-lines
  "
  Plots lines on the given scatter or line plot (xy-plot) of the (x,y) points.
  Equivalent to R's lines function, returns the modified chart object.
  Options:
    :series-label (default x expression)
    :points (default false)
    :auto-sort (default true) sort data by x
  Examples:
    (use '(incanter core stats io datasets charts))
    (def cars (to-matrix (get-dataset :cars)))
    (def y (sel cars :cols 0))
    (def x (sel cars :cols 1))
    (def plot1 (scatter-plot x y :legend true))
    (view plot1)
    ;; add regression line to scatter plot
    (def lm1 (linear-model y x))
    (add-lines plot1 x (:fitted lm1))
    ;; model the data without an intercept
    (def lm2 (linear-model y x :intercept false))
    (add-lines plot1 x (:fitted lm2))
    ;; Clojure's doto macro can be used to build a chart
    (doto (histogram (sample-normal 1000) :density true)
          (add-lines (range -3 3 0.05) (pdf-normal (range -3 3 0.05)))
          view)
    (with-data (get-dataset :iris)
        (doto (xy-plot :Sepal.Width :Sepal.Length :legend true)
              (add-lines :Petal.Width :Petal.Length)
              view))
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html
  "
  ([chart x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#)
                           (format "%s, %s" '~x '~y))
           args# (concat [~chart ~x ~y] (apply concat (seq (apply assoc opts#
                                                                  [:series-label series-lab#]))))]
        (apply add-lines* args#))))

(defn- range-inclusive
  "Similar to range but adds end to result."
  [start end step]
  (concat (range start end step) [end]))

(defn function-plot*
  ([function min-range max-range & options]
    (let [opts (when options (apply assoc {} options))
          step-size (or (:step-size opts) (float (/ (- max-range min-range) 500)))
          _x (range-inclusive min-range max-range step-size)
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (format "%s < x < %s" min-range max-range))
          y-lab (or (:y-label opts) (str 'function))
          series-lab (or (:series-label opts) (format "%s" 'function))
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))]
      (set-theme (xy-plot _x (map function _x)
                          :x-label x-lab
                          :y-label y-lab
                          :title title
                          :series-label series-lab
                          :legend legend?) theme))))


(defmacro function-plot
  "
  Returns a xy-plot object of the given function over the range indicated
  by the min-range and max-range arguments. Use the 'view' function to
  display the chart, or the 'save' function to write it to a file.
  Options:
    :title (default '') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
    :step-size (default (/ (- max-range min-range) 500))
  See also:
    view, save, add-points, add-lines
  Examples:
    (use '(incanter core stats charts))
    (view (function-plot sin (- Math/PI) Math/PI))
    (view (function-plot pdf-normal -3 3))
    (defn cubic [x] (+ (* x x x) (* 2 x x) (* 2 x) 3))
    (view (function-plot cubic -10 10))
  "
  ([function min-range max-range & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (format "%s < x < %s" '~min-range '~max-range))
           y-lab# (or (:y-label opts#) (str '~function))
           series-lab# (or (:series-label opts#) (format "%s" '~function))
           args# (concat [~function ~min-range ~max-range]
                         (apply concat (seq (apply assoc opts#
                                                   [:group-by group-by#
                                                    :title title#
                                                    :x-label x-lab#
                                                    :y-label y-lab#
                                                    :series-label series-lab#]))))]
       (apply function-plot* args#))))

(defn add-function*
  ([chart function min-range max-range & options]
    (let [opts (when options (apply assoc {} options))
           step-size (or (:step-size opts)
                         (float (/ (- max-range min-range) 500)))
           x (range-inclusive min-range max-range step-size)
           series-lab (or (:series-label opts)
                          (format "%s" 'function))]
       (add-lines chart x (map function x) :series-label series-lab))))


(defmacro add-function
  "
  Adds a xy-plot of the given function to the given chart, returning
  a modified version of the chart.
  Options:
    :series-label (default x expression)
    :step-size (default (/ (- max-range min-range) 500))
  See also:
    function-plot, view, save, add-function, add-points, add-lines
  Examples:
    (use '(incanter core stats charts))
    ;; plot the sine and cosine functions
    (doto (function-plot sin (- Math/PI) Math/PI)
          (add-function cos (- Math/PI) Math/PI)
          view)
    ;; plot two normal pdf functions
    (doto (function-plot pdf-normal -3 3 :legend true)
          (add-function (fn [x] (pdf-normal x :mean 0.5 :sd 0.5)) -3 3)
          view)
    ;; plot a user defined function and its derivative
    (use '(incanter core charts optimize))
    ;; define the function, x^3 + 2x^2 + 2x + 3
    (defn cubic [x] (+ (* x x x) (* 2 x x) (* 2 x) 3))
    ;; use the derivative function to get a function
    ;; that approximates its derivative
    (def deriv-cubic (derivative cubic))
    ;; plot the cubic function and its derivative
    (doto (function-plot cubic -10 10)
          (add-function deriv-cubic -10 10)
          view)
  "
  ([chart function min-range max-range & options]
    `(let [opts# ~(when options (apply assoc {} options))
           series-lab# (or (:series-label opts#) (str '~function))
           args# (concat [~chart ~function ~min-range ~max-range]
                         (apply concat (seq (apply assoc opts#
                                                   [:series-label series-lab#]))))]
        (apply add-function* args#))))

(defn log-axis
  "
  Create a logarithmic axis.
  Note: By default, values smaller than 0.5 are rounded to 0.5 to prevent strange behavior that
  happens for values close to 0.
  Options:
    :base (default 10) base of the logarithm; typically 2 or 10
    :label (default none) the label of the axis
    :int-ticks? (default true) if true, use normal numbers instead of
       <base>^<exponent>, i.e. 1 instead of f.ex. 10^0.0
    :smallest-value (default: 0.5) Set the smallest value represented by the axis, set to 0.0 to 'reset'
  See also:
    set-axis
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/axis/LogAxis.html
  "
  [& options]
  (let [opts (when options (apply assoc {} options))
        base (or (:base opts) 10)
        smallest-value (or (:smallest-value opts) 0.5)
        int-ticks? (get opts :int-ticks? true)
        label (:label opts)
        axis (if label
               (LogAxis. label)
               (LogAxis.))]
    (doto axis (.setBase base))
    (if int-ticks?
      (.setStandardTickUnits axis (NumberAxis/createIntegerTickUnits))) ;; must be after setting the base
    (if smallest-value
      (.setSmallestValue axis smallest-value)) ; TODO TEST THIS!
    axis))

(defmulti set-axis
  "
  Set the selected axis of the chart, returning the chart.
  (Beware: the axis' label will replace axis label set previously on the chart.)
  Arguments:
    chart - the JFreeChart object whose axis to change
    dimension - depends on the plot type for plots with mutliple axes
                 f.ex. :x or :y for an XYPlot (x is the domain axis, y the range one)
    axis - the axis to set, an instance of ValueAxis
  See also:
    log-axis
  Note:
    Not applicable to DialPlot MeterPlot PiePlot MultiplePiePlot CompassPlot WaferMapPlot SpiderWebPlot
  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/axis/ValueAxis.html
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/plot/XYPlot.html
  Examples:
    (use '(incanter core charts))
    (view
      (doto (function-plot #(Math/pow 10 %) 0 5)
            (set-axis :x (log-axis :base 10, :label \"log(x)\"))))
  "

  (fn [chart & args] (type (.getPlot chart))))

;;; Note: it would be nicer to use hierarchies to declare what plot types support
;;; the x and y axes but it feels as an overkill now
(defmethod set-axis :default
  ([chart axis] (throw (IllegalArgumentException. "Dimension (:x or :y) is required for XY-like charts")))
  ([chart dimension axis]
     {:pre [(#{:x :y} dimension)]}

     (let [plot (.getPlot chart)
           allowed-types #{org.jfree.chart.plot.XYPlot org.jfree.chart.plot.CategoryPlot org.jfree.chart.plot.ContourPlot org.jfree.chart.plot.FastScatterPlot}]
       (assert (allowed-types (type plot))
               (str "The default set-axis method only works for " allowed-types))
       (if (= :x dimension)
         (.setDomainAxis plot axis)
         (.setRangeAxis plot axis)))
     chart))

(defmethod set-axis org.jfree.chart.plot.PolarPlot
  ([chart axis]
     (let [plot (.getPlot chart)]
       (.setAxis plot axis))
     chart))

(defmethod set-axis org.jfree.chart.plot.ThermometerPlot
  ([chart axis]
     (let [plot (.getPlot chart)]
       (.setRangeAxis plot axis))
     chart))
