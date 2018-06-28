(ns chart-utils.3dplot
  (:import org.jzy3d.chart.factories.AWTChartComponentFactory
           [org.jzy3d.colors Color IColorMappable ColorMapper]
           org.jzy3d.maths.Coord3d
           [org.jzy3d.plot3d.primitives Scatter ScatterMultiColorList]
           org.jzy3d.plot3d.rendering.canvas.Quality
           org.jzy3d.chart.ChartLauncher))


(defn create-3d-scatter-plot [points {:keys [size x-label y-label z-label color-fn]
                                      :or {size 1.0
                                           x-label "X"
                                           y-label "Y"
                                           z-label "Z"}}]
  (let [n (count points)
        coords (make-array Coord3d n)
        cols (make-array Color n)
        [min-z max-z] (apply (juxt min max) (map last points))]
    (let [coords (map (fn [[x y z]] (Coord3d. ^double x ^double y ^double z)) points)
          cm (ColorMapper. (org.jzy3d.colors.colormaps.ColorMapHotCold.)
                           #_(reify org.jzy3d.colors.colormaps.IColorMap
                               (^Color getColor [_this ^IColorMappable im ^double x ^double y ^double z]
                                Color/RED)
                               (^Color getColor [_this ^IColorMappable im ^double z]
                                Color/BLUE))
                           min-z max-z)
          scatter (doto (ScatterMultiColorList. coords cm)
                    (.setWidth (float size)))
          chart (doto (AWTChartComponentFactory/chart Quality/Advanced "newt")
                  (.. getAxeLayout (setXAxeLabel x-label))
                  (.. getAxeLayout (setYAxeLabel y-label))
                  (.. getAxeLayout (setZAxeLabel z-label))
                  (.. getScene (add scatter)))
          legend (org.jzy3d.plot3d.rendering.legends.colorbars.AWTColorbarLegend. scatter (.. chart getView getAxe getLayout))]
      (.setLegend scatter legend)
      chart)))


(defn view [chart]
  (let [rect (org.jzy3d.maths.Rectangle. 200 200 800 600)]
    (ChartLauncher/instructions)
    (ChartLauncher/openChart chart rect "Scatter")))

(comment
  (view (create-3d-scatter-plot [[0.0 0.0 0.0]
                                 [1.0 1.0 1.0]
                                 [5.0 5.0 5.0]]
                                {:size 8.0}))
  )
