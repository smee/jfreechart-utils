(ns chart-utils.3dplot
  (:import org.jzy3d.chart.factories.AWTChartComponentFactory
           org.jzy3d.colors.Color
           org.jzy3d.maths.Coord3d
           org.jzy3d.plot3d.primitives.Scatter
           org.jzy3d.plot3d.rendering.canvas.Quality
           org.jzy3d.chart.ChartLauncher))


(defn create-3d-scatter-plot [points colors {:keys [size x-label y-label z-label]
                                             :or {size 1.0
                                                  x-label "X"
                                                  y-label "Y"
                                                  z-label "Z"}}]
  (let [n (count points)
        coords (make-array Coord3d n)
        cols (make-array Color n)]
    (loop [points points colors colors idx 0]
      (if points
        (let [[x y z] (first points)
              [r g b a] (first colors)]
          (aset coords idx (Coord3d. x y z))
          (aset cols idx (Color. r g b a))
          (recur (next points) (next colors) (inc idx)))
        (doto (AWTChartComponentFactory/chart Quality/Advanced "newt")
          (.. getAxeLayout (setXAxeLabel x-label))
          (.. getAxeLayout (setYAxeLabel y-label))
          (.. getAxeLayout (setZAxeLabel z-label))
          (.. getScene (add (doto (Scatter. coords cols)
                              (.setWidth (float size))))))))))


(defn view [chart]
  (let [rect (org.jzy3d.maths.Rectangle. 200 200 800 600)]
    (ChartLauncher/instructions)
    (ChartLauncher/openChart chart rect "Scatter")))
(comment
  (view (create-3d-scatter-plot [[0.0 0.0 0.0]
                                 [1.0 1.0 1.0]
                                 [5.0 5.0 5.0]]
                                [[255 0 0 255]
                                 [0 255 0 255]
                                 [0 0 255 255]]
                                {:size 8.0}))
  )
