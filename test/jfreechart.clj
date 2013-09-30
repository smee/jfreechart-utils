(ns jfreechart
  (:use clojure.test
        chart-utils.jfreechart))

(deftest binning
  (are [min max width x lower upper] (is (= [lower upper] ((bin-fn min max width) x)))
       0 10   1 5.5 5 6
       0 10   3 7.1 6 9
       -100 100 25 0 0 25))