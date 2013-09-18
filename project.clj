(defproject chart-utils "1.0.2-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.jfree/jfreechart "1.0.15"
                  :exclusions [com.lowagie/itext
                               xml-apis]]])
