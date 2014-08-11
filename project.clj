(defproject chart-utils "1.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.jfree/jfreechart "1.0.15"
                  :exclusions [com.lowagie/itext
                               xml-apis]]
                 [org.jzy3d/jzy3d-api "0.9.1"]]
  :repositories [["jzy3d" "http://www.jzy3d.org/maven/releases"]])
