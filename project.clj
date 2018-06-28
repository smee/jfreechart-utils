(defproject chart-utils "1.2.2-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.jfree/jfreechart "1.5.0"
                  :exclusions [com.lowagie/itext
                               xml-apis]]
                 [org.jzy3d/jzy3d-api "1.0.2"]]
  :repositories [["jzy3d" "http://maven.jzy3d.org/releases"]])
