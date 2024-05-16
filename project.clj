(defproject org.clojars.smee/chart-utils "1.2.3"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.jfree/jfreechart "1.0.19"
                  :exclusions [com.lowagie/itext
                               xml-apis]]
                 [org.jzy3d/jzy3d-api "1.0.2"]]
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"}
  :repositories [["jzy3d" "https://maven.jzy3d.org/releases"]])
