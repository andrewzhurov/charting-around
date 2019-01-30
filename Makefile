figwheel:
	clj -Sdeps "{:deps {com.bhauman/figwheel-main {:mvn/version \"0.1.9\"}}}}"  -m figwheel.main -c charting-around.core --repl

dev_css:
	clj -R:dev-css -e "(require 'clojure-watch.core) (require 'charting-around.css) (clojure-watch.core/start-watch [{:path \"./src/charting_around/\" :callback (fn [event filename] (println \"Recompiling, because changed:\" event filename) (garden.core/css {:output-to \"css.css\"} charting-around.css/styles)) :event-types [:create :modify :delete] :recursive false}])"

dev_autocompile:
	clj -m cljs.main -w src -c charting-around.core
dev_serve:
	clj -m cljs.main -s
autotest:
	clj -Sdeps "{:deps {healthsamurai/matcho {:mvn/version \"0.3.2\"} com.jakemccrary/lein-test-refresh {:mvn/version \"0.23.0\"}}}" -C:test -e "(require 'com.jakemccrary.test-refresh) (com.jakemccrary.test-refresh/monitor-project [\"test\" \"src\"] {:watch-dirs [\"src\" \"test\"] :refresh-dirs [\"src\" \"test\"]})"

