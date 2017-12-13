(set-env!
  :source-paths #{"src/clj" "src/cljs"}
  :resource-paths #{"html"}

  :dependencies '[[org.clojure/clojure "1.8.0"]
  [org.clojure/clojurescript "1.9.473"]
  [adzerk/boot-cljs "1.7.228-2"]
  [pandeiro/boot-http "0.7.6"]
  [org.clojure/tools.nrepl "0.2.12"] ;;required in order to make boot-http works
  [adzerk/boot-reload "0.5.1"]
  [adzerk/boot-cljs-repl "0.3.3"]
  [com.cemerick/piggieback "0.2.1"]
  [org.clojars.magomimmo/domina "2.0.0-SNAPSHOT"]
  [weasel "0.7.0"]
  [hiccups "0.3.0"]
  [compojure "1.5.2"]
  [org.clojars.magomimmo/shoreleave-remote-ring "0.3.3"]
  [org.clojars.magomimmo/shoreleave-remote "0.3.1"]
  [javax.servlet/javax.servlet-api "3.1.0"]
  ])

(require '[adzerk.boot-cljs :refer [cljs]]
'[pandeiro.boot-http :refer [serve]]    ;; make serve task visible
'[adzerk.boot-reload :refer [reload]]
'[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]])


;; define dev task as composition of subtasks
(deftask dev
  "Launch Immediate Feedback Development Environment"
  []
  (comp
    (serve ;:handler 'modern-cljs.remotes/app
	   :dir "target"
	   :reload true)
    (watch)
    (reload)
    (cljs-repl) ;; before cljs task
    (cljs)
    (target :dir #{"target"})))
