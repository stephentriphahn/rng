(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'net.clojars.stephentriphahn/rng)
(def version "0.1.0")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_] (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/write-pom {:class-dir class-dir :lib lib :version version :basis basis
                  :src-dirs ["src"]
                  :scm {:url "https://github.com/stephentriphahn/rng"}
                  :pom-data [[:description "Pure, serialisable, replayable randomness: splitmix64 dice rolls and shuffles as plain data."]
                             [:licenses [:license
                                         [:name "Eclipse Public License 1.0"]
                                         [:url "https://www.eclipse.org/legal/epl-v10.html"]]]]})
    (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
    (b/jar {:class-dir class-dir :jar-file jar-file})
    (println "Built" jar-file)))

(defn install [_]
  (jar nil)
  (b/install {:basis (b/create-basis {:project "deps.edn"}) :lib lib :version version
              :jar-file jar-file :class-dir class-dir}))

;; Needs CLOJARS_USERNAME / CLOJARS_PASSWORD (a Clojars deploy token).
(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
