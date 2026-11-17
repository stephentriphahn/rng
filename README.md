# rng

Pure, serialisable, replayable randomness for Clojure. The rng is plain data
(`{:seed s :n counter}`), each value is a function of seed and counter
(splitmix64), and every function returns `[result new-rng]`. Nothing is mutated.

```clojure
(require '[stevetrip.rng.core :as rng])

(def r0 (rng/seeded 42))
(rng/roll-die r0)          ;=> [d {:seed 42, :n 1}]
(rng/roll r0 20)           ;=> [1..20 rng']
(rng/next-int r0 100)      ;=> [0..99 rng']
(rng/choose r0 [:a :b :c]) ;=> [:a|:b|:c rng']
(rng/roll-dice r0 4)       ;=> [[d d d d] {:seed 42, :n 4}]
(rng/shuffle r0 [:a :b :c]) ;=> [[...] rng']
(rng/random-seed)          ; fresh seed, <= 2^53-1 so it survives a JSON round trip

;; Scripted rng for tests: you choose the dice.
(rng/roll-dice (rng/scripted [6 1 4]) 2) ;=> [[6 1] {:queue [4]}]
```

Install: `net.clojars.stephentriphahn/rng {:mvn/version "0.1.0"}`

Test: `clj -X:test` / `clj -M:test`. Build: `clj -T:build jar`.
Licensed under the EPL-1.0.
