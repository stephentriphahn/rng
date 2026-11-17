(ns stevetrip.rng.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [stevetrip.rng.core :as rng]))

(deftest roll-die-test
  (testing "same seed and counter give the same die; the counter advances"
    (let [[a r1] (rng/roll-die (rng/seeded 7))
          [b _]  (rng/roll-die (rng/seeded 7))]
      (is (= a b))
      (is (= {:seed 7 :n 1} r1))))
  (testing "values stay in 1..6 and all faces show up"
    (let [vs (first (rng/roll-dice (rng/seeded 1) 600))]
      (is (every? #(<= 1 % 6) vs))
      (is (= #{1 2 3 4 5 6} (set vs)))
      (is (every? #(< 60 (count (filter #{%} vs)) 150) (range 1 7)))))
  (testing "different seeds give different streams"
    (is (not= (first (rng/roll-dice (rng/seeded 1) 20))
              (first (rng/roll-dice (rng/seeded 2) 20))))))

(deftest roll-dice-test
  (let [[vs r] (rng/roll-dice (rng/seeded 5) 4)
        [singles r'] (reduce (fn [[acc rn] _]
                               (let [[v rn'] (rng/roll-die rn)] [(conj acc v) rn']))
                             [[] (rng/seeded 5)] (range 4))]
    (is (= singles vs))
    (is (= r' r))
    (is (= 4 (:n r))))
  (is (= [[] (rng/seeded 5)] (rng/roll-dice (rng/seeded 5) 0))))

(deftest scripted-test
  (testing "pops the queue in order"
    (let [[vs r] (rng/roll-dice (rng/scripted [6 1 4]) 2)]
      (is (= [6 1] vs))
      (is (= {:queue [4]} r))))
  (testing "throws when empty"
    (is (thrown? clojure.lang.ExceptionInfo (rng/roll-die (rng/scripted []))))
    (is (thrown? clojure.lang.ExceptionInfo (rng/roll-dice (rng/scripted [3]) 2)))))

(deftest shuffle-test
  (let [coll (vec (range 20))
        [s r] (rng/shuffle (rng/seeded 3) coll)]
    (testing "a permutation, deterministic per seed, different across seeds"
      (is (= (sort coll) (sort s)))
      (is (not= coll s))
      (is (= [s r] (rng/shuffle (rng/seeded 3) coll)))
      (is (not= s (first (rng/shuffle (rng/seeded 4) coll)))))
    (testing "consumes the seeded rng"
      (is (= 19 (:n r))))
    (testing "scripted rng leaves order alone and is not consumed"
      (is (= [coll (rng/scripted [1 2])]
             (rng/shuffle (rng/scripted [1 2]) coll))))
    (testing "empty and singleton collections"
      (is (= [[] (rng/seeded 3)] (rng/shuffle (rng/seeded 3) [])))
      (is (= [[:a] (rng/seeded 3)] (rng/shuffle (rng/seeded 3) [:a]))))))

(deftest random-seed-survives-json-test
  ;; A seed is often reported to a browser so a run can be replayed, and JSON
  ;; has one number type that JavaScript reads as a double - exact only to 2^53.
  ;; A full 64-bit seed would arrive altered (8256368015606065355 ->
  ;; 8256368015606065000), and replaying it would silently give a different stream.
  (let [seeds (repeatedly 500 rng/random-seed)]
    (testing "every generated seed is inside JavaScript's exact-integer range"
      (is (every? #(<= 0 % rng/max-safe-seed) seeds)))
    (testing "max-safe-seed really is Number.MAX_SAFE_INTEGER"
      (is (= 9007199254740991 rng/max-safe-seed))
      (is (= rng/max-safe-seed (dec (long (Math/pow 2 53))))))
    (testing "and they are still seeds - distinct, and usable"
      (is (= 500 (count (distinct seeds))))
      (is (every? #(= {:seed % :n 0} (rng/seeded %)) (take 5 seeds))))
    (testing "a hand-passed seed is still allowed to be any long - only what
             random-seed hands out is bounded"
      (is (= {:seed 8256368015606065355 :n 0} (rng/seeded 8256368015606065355))))))

(deftest next-int-test
  (let [[ks r] (reduce (fn [[acc rn] _]
                         (let [[k rn'] (rng/next-int rn 10)] [(conj acc k) rn']))
                       [[] (rng/seeded 9)] (range 500))]
    (testing "in [0, bound), covers the range, advances the counter"
      (is (every? #(<= 0 % 9) ks))
      (is (= (set (range 10)) (set ks)))
      (is (= 500 (:n r))))
    (testing "deterministic"
      (is (= (rng/next-int (rng/seeded 9) 10) (rng/next-int (rng/seeded 9) 10)))))
  (testing "bound 1 always gives 0"
    (is (= 0 (first (rng/next-int (rng/seeded 1) 1)))))
  (testing "bad bounds throw"
    (is (thrown? clojure.lang.ExceptionInfo (rng/next-int (rng/seeded 1) 0)))
    (is (thrown? clojure.lang.ExceptionInfo (rng/next-int (rng/seeded 1) -3))))
  (testing "scripted pops the queue"
    (is (= [7 {:queue [2]}] (rng/next-int (rng/scripted [7 2]) 10)))
    (is (thrown? clojure.lang.ExceptionInfo (rng/next-int (rng/scripted []) 10)))))

(deftest roll-test
  (testing "N-sided dice stay in 1..N and hit every face"
    (let [vs (loop [r (rng/seeded 4), acc [], i 0]
               (if (= i 600) acc
                   (let [[v r'] (rng/roll r 20)] (recur r' (conj acc v) (inc i)))))]
      (is (every? #(<= 1 % 20) vs))
      (is (= (set (range 1 21)) (set vs)))))
  (testing "roll-die is a d6 roll"
    (is (= (rng/roll (rng/seeded 8) 6) (rng/roll-die (rng/seeded 8)))))
  (testing "one-sided die is always 1"
    (is (= 1 (first (rng/roll (rng/seeded 8) 1)))))
  (testing "bad sides throw"
    (is (thrown? clojure.lang.ExceptionInfo (rng/roll (rng/seeded 1) 0))))
  (testing "scripted"
    (is (= [20 {:queue [1]}] (rng/roll (rng/scripted [20 1]) 20)))))

(deftest choose-test
  (let [coll [:a :b :c :d]]
    (testing "returns members, eventually all of them, deterministically"
      (let [picks (loop [r (rng/seeded 2), acc [], i 0]
                    (if (= i 200) acc
                        (let [[x r'] (rng/choose r coll)] (recur r' (conj acc x) (inc i)))))]
        (is (= (set coll) (set picks))))
      (is (= (rng/choose (rng/seeded 2) coll) (rng/choose (rng/seeded 2) coll))))
    (testing "works on any seqable and a singleton"
      (is (= :x (first (rng/choose (rng/seeded 2) '(:x)))))
      (is (contains? #{0 1 2} (first (rng/choose (rng/seeded 2) (range 3))))))
    (testing "empty collection throws"
      (is (thrown? clojure.lang.ExceptionInfo (rng/choose (rng/seeded 2) []))))
    (testing "scripted value is a 0-based index"
      (is (= [:c {:queue []}] (rng/choose (rng/scripted [2]) coll))))))
