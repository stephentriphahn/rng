(ns stevetrip.rng.core
  "Pure, serialisable randomness. The rng is plain data you can keep in application state, log, or send over the wire.

  Two forms:
    {:seed s :n counter}  each value is a function of seed and counter (splitmix64),
                          so the same seed and call sequence always replay identically.
    {:queue [6 1 4]}      scripted dice for tests. `roll-die` pops the head and throws
                          when empty. `shuffle` leaves the collection unchanged and does
                          not consume the queue, so tests set deck order directly.

  Every function returns the new rng alongside its result. Nothing is mutated."
  (:refer-clojure :exclude [shuffle]))

(def ^:private gamma (.longValue (BigInteger. "9E3779B97F4A7C15" 16)))
(def ^:private mul1 (.longValue (BigInteger. "BF58476D1CE4E5B9" 16)))
(def ^:private mul2 (.longValue (BigInteger. "94D049BB133111EB" 16)))

(defn- mix
  "splitmix64 output function: the n-th value of the stream for seed."
  ^long [^long seed ^long n]
  (let [z (unchecked-add seed (unchecked-multiply gamma (unchecked-inc n)))
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 30)) mul1)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 27)) mul2)]
    (bit-xor z (unsigned-bit-shift-right z 31))))

(def max-safe-seed
  "2^53 - 1, JavaScript's Number.MAX_SAFE_INTEGER.

  Seeds this engine GENERATES stay at or below it so that they survive a JSON
  round trip through a browser exactly (slice 31). `mix` above happily takes
  any long and a hand-passed one still may be any long - this bounds only what
  we hand out."
  9007199254740991)

(defn random-seed
  "A fresh, unpredictable seed - the same fallback state/new-game already
  uses when no :seed is given, extracted here so a caller that wants to
  *know* what seed it's using (rather than leave it implicit - see
  game_log.clj's start-game!) can generate one the same way.

  **Bounded to max-safe-seed, and that is a correctness fix rather than
  tidiness (slice 31).** This used to be a bare `.nextLong`, i.e. the full
  64-bit range. JSON has one number type and JavaScript parses it as a double,
  exact only to 2^53 - so a seed like 8256368015606065355 reached the browser
  as 8256368015606065000, and a player who asked to replay it got a DIFFERENT
  GAME. Silent, and it defeated the entire point of reporting the seed.

  Found by a suspicious literal in a UI test, not by the test itself: the
  literal truncated identically, so the assertion passed while proving
  nothing.

  2^53 is still 9 quadrillion games, and a seed's job is to be reproducible
  rather than unguessable."
  []
  (.nextLong (java.util.concurrent.ThreadLocalRandom/current) 0 (inc max-safe-seed)))

(defn seeded
  "A seeded rng starting at counter 0."
  [seed]
  {:seed (long seed) :n 0})

(defn scripted
  "A scripted rng that will roll the given values in order."
  [values]
  {:queue (vec values)})

(defn scripted? [rng]
  (contains? rng :queue))

(defn- next-below
  "Returns [k rng'] with k in [0, bound) for a seeded rng."
  [{:keys [seed n] :as rng} bound]
  [(Long/remainderUnsigned (mix seed n) (long bound))
   (assoc rng :n (inc n))])

(defn- pop-scripted
  "Returns [head rng'] for a scripted rng, throwing when the queue is empty."
  [rng]
  (let [[head & more] (:queue rng)]
    (when-not (seq (:queue rng))
      (throw (ex-info "Scripted rng queue is empty" {:rng rng})))
    [head (assoc rng :queue (vec more))]))

(defn next-int
  "Returns [k rng'] with k uniform in [0, bound). bound must be a positive long.
  A scripted rng pops and returns the next queued value as is."
  [rng bound]
  (when-not (pos? bound)
    (throw (ex-info "bound must be positive" {:bound bound})))
  (if (scripted? rng)
    (pop-scripted rng)
    (next-below rng bound)))

(defn roll
  "Returns [value rng'] for one die with the given number of sides, value in
  1..sides. A scripted rng pops and returns the next queued value as is."
  [rng sides]
  (when-not (pos? sides)
    (throw (ex-info "sides must be positive" {:sides sides})))
  (if (scripted? rng)
    (pop-scripted rng)
    (let [[k rng'] (next-below rng sides)]
      [(inc k) rng'])))

(defn roll-die
  "Returns [value rng'] with value in 1..6."
  [rng]
  (roll rng 6))

(defn choose
  "Returns [element rng'] for a uniformly chosen element of coll. Throws on an
  empty collection. A scripted rng pops the next queued value and uses it as a
  0-based index into coll."
  [rng coll]
  (let [v (vec coll)]
    (when (empty? v)
      (throw (ex-info "Cannot choose from an empty collection" {})))
    (let [[k rng'] (next-int rng (count v))]
      [(nth v k) rng'])))

(defn roll-dice
  "Returns [values rng'] for n dice, rolled in order."
  [rng n]
  (reduce (fn [[vs r] _]
            (let [[v r'] (roll-die r)]
              [(conj vs v) r']))
          [[] rng]
          (range n)))

(defn shuffle
  "Returns [shuffled-vector rng'] (Fisher-Yates). A scripted rng returns the
  collection as a vector, unchanged, and is not consumed."
  [rng coll]
  (if (scripted? rng)
    [(vec coll) rng]
    (loop [v (vec coll), i (dec (count coll)), r rng]
      (if (pos? i)
        (let [[j r'] (next-below r (inc i))]
          (recur (assoc v i (nth v j) j (nth v i)) (dec i) r'))
        [v r]))))

(comment
  ;; The rng is plain data. Every function returns [result new-rng], so you thread
  ;; the new rng into the next call yourself. Nothing is mutated.

  ;; A seeded rng starts at counter 0.
  (def r0 (seeded 42))
  r0                                  ; => {:seed 42, :n 0}

  ;; Roll one die. The counter advances by one.
  (roll-die r0)                       ; => [<1..6> {:seed 42, :n 1}]

  ;; Rolling again from the *same* rng gives the *same* die: the value depends only
  ;; on seed and counter.
  (= (roll-die r0) (roll-die r0))     ; => true

  ;; To keep rolling, pass the returned rng along.
  (let [[a r1] (roll-die r0)
        [b r2] (roll-die r1)]
    [a b r2])                         ; => [<die> <die> {:seed 42, :n 2}]

  ;; Several dice at once, e.g. an attack with power 4.
  (roll-dice r0 4)                    ; => [[<d> <d> <d> <d>] {:seed 42, :n 4}]

  ;; A hit is a 4 or more by default, but the real threshold is per-defender
  ;; (Armor makes it 5), so hit determination lives in engine.clj's RollDice
  ;; where the threshold is a parameter - not here. This namespace only rolls.
  (roll-dice r0 4)

  ;; Different seeds give different streams.
  (first (roll-dice (seeded 1) 10))
  (first (roll-dice (seeded 2) 10))

  ;; Shuffle a collection. The result is a vector and the rng comes back advanced.
  (shuffle r0 [:a :b :c :d :e])       ; => [[...permutation...] {:seed 42, :n 4}]

  ;; Replay: the same seed and the same calls reproduce the same game.
  (= (shuffle (seeded 7) (range 10))
     (shuffle (seeded 7) (range 10))) ; => true

  ;; Sanity check the distribution over many rolls.
  (frequencies (first (roll-dice (seeded 1) 6000)))
  ;; => roughly 1000 of each face 1..6

  ;; Scripted rng for tests: you choose the dice. Values pop off the queue in order.
  (def s0 (scripted [6 1 4]))
  (roll-die s0)                       ; => [6 {:queue [1 4]}]
  (roll-dice s0 2)                    ; => [[6 1] {:queue [4]}]

  ;; Asking for more dice than were scripted throws, so a test can't silently
  ;; roll dice it never planned.
  (try (roll-dice s0 4)
       (catch clojure.lang.ExceptionInfo e (ex-message e)))
  ;; => "Scripted rng queue is empty"

  ;; A scripted rng does not shuffle: the order you give is the order you get.
  (shuffle s0 [:a :b :c])             ; => [[:a :b :c] {:queue [6 1 4]}]

  ;; In game state the rng sits at (:rng state). A rule never rolls; the engine does:
  ;;   (let [[dice rng'] (roll-dice (:rng st) power)]
  ;;     (-> st (assoc :rng rng') ...))
  #_())
