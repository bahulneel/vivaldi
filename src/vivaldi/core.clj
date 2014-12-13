(ns vivaldi.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as mo]))

(defn height-vector
  [x y h]
  [[x y] h])

(defn sum
  [[v1 h1] [v2 h2]]
  [(mo/+ v1 v2) (+ h1 h2)])

(defn diff
  [[v1 h1] [v2 h2]]
  [(mo/- v1 v2) (+ h1 h2)])

(defn mag
  [[v h]]
  (+ h (m/length v)))

(defn scale
  [a [v h]]
  [(mo/* a v) (* a h)])

(defn unit
  [x]
  (scale (/ 1 (mag x)) x))

(defn rand-unit
  []
  (unit (height-vector (rand) (rand) (rand))))

(defn empty-env
  [cc ce]
  {:e nil
   :x (height-vector 0 0 0)
   :cc cc
   :ce ce})

(defn update
  [env rtt sample error]
  (let [{:keys [e x cc ce]} env
        dist (diff x sample)
        mag-dist (mag dist)
        e (if (nil? e) (- rtt mag-dist) e)
        w (/ e (+ e error))
        es (/ (Math/abs (- mag-dist rtt)) rtt)
        e (+ (* es ce w)
             (* e (- 1 (* ce w))))
        delta (* cc w)
        u (if (= mag-dist 0.0) (rand-unit) (unit dist))
        x (sum x (scale (* delta (- rtt mag-dist)) u))]
    (assoc env :x x :e e)))
