;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns duel.core
  (:require [duel.gtp :as gtp]))

(defn gnugo-player
  [& {:as options
      :keys [level
             monte-carlo]
      :or {level 10
           monte-carlo false}}]
  (apply gtp/create-player "gnugo"
    "--mode" "gtp"
    "--level" level
    (when monte-carlo
      ["--monte-carlo"])))

(defn run-trials
  [black-generator
   white-generator
   & {:keys [num-trials
             concurrency]
      :or {concurrency 1}
      :as options}]
  (let [sub-trials (/ num-trials concurrency)
        players (repeatedly concurrency #(vector (black-generator) (white-generator)))]
    (try
      (->> players
        (map (fn [[black white]]
               (Thread/sleep (rand-int 10))
               (->> (range sub-trials)
                 (map (fn [_] (gtp/playout black white options)))
                 doall
                 future)))
        doall
        (map deref)
        (apply concat)
        (map :winner)
        frequencies)
      (finally
        (->> players
          (apply concat)
          (map gtp/terminate)
          doall)))))
