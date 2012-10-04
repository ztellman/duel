;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns duel.gtp
  (:use
    [potemkin])
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.java.shell :as sh])
  (:import
    [java.io
     InputStream
     OutputStream]))

(defprotocol-once Player
  (send-command- [_ command])
  (terminate [_]))

(defn parse-error [s]
  (second (re-find #"^?\S*\s(.*)" s)))

(defn parse-response [s]
  (second (re-find #"=\S*\s(.*)" s)))

(defn create-player [& cmds]
  (let [^Process p (.exec (Runtime/getRuntime) (->> cmds (map str) into-array))
        is (.getInputStream p)
        os (.getOutputStream p)
        responses (delay (atom (->> is io/reader line-seq (remove empty?))))]
    (reify Player
      (send-command- [_ command]

        ;; send command
        (.write os (.getBytes (str command "\r\n")))
        (.flush os)

        ;; wait for response
        (if (empty? @@responses)
          (throw (Exception. (str "closed with exit value of " (.exitValue p))))
          (let [s (first @@responses)]
            (swap! @responses rest)
            (if-let [rsp (parse-response s)]
              rsp
              (throw (Exception. (or (parse-error s) (str "Cannot parse '" s "'"))))))))
      
      (terminate [_]
        (.destroy p)))))

(defn send-command [p & commands]
  (send-command- p (->> commands (interpose " ") (apply str))))

(defn initialize-players
  [{:keys [board-size
           handicap]
    :or {board-size 9}}
   & players]
  (doseq [p players]
    (send-command p "boardsize" board-size)
    (send-command p "clear_board")
    (when handicap
      (send-command p "fixed_handicap" handicap))))

(defn initialize-board [p moves options]
  (initialize-players options p)
  (doseq [[b w] (partition-all 2 moves)]
    (send-command p "play" "b" b)
    (when w
      (send-command p "play" "w" w))))

(defn final-score
  ([p moves options]
     (locking p
       (initialize-board p moves options)
       (let [result (send-command p "final_score")
             [winner score] (->> result
                              str/lower-case
                              (re-find #"(\w?)\+([\d.]+)")
                              rest)]
         [(case winner
            "w" :white
            "b" :black
            :tie)
          (when score
            (read-string score))]))))

;;

(defn playout
  "Plays an entire game."
  [black white
   {:as options
    :keys [arbiter]
    :or {arbiter white}}]

  ;; set up players
  (initialize-players options black white)

  (let [moves (atom [])

        ;; is the game over?
        done? #(or
                 (= "resign" (last @moves))
                 (= ["pass" "pass"] (->> @moves (partition 2 1) last)))

        ;; what is the color of the current player?
        color #(condp = %
                 black "b"
                 white "w")

        ;; who won?
        final-score #(if-not (= "resign" (last @moves))
                       (final-score arbiter @moves options)
                       (if (even? (count @moves))
                         [:black 0]
                         [:white 0]))]

    ;; game loop
    (loop [curr black, next white]
      (let [col (color curr)
            move (str/lower-case (send-command curr "genmove" col))]
        (when-not (= "resign" move)
          (send-command next "play" col move))
        (swap! moves conj move)
        (when-not (done?)
          (recur next curr))))

    ;; return game information
    (let [[winner score] (final-score)]
      {:moves @moves
       :winner winner
       :score score})))
