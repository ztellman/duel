;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns duel.core
  (:use
    [potemkin])
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.java.shell :as sh])
  (:import
    [java.io
     PipedOutputStream
     PipedInputStream
     InputStream
     OutputStream]))
  
;;;

(defprotocol-once Player
  (send-command- [_ command-args])
  (terminate [_]))

(defn send-command [p & command-args]
  (send-command- p command-args))

;;; GTP

(defn- parse-error [s]
  (second (re-find #"^?\S*\s(.*)" s)))

(defn- parse-response [s]
  (second (re-find #"=\S*\s(.*)" s)))

(defn- parse-request [s]
  (when-let [[id req] (rest (re-find #"(\d*)\s?(.*)" s))]
    (let [[cmd & args] (str/split (str/lower-case req) #"[ \t]")]
      (list* id (keyword cmd) args))))

(defn create-player
  "Given input and output streams, creating a Player that sends and receives commands."
  [in out close-fn]
  (let [responses (delay (atom (->> in io/reader line-seq (remove empty?))))
        str* #(if (keyword? %) (name %) (str %))]
    (reify Player
      (send-command- [_ command-args]

        (let [command (->> command-args (map str*) (interpose " ") (apply str))]

          ;; send command
          (.write out (.getBytes (str command "\r\n")))
          (.flush out)
          
          ;; wait for response
          (if (empty? @@responses)
            (throw (Exception. "no response"))
            (let [s (first @@responses)]
              (swap! @responses rest)
              (if-let [rsp (parse-response s)]
                rsp
                (throw (Exception. (or (parse-error s) (str "Cannot parse '" s "'")))))))))
      
      (terminate [_]
        (when close-fn
          (close-fn))))))

(defn- stream-pair []
  (let [out (PipedOutputStream.)
        in (PipedInputStream. out)]
    [in out]))

(defn create-external-player
  "Create a player by starting an external process."
  [& cmds]
  (let [^Process p (.exec (Runtime/getRuntime) (->> cmds (map str) into-array))
        is (.getInputStream p)
        os (.getOutputStream p)]
    (try
      (create-player is os #(.destroy p))
      (catch Exception e
        (throw (Exception. (str "closed with exit value of " (.exitValue p))))))))

(defn- wrap-internal-player [handler in out]
  (future
    (doseq [[id & req] (->> in io/reader line-seq (map parse-request))]
      (try
        (.write out (.getBytes (str "=" id " " (handler req) "\r\n")))
        (catch Exception e
          (.printStackTrace e)
          (.write out (.getBytes (str "?" id " " (.getMessage e) "\r\n")))))
      (.flush out))))

(defn create-internal-player
  "Create a player via a command handler function."
  [handler]
  (let [[in-a out-a] (stream-pair)
        [in-b out-b] (stream-pair)]
    (wrap-internal-player handler
      in-b
      out-a)
    (create-player
      in-a
      out-b
      #(do
         (.close out-a)
         (.close out-b)))))

;;;

(defn gnugo-player
  [& {:as options
      :keys [level
             monte-carlo]
      :or {level 10
           monte-carlo false}}]
  (apply create-external-player "gnugo"
    "--mode" "gtp"
    "--level" level
    (when monte-carlo
      ["--monte-carlo"])))

(defmacro with-gnugo [[g] & body]
  `(let [~g (gnugo-player)]
     (try
       ~@body
       (finally
         (terminate ~g)))))

;;;

(defn initialize-players
  [{:keys [board-size
           handicap]
    :or {board-size 9}}
   & players]
  (doseq [p players]
    (send-command p :boardsize board-size)
    (send-command p :clear_board)
    (when handicap
      (send-command p :fixed_handicap handicap))))

(defn initialize-board [p moves options]
  (initialize-players options p)
  (doseq [[b w] (partition-all 2 moves)]
    (send-command p :play "b" b)
    (when w
      (send-command p :play "w" w))))

;;;

(defn final-score
  [moves options]
  (with-gnugo [g]
    (initialize-board g moves options)
    (let [result (send-command g :final_score)
          [winner score] (->> result
                           str/lower-case
                           (re-find #"(\w?)\+([\d.]+)")
                           rest)]
      [(case winner
         "w" :white
         "b" :black
         :tie)
       (when score
         (read-string score))])))

;;

(defn playout
  "Plays an entire game."
  [black white
   {:as options
    :keys [arbiter]}]

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
                       (final-score @moves options)
                       (if (even? (count @moves))
                         [:black 0]
                         [:white 0]))]

    ;; game loop
    (loop [curr black, next white]
      (let [col (color curr)
            move (str/lower-case (send-command curr :genmove col))]
        (prn col move)
        (when-not (= "resign" move)
          (send-command next :play col move))
        (swap! moves conj move)
        (when-not (done?)
          (recur next curr))))

    (let [[winner score] (final-score)]
      {:moves @moves
       :winner winner
       :score score})))

(defn group-playouts [playouts]
  {:games playouts
   :score (->> playouts (map :winner) frequencies)})

(defn run-trials
  [black-generator
   white-generator
   & {:keys [num-trials
             concurrency]
      :or {concurrency 1
           num-trials 1}
      :as options}]
  (let [sub-trials (/ num-trials concurrency)
        players (repeatedly concurrency #(vector (black-generator) (white-generator)))]
    (try
      (->> players
        (map (fn [[black white]]
               (Thread/sleep (rand-int 10))
               (->> (range sub-trials)
                 (map (fn [_] (playout black white options)))
                 doall
                 future)))
        doall
        (map deref)
        (apply concat)
        group-playouts)
      (finally
        (->> players
          (apply concat)
          (map terminate)
          doall)))))
