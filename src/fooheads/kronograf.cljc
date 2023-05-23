(ns fooheads.kronograf)


(def example
  {:label "The run"
   :laps
   [{:label "Lap 1"
     :started 0
     :finished 20
     :checkpoints
     [{:label 1 :ts 2}
      {:label 2 :ts 12}
      {:label 3 :ts 18}]}
    {:label "Lap 2"
     :started 31
     :finished 49
     :checkpoints
     [{:label 1 :ts 33}
      {:label 2 :ts 37}
      {:label 3 :ts 48}]}]})


(defn- now
  [kronograf]
  ((:now-fn kronograf)))


(defn- diff [kronograf ts1 ts2]
  ((:diff-fn kronograf) ts1 ts2))


(defn laps
  [kronograf]
  (get-in kronograf [:data :laps]))


(defn lap-time [kronograf lap]
  (when (and (:started lap) (:finished lap))
    (diff kronograf (:finished lap) (:started lap))))


(defn lap-segments [kronograf lap]
  (->>
    (:checkpoints lap)
    (partition 2 1)
    (map
      (fn [[c1 c2]]
        {:label [(:label c1) (:label c2)] :delta (diff kronograf (:ts c2) (:ts c1))}))))



(defn- lap-index
  [kronograf]
  (when (seq (laps kronograf))
    (dec (count (laps kronograf)))))


(defn- update-current-lap
  [kronograf f & args]
  (update-in kronograf [:data :laps (lap-index kronograf)] #(apply f % args)))


(defn create
  [now-fn diff-fn]
  {:now-fn now-fn
   :diff-fn diff-fn
   :data {:laps nil}})


(defn started?
  [kronograf]
  (get-in kronograf [:data :laps]))


(defn start-lap
  [kronograf]
  (let [entry {:started (now kronograf) :checkpoints []}]
    (if-not (started? kronograf)
      (assoc-in kronograf [:data :laps] [entry])
      (update-in kronograf [:data :laps] conj entry))))


(defn finish-lap
  [kronograf]
  (update-current-lap kronograf assoc :finished (now kronograf)))


(defn checkpoint
  [kronograf label]
  (update-current-lap
    kronograf update :checkpoints conj {:label label :ts (now kronograf)}))


(defn simple-stats [kronograf]
  (->>
    (laps kronograf)
    (map #(lap-segments kronograf %))
    (apply map vector)
    (map (fn [xs]
           (let [sum (apply + (map :delta xs))
                 n (count xs)
                 avg (/ sum n 1.0)]
             {:label (:label (first xs)) :sum sum :avg avg})))))


(defn create!
  [now-fn diff-fn]
  (atom (create now-fn diff-fn)))


(defn start-lap!
  [kronograf-atom]
  (swap! kronograf-atom start-lap))


(defn finish-lap!
  [kronograf-atom]
  (swap! kronograf-atom finish-lap))


(defn checkpoint!
  [kronograf-atom label]
  (swap! kronograf-atom checkpoint label))


