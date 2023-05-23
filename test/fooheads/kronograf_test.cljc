(ns fooheads.kronograf-test
  (:require
    [clojure.test :refer [deftest is]]
    [fooheads.kronograf :as krono]))


(def ts (atom 0))
(defn set-ts [n] (reset! ts n))
(defn now [] @ts)


(def k (krono/create! now -))

(identity @k)


(deftest scenario-1
  (let [k (krono/create! now -)]
    ;; First lap
    (set-ts 2)
    (krono/start-lap! k)
    (set-ts 10)
    (krono/checkpoint! k "1")
    (set-ts 14)
    (krono/checkpoint! k "2")
    (set-ts 20)
    (krono/checkpoint! k "3")
    (set-ts 28)
    (krono/finish-lap! k)

    ;; Second lap
    (set-ts 40)
    (krono/start-lap! k)
    (set-ts 44)
    (krono/checkpoint! k "1")
    (set-ts 47)
    (krono/checkpoint! k "2")
    (set-ts 50)
    (krono/checkpoint! k "3")
    (set-ts 57)
    (krono/finish-lap! k)

    (is (= [{:label ["1" "2"] :sum 7}
            {:label ["2" "3"] :sum 9}]
           (krono/simple-stats @k)))))


