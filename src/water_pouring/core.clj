(ns water-pouring.core
  (:require [water-pouring.pour-helper :refer :all]))

(defn update-state [volume-used-fn key state]
  (let [entry (state key)]
    (assoc state key
           (assoc entry :volume-used (volume-used-fn entry)))))

(defn update-by-entry [state-and-moves entry-update-fn type]
  (let [state (state-and-moves :state)
        moves (state-and-moves :moves)
        key-values (keys state)]
    (map
     (fn [key]
       (let [entry (state key)]
         {:state (update-state entry-update-fn key state)
          :moves (conj moves {type key})}))
     key-values)))

(defn empty [state]
  (update-by-entry state (fn [entry] 0) :empty))

(defn fill [state]
  (update-by-entry state (fn [entry] (entry :capacity)) :fill))

(defn pourings [{state :state moves :moves}]
  (let [pairs (pairwise-permute (keys state))]
    (map
     (fn [{from-key :1 to-key :2}]
       (let [from (state from-key)
             to (state to-key)
             available-water (from :volume-used)
             available-volume (- (to :capacity) (to :volume-used))
             delta (min available-water available-volume)]
         {:state (-> state
                     ((fn [state-obj]
                        (update-state (pour-from-fn delta) from-key state-obj)))
                     ((fn [state-obj]
                        (update-state (pour-to-fn delta) to-key state-obj))))
          :moves (conj moves {:pour
                              {:from from-key
                               :to to-key}})}))
     pairs)))

(defn map-to-next-state [states mapper-fn]
  (map
   (fn [state-entry]
     (mapper-fn state-entry))
   states))

(defn update-visited [visited-set to-be-added]
  (reduce
   (fn [accume entry]
     (conj accume (entry :state)))
   visited-set
   to-be-added))

(defn solves? [potential-solution goal]
  (not (empty?
        (filter
         (fn [key]
           (let [state (potential-solution :state)
                 entry (state key)]
             (= goal (entry :volume-used))))
         (keys state)))))

(defn get-solution [possible-solutions goal]
  (let [solution (take 1
                       (filter
                        (fn [possible-solution]
                          (solves? possible-solution goal))
                        possible-solutions))]
    {:found-solution (not (empty? solution))
     :solution solution}))

(defn iterating-solve [states-to-visit visited-states goal]
  (loop [states states-to-visit visited visited-states]
    (let [next-states (filter
                       (fn [current-state]
                         (not
                          (contains?
                           visited
                           (current-state :state))))
                       (flatten
                        (list
                         (map-to-next-state states fill)
                         (map-to-next-state states empty)
                         (map-to-next-state states pourings))))
          updated-visited (update-visited visited next-states)
          solution (get-solution next-states goal)]
      (cond
        (:found-solution solution) solution
        (empty? next-states) solution
        :else (recur next-states updated-visited)))))

(defn solve [state target]
  (let [states [{:state state
                 :moves []}]
        visited #{state}]
    (iterating-solve states visited target)))

;;Example

(def capacities (list 3 5 7))
(defn build-cup [capacity]
  {:capacity capacity
   :volume-used 0})
(def goal 4)
(def state (reduce
            (fn [accume capacity]
              (assoc accume capacity (build-cup capacity)))
            {}
            capacities))

;; (solve state goal)
