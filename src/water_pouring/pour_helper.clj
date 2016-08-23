(ns water-pouring.pour-helper)

(defn pairwise-permute [list]
  (flatten
   (map (fn [entry]
          (map
           (fn [second-entry]
             {:1 entry :2 second-entry})
           (filter
            (fn [value]
              (not (= value entry)))
            list)))
        list)))

(defn pour-from-fn [delta]
  (fn [entry]
    (- (entry :volume-used) delta)))

(defn pour-to-fn [delta]
  (fn [entry]
    (+ (entry :volume-used) delta)))


