(ns the-divine-cheese-code.visualization.svg
  (:require [clojure.string :as s])
  (:refer-clojure :exclude [min max]))


(defn comparator-over-maps
  [comparison-fn ks]
  (fn [maps]
    (zipmap ks
            (map (fn [k] (apply comparison-fn (map k maps)))
                 ks))))


(def min (comparator-over-maps clojure.core/min [:lat :lng]))

(def max (comparator-over-maps clojure.core/max [:lat :lng]))


(defn translate-to-00
  [locations]
  (let [mincoords (min locations)]
    (map #(merge-with - % mincoords) locations)))


(defn scale
  [width height locations]
  (let [maxcoords (max locations)
    ratio {:lat (/ height (:lat maxcoords))
           :lng (/ width (:lng maxcoords))}]
    (map #(merge-with * % ratio) locations)))


(defn latlng->point
  "Convert latitude/longitude map to comma-separated string"
  [latlng]
  (str (:lat latlng) "," (:lng latlng)))


(defn points
  [locations]
  (s/join " " (map latlng->point locations)))


(defn line
  [points]
  (str "<polyline points=\"" points "\" />"))


(defn transform
  "Just chains other functions"
  [width height locations]
  (->> locations
       translate-to-00
       (scale width height)))


(defn xml
  "svg 'template', which also flips the coordinate system"
  [width height locations]
  (str "<svg height=\"" height "\" width=\"" width "\">"
       ;; These two '<g>' tags flip the coordinate system to put (0,0) in the
       ;; lower-lefthand corner instead of the upper-lefthand corner, which is
       ;; SVG's default
       "<g transform=\"translate(0," height ")\">"
       "<g transform=\"rotate(-90)\">"
       (-> (transform width height locations)
           points
           line)
       "</g></g>"
       "</svg>"))
