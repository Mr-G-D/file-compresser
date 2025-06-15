(ns compress
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; COMPRESSION METHODS 

(defn load-frequency-map
  "Reads frequency.txt, splits on whitespace, and builds a map
   of cleaned words to their first appearance rank.
   Only first occurrence of a cleaned word is counted."
  [file-name]
  (let [content (slurp file-name)
        words (str/split content #"\s+")]
    (loop [remaining words
           rank 0
           seen #{}
           result {}]
      (if (empty? remaining)
        result
        (let [raw (first remaining)
              word (-> raw
                       str/lower-case
                       (str/replace #"[^a-z]" ""))]
          (if (or (empty? word) (contains? seen word))
            (recur (rest remaining) rank seen result)
            (recur (rest remaining)
                   (inc rank)
                   (conj seen word)
                   (assoc result word rank))))))))

(defn get-rank
  ;"Given a frequency map and a word, return its rank if present; otherwise return the word itself."
  [freq-map word]
  (let [cleaned (-> word
                    clojure.string/lower-case
                    (clojure.string/replace #"[^a-z]" ""))]
    (if-let [rank (get freq-map cleaned)]
      (do
        (println (str "Found in map: \"" word "\" -> " rank))
        rank)
      (do
        (println (str "Not found in map: \"" word "\" -> returning original"))
        word))))


(defn read-and-get-words
  "Reads a file, prints its contents, and returns a vector of words."
  [file-name]
  (let [file (io/file file-name)]
    (cond
      (not (.exists file))
      (do
        (println (str "Error: File \"" file-name "\" does not exist."))
        [])

      (not (.isFile file))
      (do
        (println (str "Error: \"" file-name "\" is not a valid file."))
        [])

      :else
      (let [content (slurp file-name)
            words (str/split content #"\s+")]

        (vec words)))))  ; return vector of words


;; (defn main [input-file]
;;   (let [freq-map (load-frequency-map "frequency.txt")
;;         output-file (str input-file ".ct")
;;         words (read-and-get-words input-file)
;;         ranks (map #(get-rank freq-map %) words)
;;         paragraph (str/join " " ranks)]
;;     (println "\nCompressed Paragraph:\n")
;;     (println paragraph)
    
;;     (spit output-file paragraph)

;;     (println "\nCompressed paragraph written to:" output-file)

;;     ) ; also return it


;; DECOMPRESSION METHODS


(defn invert-map [m]
  (into {} (map (fn [[k v]] [v k]) m)))

(defn decompress-file
  [compressed-file freq-map]
  (try
    (let [content (slurp compressed-file)
          codes (str/split content #"\s+")
          ;; Reverse map: rank (string) -> word
          rev-map (into {} (map (fn [[w r]] [(str r) w]) freq-map))
          decompressed-words
            (map (fn [code]
                   (if (re-matches #"\d+" code)          ;; if code is numeric
                     (get rev-map code code)              ;; map number to word, else code itself
                     code))                              ;; if not numeric, just print as is
                 codes)
          paragraph (str/join " " decompressed-words)]
      (println "\nDecompressed content:\n" paragraph)
      paragraph)
    (catch java.io.FileNotFoundException _
      (println "Error: File not found -" compressed-file))
    (catch Exception e
      (println "Error reading file:" (.getMessage e)))))