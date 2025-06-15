(ns compress
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; FREQUENCY MAP GENERATOR

(defn load-frequency-map
  ;"Reads frequency.txt, splits on whitespace, and builds a map
   ;of cleaned words to their first appearance rank.
   ;Only first occurrence of a cleaned word is counted."
  [file-name]
  (let [content (slurp file-name)
        ;; Split paragraph into words using whitespace
        words (str/split content #"\s+")]
    ;; Loop through words, building the map
    (loop [remaining words
           rank 0
           word-map {}]
      (if (empty? remaining)
        word-map
        (let [word (first remaining)]
          (if (contains? word-map word)
            (recur (rest remaining) rank word-map)
            (recur (rest remaining) (inc rank) (assoc word-map word rank))))))))

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


;; COMPRESSION METHODS 

(defn tokenize [text]
  ;"Splits text into words and symbols, preserving punctuation as separate tokens."
  (->> (re-seq #"\w+|[^\s\w]" text) ; matches words or punctuation
       (map str)))

(defn compress-file [input-file freq-map]
  (try
    (let [output-file (str input-file ".ct")
          content (slurp input-file)
          ;; Tokenize into words + punctuation
          tokens (re-seq #"\w+|[^\s\w]" content) ; keeps punctuation separate
          compressed (map (fn [token]
                            (cond
                              (re-matches #"\d+" token) (str "@" token "@") ; number
                              (contains? freq-map (clojure.string/lower-case token))
                              (str (get freq-map (clojure.string/lower-case token))) ; known word
                              :else token)) ; punctuation or unknown
                          tokens)
          result (clojure.string/join " " compressed)]
      (spit output-file result)
      (println (str "\nCompression complete. Output written to: " output-file)))
    (catch Exception e
      (println "Unexpected error during compression:" (.getMessage e)))))

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


;; Helper to trim leading spaces
(defn ltrim [s]
  (str/replace s #"^\s+" ""))

(defn format-decompressed-text [text]
  (let [;; 1️⃣ Collapse spaces between closing brackets and punctuation
        text (str/replace text #"([\]\)]) *([.,?!])" "$1$2")


        ;; 3️⃣ Ensure space after punctuation if not end of string
        text (str/replace text #"([.,?!\]])(?=\S)" "$1 ")

        ;; 4️⃣ Add space before opening brackets unless at start
        text (str/replace text #"(?<!^)\s*([\(\[])" " $1")
        ;; Remove space after opening brackets
        text (str/replace text #"([\(\[]) +" "$1")

        ;; 5️⃣ Remove space before closing brackets, ensure space after if not end
        text (str/replace text #" *([\)\]])" "$1")
        text (str/replace text #"([\)\]])(?=\S)" "$1 ")

        ;; 6️⃣ Normalize dashes: space before and after
        text (str/replace text #" *- *" " - ")

        ;; 7️⃣ Handle @ and $: space before, no space after
        text (str/replace text #" *([@$]) *" " $1")
        text (str/replace text #"([@$]) +" "$1")

        ;; 2️⃣ Remove spaces before punctuation marks (.,?!])
        text (str/replace text #" *([.,?!\]])" "$1")

        ;; 8️⃣ Normalize multiple spaces
        text (str/replace text #"\s+" " ")

        ;; 9️⃣ Full trim + left trim to eliminate any starting space
        
        ;; 🔟 Capitalize first letter of every sentence
        sentences (str/split text #"(?<=[.?!])\s+")
        sentences (map #(str (str/upper-case (subs % 0 1)) (subs % 1)) sentences)
        final-text (str/join " " sentences)
        final-text (-> final-text str/trim ltrim)]
    final-text))

(defn decompress-file
  [compressed-file freq-map]
  (try
    (let [content (slurp compressed-file)
          codes (str/split content #"\s+")
          ;; Reverse map: rank (string) -> word
          rev-map (into {} (map (fn [[w r]] [(str r) w]) freq-map))
          ;; Helper to process each token according to your rules
          processed-codes
          (map (fn [code]
                 (cond
                   ;; If code is enclosed in '@' and length > 2, strip '@' and return number as string
                   (and (.startsWith code "@")
                        (.endsWith code "@")
                        (> (count code) 2))
                   (subs code 1 (dec (count code)))

                   ;; If code is enclosed in '@' but too short, return as-is (invalid token)
                   (and (.startsWith code "@")
                        (.endsWith code "@"))
                   code

                   ;; If purely numeric string, map rank to word or keep code if no mapping found
                   (re-matches #"^\d+$" code)
                   (get rev-map code code)

                   ;; Otherwise, return token as-is (punctuation, words not compressed, etc.)
                   :else
                   code))
               codes)
          paragraph (str/join " " processed-codes)
          ;; Now apply your formatting function here (you need to define format-decompressed-text)
          formatted-paragraph (format-decompressed-text paragraph)]
      (println (str "\nDecompressed content:\n" formatted-paragraph))
      formatted-paragraph)
    (catch java.io.FileNotFoundException _
      (println "Error: File not found -" compressed-file))
    (catch Exception e
      (println "Error reading file:" (.getMessage e)))))