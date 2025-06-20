(ns menu
    (:require
     [clojure.java.io :as io]
     [clojure.string :as str])
    (:require [compress]))
  ; this is where you would also include/require the compress module


; Display the menu and ask the user for the option
(defn showMenu
  []
  (println "\n\n*** Compression Menu ***")
  (println "------------------\n")
  (println "1. Display list of files")
  (println "2. Display file contents")
  (println "3. Compress a file")
  (println "4. Uncompress a file")
  (println "5. Exit")
  (do 
    (print "\nEnter an option? ") 
    (flush) 
    (read-line)))


; Display all files in the current folder
(defn option1 []
      (println "Listing all files in the current folder and subdirectories:")
      (doseq [file (file-seq (java.io.File. "."))]
             (when (.isFile file)  ; Only print files, not directories
                   (println (.getName file)))))
    
    
    
; Read and display the file contents (if the file exists). Java's File class can be used to 
; check for existence first. 
(defn option2 []
      (print "\nPlease enter the file name to display => ")
      (flush)
      (let [file-name (read-line)
            file (java.io.File. file-name)]
           (cond
             (empty? file-name)  ;; Check if file name is empty
             (println "Error: File name cannot be empty.")

             (not (.exists file))  ;; Check if file does not exist
             (println "Error: The file does not exist.")

             (not (.isFile file))  ;; Check if the path is not a regular file (could be a directory)
             (println "Error: The specified path is not a valid file.")

             :else  ;; If no errors, read and display the file contents
             (try
               (println "\nFile contents:")
               (println (slurp file-name))  ;; Read and display the contents of the file
               (catch Exception e
                 (println "Error: An issue occurred while reading the file."))))))





; Compress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your compression function
(defn option3 []
  (print "\nPlease enter a .txt file to compress => ")
  (flush)
  (let [file-name (str/trim (read-line))]
    (cond
      (or (nil? file-name) (str/blank? file-name))
      (println "Error: File name cannot be empty.")

      (not (str/ends-with? file-name ".txt"))
      (println "Error: Only .txt files can be compressed.")

      (not (.exists (io/file file-name)))
      (println (str "Error: File not found - " file-name))

      :else
      (let [freq-map (compress/load-frequency-map "frequency.txt")]
        (compress/compress-file file-name freq-map)))))


; Decompress the (valid) file provided by the user. You will replace the println expression with code 
; that calls your decompression function
(defn option4 []
  (print "\nPlease enter a .ct file name => ")
  (flush)
  (let [file-name (str/trim (read-line))]
    (cond
      (or (nil? file-name) (str/blank? file-name))
      (println "Error: File name cannot be empty.")

      (not (str/ends-with? file-name ".txt.ct"))
      (println "Error: Only .txt.ct files are accepted for decompression.")

      (not (.exists (io/file file-name)))
      (println (str "Error: File not found - " file-name))

      :else
      (let [freq-map (compress/load-frequency-map "frequency.txt")]
        (compress/decompress-file file-name freq-map)))))


; If the menu selection is valid, call the relevant function to 
; process the selection
(defn processOption
  [option] ; other parm(s) can be provided here, if needed
  (if( = option "1")
     (option1)
     (if( = option "2")
        (option2)
        (if( = option "3")
           (option3)  ; other args(s) can be passed here, if needed
           (if( = option "4")
              (option4)   ; other args(s) can be passed here, if needed
              (println "Invalid Option, please try again"))))))


; Display the menu and get a menu item selection. Process the
; selection and then loop again to get the next menu selection
(defn menu
  [] ; parm(s) can be provided here, if needed
  (let [option (str/trim (showMenu))]
    (if (= option "5")
      (println "\nGood Bye\n")
      (do 
         (processOption option)
         (recur )))))   ; other args(s) can be passed here, if needed




; ------------------------------
; Run the program. You might want to prepare the data required for the mapping operations
; before you display the menu. You don't have to do this but it might make some things easier

(menu) ; other args(s) can be passed here, if needed
