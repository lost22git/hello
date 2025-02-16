#!/usr/bin/env bb

(import '[java.io File IOException])
(import '[java.nio.file NoSuchFileException])

(set! *warn-on-reflection* true)

;;============
;; handle-err 
;;============

(defmulti handle-err type)
(defmethod handle-err NoSuchFileException
  [e]
  (println "File not exists")
  (System/exit 1))
(defmethod handle-err IOException
  [e]
  (println "IO error")
  (System/exit 1))
(defmethod handle-err :default
  [e]
  (println "Unknown error")
  (System/exit 1))

;;=============
;; get-content 
;;=============

(defmulti get-content (fn [^File file]
                        (case (fs/directory? file)
                          true :directory
                          false :file)))
(defmethod get-content :directory
  ^String [^File file]
  (->> file
       (.listFiles)
       (map str)
       (str/join \newline)))
(defmethod get-content :file
  ^String [^File file]
  (String. (fs/read-all-bytes file)))

;;======
;; Main 
;;======

; (println *command-line-args*)
(try
  (let [path (first *command-line-args*)
        real-path (fs/real-path path)
        _ (println (format "Content of %s:" real-path))
        file (fs/file real-path)
        content (get-content file)]

    (println content))
  (catch Exception e
    (handle-err e)))


