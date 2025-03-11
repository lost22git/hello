#!/usr/bin/env bb

(import '[java.io File IOException])
(import '[java.nio.file NoSuchFileException])

(require '[babashka.cli :as cli])

(set! *warn-on-reflection* true)

; === handle-err ===

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

; === get-content ===

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

; === cli ===

(def cli-spec
  {:spec
   [[:help {:alias :h
            :desc "Print help messages"}]]
   :error-fn
   (fn [{:keys [spec type cause msg option] :as data}]
     (if (= :org.babashka/cli type)
       (case cause
         :require (println
                   (format "Missing required argument:\n%s"
                           (cli/format-opts {:spec (select-keys spec [option])})))
         (println msg))
       (throw (ex-info msg data)))
     (System/exit 1))})

(defn- format-cli-opts
  "Format cli opts"
  []
; (cli/format-opts cli-spec)
  (cli/format-table
   {:rows (concat [["Alias" "Option" "Description"]]
                  (cli/opts->table cli-spec))
    :indent 2}))

(defn- print-help
  "Print help messages"
  []
  (->
   "
Print content of given path 

\u001b[1;4mUsage:\u001b[m bb-demo [OPTIONS] [PATH]

\u001b[1;4mArguments:\u001b[m
  [PATH] Path to print [Default: .]

\u001b[1;4mOptions:\u001b[m
%s
  "
   (format (format-cli-opts))
   println))

(comment
  (print-help))

; === Main ===

(defn- run
  [{:keys [args opts]}]
  (if (:help opts)
    (print-help)
    (try
      (let [path (or (first args) ".")
            real-path (fs/real-path path)
            _ (println (format "Content of %s:" real-path))
            file (fs/file real-path)
            content (get-content file)]
        (println content))
      (catch Exception e
        (handle-err e)))))

(defn -main [args]
  (run (cli/parse-args args cli-spec)))

(when (= *file* (System/getProperty "babashka.file"))
  (-main *command-line-args*))
