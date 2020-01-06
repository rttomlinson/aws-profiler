(ns aws-profiler.core
  (:require [com.rpl.specter :as s]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [cognitect.aws.client.api :as aws])
  (:import (org.apache.commons.compress.archivers.tar TarArchiveInputStream)
           (java.io File)
           (java.io FileOutputStream)
           (java.io FileInputStream))
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn copy-uri-to-file [uri file]
  (with-open [in (clojure.java.io/input-stream uri)
              out (clojure.java.io/output-stream file)]
    (clojure.java.io/copy in out)))

(defn untar-file
  "untar file and save to location - preserving filenames"
  [zipfile outdirpath]
  (let [buffer (byte-array 1024)
        outdir (File. outdirpath)]
    (.mkdir outdir)
    (println "untarring " (.getPath zipfile))
    (with-open [zis (TarArchiveInputStream. (FileInputStream. zipfile))]
      (loop [e (.getNextEntry zis)]
        (if e
          (let [filename (.getName e)
                outfile (File. (str outdir (File/separator) filename))]
            (if (.isDirectory e)
              (.mkdirs outfile)
              (do
                (.mkdirs (File. (.getParent outfile)))
                (with-open [outstream (FileOutputStream. outfile)]
                  (loop [len (.read zis buffer)]
                    (if (< 0 len)
                      (do
                        (.write outstream buffer 0 len)
                        (recur (.read zis buffer))))))))
            (recur (.getNextEntry zis))))))))


(defn download-unzip [url dir]
  (let [saveDir (File. dir)]
    (with-open [stream (-> (client/get url {:as :stream})
                           (:body)
                           (java.util.zip.ZipInputStream.))]
      (loop [entry (.getNextEntry stream)]
        (if entry
          (let [savePath (str dir File/separatorChar (.getName entry))
                saveFile (File. savePath)]
            (if (.isDirectory entry)
              (if-not (.exists saveFile)
                (.mkdirs saveFile))
              (let [parentDir (File. (.substring savePath 0 (.lastIndexOf savePath (int File/separatorChar))))]
                (if-not (.exists parentDir) (.mkdirs parentDir))
                (clojure.java.io/copy stream saveFile)))
            (recur (.getNextEntry stream))))))))

(defn get-all-normal-json-files
  [files]
  (filter #(clojure.string/includes? (last (clojure.string/split (str %) #"/")) "normal") files))

;; so amazon sbd or simpledb doesn't follow the convention of having uid
;; for this reason we might just pull the uid from the filename since those seem to also match up
;; will need to keep following since this is odd
;; There are some known exception. this is not a complete list: iot1click has projects/jobs-data/events-data/
;; Need a way to discover apis that don't fit the convention

(defn get-all-list-and-describe-operations
  "Returns a map with the endpoint prefix as the key and a vector of list and describe operations"
  [file]
  (do
    (println (nil? file))
    (println (str file))
    (let [service-json (json/read-str (slurp file))
          service-uid (first (clojure.string/split (last (clojure.string/split (str file) #"/")) #"-"))
          list-and-describe-operations (filter #(or (clojure.string/starts-with? % "Describe") (clojure.string/starts-with? % "List")) (s/select ["operations" s/MAP-KEYS] service-json))]
      (print "Full service name " (s/select-first ["metadata" "serviceFullName"] service-json))
      (println "HEY! " service-uid)
      (hash-map (keyword service-uid) list-and-describe-operations))))

(defn build-services-mapping
  [files]
  (if (= 0 (count files))
    (hash-map)
    (do
      (conj (get-all-list-and-describe-operations (first files)) (build-services-mapping (rest files))))))


(defn call-op-and-return-hash-map
  [aws-service-client service-name op]
  (let [service-op-response (aws/invoke aws-service-client {:op op})]
    (println (:cognitect.anomalies/category service-op-response))
    (println (keys service-op-response))
    (if (contains? service-op-response :cognitect.anomalies/category)
      (hash-map service-name (str "Something's not right for " op " for the " service-name " service"))     (hash-map service-name service-op-response))))


(defn call-all-operations-for-service
  "Tries to call all the operations under the service endpoint prefix. service-name should be a keyword to work with the aws-api library"
  [service-name ops]
  (do
    (try
      (aws/client {:api service-name})
      (let [aws-service-client (aws/client {:api service-name})]
        ;; for each ops invoke it with the client))
        (doall
         (map #(call-op-and-return-hash-map aws-service-client service-name (keyword %)) ops))) 
      (catch Exception e
        (println "exception caught " (.getMessage e))
        (str "catch exception: " (.getMessage e))))))


(defn get-endpoint-prefixes
  []
  (inc 1))

(defn my-value-reader
  [key value]
  (do
    (println (type value)) 
    (cond
      (= (type value) java.util.Date)
      (.toString value)
      (= (type value) java.io.BufferedInputStream)
      (slurp value)
      :else value)))



(let [aws-sdk-js-repo-data (json/read-str (slurp "https://api.github.com/repos/aws/aws-sdk-js/releases/latest"))
      latest-zipball-release-url (s/select-first ["zipball_url"] aws-sdk-js-repo-data)]
  (do
    (println latest-zipball-release-url)
    (download-unzip latest-zipball-release-url "tempdir")
    (let [f (clojure.java.io/file "tempdir")
          fs (file-seq f) 
          ts-api-files (file-seq (File. (first (mapv str (filter #(and (.isDirectory %) (clojure.string/ends-with? (str %) "apis")) fs)))))
          ts-normal-api-files (get-all-normal-json-files ts-api-files)
          services-mapping (build-services-mapping ts-normal-api-files)
          first-ec2-json-file (first (filter #(and (clojure.string/includes? (last (clojure.string/split (str %) #"/")) "normal") (clojure.string/includes? (last (clojure.string/split (str %) #"/")) "ec2")) ts-api-files))]
      ;;(println (get-all-list-and-describe-operations first-ec2-json-file))
      ;;(spit "list-describe-ops.json" (json/write-str (build-services-mapping ts-normal-api-files)))

      (let [service-names (s/select s/MAP-KEYS services-mapping)]
        (spit "temp-ops-results.json" (json/write-str (list (map #(call-all-operations-for-service % (get services-mapping %)) service-names)) :value-fn my-value-reader))))))
        ;;(println ts-normal-api-files)
        ;; need to build clients from "endpoint-prefix"
      ;;   (let [aws-service-client (aws/client {:api :ec2})]
      ;;     (aws/ops aws-service-client)
      ;;     (println (aws/invoke aws-service-client {:op :DescribeInstances}))))
      ;; (let [aws-service-client (aws/client {:api :ecs})]
      ;;   (aws/invoke aws-service-client {:op :ListClusters})))))
    ;;get dir name of unzipped dir))









(defn get-latest-releases-deps
  "creates a compatible deps list for project.clj"
  []
  (let [aws-api-deps (clojure.edn/read-string (slurp "latest-releases.edn"))]
    (spit "latest-deps.txt" (list
                             (map #(vector % (get-in aws-api-deps [% :mvn/version])) (keys aws-api-deps))))))


;(get-latest-releases-deps)
