(ns us.sellars.song-transfer.elvanto
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [us.sellars.job.impl.mem]
            [us.sellars.job.protocol :as job-proto]
            [us.sellars.song-transfer.core :refer [xf]]
            [us.sellars.song-transfer.elvanto.job])) ; for multi-methods

(defn- write-file-content
  [filename content]
  (cond
    (string? content) (spit filename content :encoding "UTF8")
    (instance? java.io.InputStream content) (with-open [input content
                                                        output (java.io.FileOutputStream. filename)]
                                              (let [buffer (byte-array 1024)]
                                                (loop []
                                                  (let [bytes-read (.read input buffer)]
                                                    (when (pos? bytes-read)
                                                      (.write output buffer 0 bytes-read)
                                                      (recur))))))
    content (spit filename content)))

(defn- post-process!
  [job-ctx job result]
  (let [result (us.sellars.job.impl.mem/data-oriented-post-process! job-ctx job result)
        {:keys [output-path]} (meta result)]
    (when-let [filename (or (and (string? output-path) output-path)
                            (some->> output-path flatten seq (apply io/file)))]
      (io/make-parents filename)
      (or (when (and (map? result) (= [:file-content] (keys result)))
            (write-file-content filename (force (:file-content result)))
            :wrote/file-content)

          (when (coll? result)
            (with-open [writer (io/writer filename)]
              (json/write result writer {:indent true}))
            :wrote/json)

          (spit filename (pr-str result) :encoding "UTF8")
          :wrote/pr-str))
    result))

(defmethod xf :exclude-status
  [_ excluded-statuses]
  (let [ss (into #{} (cond-> excluded-statuses (not (seq? excluded-statuses)) (vector)))]
    (remove (fn [{:strs [status]}]
              (contains? ss status)))))

(defn do-all-jobs
  [& jobs]
  (let [context (us.sellars.job.impl.mem/simple-job-context
                  {:post-process! #'post-process!})]
    (doseq [job jobs]
      (job-proto/add-deferred-job context job))
    (loop []
      (when (-> (.-job-queue context) job-proto/peek)
        (job-proto/execute-1! context)
        (recur)))
    context))

(comment
  (def _all-ctx
    (do-all-jobs
      [:elvanto.job/songs {:xf [:exclude-status 0]
                           :directory ["dump" (str (str/replace (re-find #"[-0-9T:]+" (pr-str (java.util.Date.))) ":" "-") "Z")]
                           :page_size 1000}]))
  ,)

(comment
  (def context (us.sellars.job.impl.mem/simple-job-context
                {:post-process! #'post-process!}))

  (def root-job [:elvanto.job/songs {:xf [:exclude-status 0]
                                     :directory ["dump" (str (str/replace (re-find #"[-0-9T:]+" (pr-str (java.util.Date.))) ":" "-") "Z")]}])

  (job-proto/add-deferred-job context root-job)
  #p
  [(-> (.-job-queue context) job-proto/peek)
   ;@(.-result-ref context)
   @(.-errors-ref context)]

  #_ (job-proto/execute-1! context)
  #_ (repeatedly 13 #(job-proto/execute-1! context)))
