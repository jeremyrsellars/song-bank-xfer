(ns us.sellars.job.impl.mem
  (:import [clojure.lang Counted PersistentQueue])
  (:require [us.sellars.job.protocol :as job-proto]))

(deftype MemJobQueue [state-atom idx-atom]
  job-proto/JobQueue
  (peek [_this]
    (->> @state-atom vals (some #(some identity %))))
  (dequeue [_this]
    (let [v! (volatile! nil)]
      (swap!
        state-atom
        (fn find-first-job-and-remove-it [st]
          (loop [kvs (seq st)]
            (if-let [[priority jobs] (first kvs)]
              (if-let [job (peek jobs)]
                (do (vreset! v! job)
                    (update st priority pop))
                (recur (next kvs)))
              st))))
      @v!))
  (enqueue [this job]
    (.enqueue this job 1))
  (enqueue
    [_this job priority]
    (let [job-id (count (swap! idx-atom #(assoc % job (inc (count %)))))]
      (swap!
        state-atom
        (fn enq [st]
          (let [q (or (get st priority)
                      PersistentQueue/EMPTY)]
            (assoc st priority (conj q job)))))
      job-id))
  (job-id [_this job]
    (-> @idx-atom (get job)))
  Counted
  (count [_this]
    (apply + 0 (map count (vals @state-atom)))))

(defn empty-queue
  []
  (MemJobQueue. (atom (sorted-map))
                (atom {})))

#_
(doto (empty-queue)
  (job-proto/enqueue 1 2)
  (job-proto/enqueue 2 1)
  (job-proto/enqueue 3 2)
  (job-proto/enqueue 4 0)
  (-> job-proto/peek println)
  (->> (println 'dequeue))
  (-> job-proto/dequeue println)
  (-> job-proto/dequeue println)
  (-> job-proto/dequeue println)
  (-> job-proto/dequeue println)
  (-> job-proto/dequeue println)
  (-> job-proto/dequeue println)
  (-> job-proto/dequeue println)
  (-> job-proto/dequeue println)
  (-> count println))

(defrecord SimpleJobContext [job-queue
                             context
                             execute!         ; (fn [job-ctx job] result)
                             post-process!    ; (fn [job-ctx job result] result)
                             result-ref
                             errors-ref]
  job-proto/JobContext
  (job-queue [_this] job-queue)
  (context [_this] context)
  #_(job-id! [_this job] "gets (adding if necessary) the id for the specified job as a `deref`able")
  #_(report-started [_this job data])
  #_(report-success [_this job data])
  #_(report-error [_this job data e])
  (add-related-job [_this job]
    (job-proto/enqueue job-queue (job-proto/as-Job job) -1))
  (add-deferred-job [_this job]
    (job-proto/enqueue job-queue (job-proto/as-Job job) 0))
  (add-follow-up-job [_this job]
    (job-proto/enqueue job-queue (job-proto/as-Job job) 1))
  job-proto/JobExecutor
  (execute-1! [this]
    (when-let [job (job-proto/dequeue job-queue)]
      (let [job-id (job-proto/job-id job-queue job)]
        (try
          (swap! result-ref assoc job-id (post-process! this job (execute! this job)))
          (catch #?(:cljs js/Error :clj Exception) e
            (swap! errors-ref conj e)
            (swap! result-ref assoc job-id (ex-data e))))
        job-id))))

(defn- fixup-job
  [job-or-vec parent-job-id]
  (as-> (job-proto/as-Job job-or-vec) job
    (cond-> job
      parent-job-id (with-meta (assoc (meta job) :parent-job-id parent-job-id)))))

(defn data-oriented-post-process!
  [job-ctx job result]
  (let [parent-job-id (some-> job-ctx job-proto/job-queue (job-proto/job-id job))]
    (doseq [j (-> result meta :jobs/related)]
      (job-proto/add-related-job job-ctx (fixup-job j parent-job-id)))
    (doseq [j (-> result meta :jobs/deferred)]
      (job-proto/add-deferred-job job-ctx (fixup-job j parent-job-id)))
    (doseq [j (-> result meta :jobs/follow-up)]
      (job-proto/add-follow-up-job job-ctx (fixup-job j parent-job-id))))
  (-> result
      (with-meta (some-> result meta (dissoc :jobs/related :jobs/deferred :jobs/follow-up)))))

(defn simple-job-context
  [{:keys [job-queue
           context
           execute!         ; (fn [job-ctx job] result)
           post-process!]    ; (fn [job-ctx job result] result)
    :or   {job-queue     (empty-queue)
           context       {}
           execute!      #'job-proto/ExecuteJob
           post-process! data-oriented-post-process!}}]
  (map->SimpleJobContext
    {:job-queue job-queue
     :context context
     :execute! execute!
     :post-process! post-process!
     :result-ref (atom (sorted-map))
     :errors-ref (atom [])}))
