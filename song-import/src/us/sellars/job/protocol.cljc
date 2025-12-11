(ns us.sellars.job.protocol)

(defrecord Job [op-k data])

(defmulti as-Job vector?)

(defmethod as-Job false
  [job]
  (assert (instance? Job job))
  job)

(defmethod as-Job true
  [[op-k data :as job]]
  (with-meta (->Job op-k data)
    (meta job)))

(defprotocol JobQueue
  "A stateful job queue"
  (peek [this])
  (dequeue [this])
  (enqueue [this job] [this job priority] "Adds a job to be performed soon, returns job-id")
  (job-id [this job]))

(defprotocol JobContext
  (job-queue [this] "a JobQueue (optional)")
  (context [this] "a map of e.g. db connections")
  #_(job-id! [this job] "gets (adding if necessary) the id for the specified job as a `deref`able")
  #_(report-started [this job data])
  #_(report-success [this job data])
  #_(report-error [this job data e])
  (add-related-job [this job] "adds a job to be performed soon")
  (add-deferred-job [this job] "adds a job to be performed")
  (add-follow-up-job [this job] "adds a job to be performed later, when there isn't much else to do"))

(defprotocol JobExecutor
  (execute-1! [this] "executes a job from its source (e.g. queue)"))

(defmulti ExecuteJob
  (fn ExecuteJob_dispatch [_job-context ^Job job]
    (.op-k job)))
