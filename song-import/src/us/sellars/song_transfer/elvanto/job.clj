(ns us.sellars.song-transfer.elvanto.job
  (:import [us.sellars.job.protocol Job])
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [us.sellars.job.protocol :as job]
            [us.sellars.song-transfer.core :refer [xf]]
            [us.sellars.song-transfer.elvanto.api :as elvanto]))

(defn- path
  [p & more]
  (cond (string? p) (into [p] more)
        (nil? p) (into [] more)
        (vector? p) (into p more)))

(defn- pretty-key-name
  [k]
  (when (string? k)
    (-> k (str/replace "b" "-flat") (str/replace "#" "-sharp"))))

(defn- pretty-file-name
  [k]
  (when (string? k)
    (-> k (str/replace #"(?i)^[^- 0-9A-Z.+=]" "_"))))

(defmethod job/ExecuteJob :elvanto.job/songs execute-songs
  [_job-context ^Job job]
  (let [songs (elvanto/post :songs/get-page (.-data job))
        directory (or (get (.-data job) :directory) ".")
        xform (some->> (:xf (.data job)) (apply xf))
        songs (cond->> songs xform (sequence xform))]
    (vary-meta songs assoc
               :output-path (path directory "songs.json")
               :jobs/deferred (for [song songs]
                                [:elvanto.job/song {:song song
                                                    :directory directory}]))))

(defmethod job/ExecuteJob :elvanto.job/song execute-song
  [_job-context ^Job job]
  (let [{{:strs [id] {files "file"} "files"} :song :keys [directory]} (.-data job)
        song (elvanto/post :songs/get-info {:id id, :files true})
        {:strs [number permalink id]} song
        directory (path directory (str number "." permalink "." id))] ; <number>.<permalink>.<id>
    (vary-meta song assoc
               :output-path (path directory "song-info.json")
               :jobs/related [[:elvanto.job/song.categories {:directory directory, :song song}]
                              [:elvanto.job/song.arrangements {:directory directory, :song song}]]
               :jobs/follow-up (for [file files]
                                 [:elvanto.job/file
                                  {:directory   directory
                                   :file        file
                                   :song        song}]))))

(defmethod job/ExecuteJob :elvanto.job/song.categories execute-song-categories
  [_job-context ^Job job]
  (let [{{{categories "category"} "categories"} :song, :keys [directory]} (.-data job)
        category-names (for [{:strs [name]} categories] name)]
    (vary-meta categories assoc
               :output-path (path directory (str "category-" (str/join "." category-names) ".json")))))

(defmethod job/ExecuteJob :elvanto.job/song.arrangements execute-song-arrangements
  [_job-context ^Job job]
  (let [{{:strs [id] :as song} :song, :keys [directory]} (.-data job)
        arrangements (elvanto/post :songs/get-arrangements {:song_id id, :files true})]
    (vary-meta arrangements assoc
               :output-path (path directory "song-arrangements.json")
               :jobs/related (for [arrangement arrangements]
                               [:elvanto.job/song.arrangement
                                {:directory   directory
                                 :song        song
                                 :arrangement arrangement}]))))

(defmethod job/ExecuteJob :elvanto.job/song.arrangement execute-song-arrangement
  [_job-context ^Job job]
  (let [{{:strs [id], {files "file"} "files", a-name "name"} :arrangement, :keys [directory song arrangement]} (.-data job)
        directory [directory (str "arrangement-" a-name "." id)]
        keys (elvanto/post :song.arrangement/get-keys {:arrangement_id id :files true})]
    (vary-meta keys assoc
               :output-path (path directory "song-keys.json")
               :jobs/related (list*
                               [:elvanto.job/song.arrangement.chordpro
                                {:directory   directory
                                 :key         key
                                 :song        song
                                 :arrangement arrangement}]
                               [:elvanto.job/song.arrangement.lyrics
                                {:directory   directory
                                 :key         key
                                 :song        song
                                 :arrangement arrangement}]
                               [:elvanto.job/song.arrangement.sequence
                                {:directory   directory
                                 :key         key
                                 :song        song
                                 :arrangement arrangement}]
                              (for [key keys]
                                [:elvanto.job/song.arrangement.key
                                 {:directory   directory
                                  :key         key
                                  :song        song
                                  :arrangement arrangement}]))
               :jobs/follow-up (for [file files]
                                 [:elvanto.job/file
                                  {:directory   directory
                                   :file        file
                                   :song        song
                                   :arrangement arrangement}]))))

(defmethod job/ExecuteJob :elvanto.job/song.arrangement.chordpro execute-song-arrangement-chordpro
  [_job-context ^Job job]
  (let [{{:strs [id chord_chart chord_chart_key]} :arrangement, :keys [directory]} (.-data job)]
    (vary-meta {:file-content chord_chart} assoc
               :output-path (path directory (str "chords-" (pretty-key-name chord_chart_key) ".chordpro.txt")))))

(defmethod job/ExecuteJob :elvanto.job/song.arrangement.lyrics execute-song-arrangement-lyrics
  [_job-context ^Job job]
  (let [{{:strs [id lyrics]} :arrangement, :keys [directory]} (.-data job)]
    (vary-meta {:file-content lyrics} assoc
               :output-path (path directory "lyrics.txt"))))

(defmethod job/ExecuteJob :elvanto.job/song.arrangement.sequence execute-song-arrangement-sequence
  [_job-context ^Job job]
  (let [{{:strs [id sequence]} :arrangement, :keys [directory]} (.-data job)]
    (vary-meta {:file-content (str/join "\n" sequence)} assoc
               :output-path (path directory "sequence.txt"))))

(defmethod job/ExecuteJob :elvanto.job/song.arrangement.key execute-song-arrangement-key
  [_job-context ^Job job]
  (let [{{:strs [id key_starting] {files "file"} "files", k-name "name" :as s-key} :key, :keys [directory song arrangement]} (.-data job)
        directory [directory (str "key-" (pretty-key-name key_starting) (when (seq k-name) ".") k-name)]]
    (vary-meta s-key assoc
               :output-path (path directory "song-arrangement-key.json")
               :jobs/follow-up (for [file files]
                                 [:elvanto.job/file
                                  {:directory   directory
                                   :file        file
                                   :song        song
                                   :arrangement arrangement}]))))

(defmethod job/ExecuteJob :elvanto.job/file execute-file
  [_job-context ^Job job]
  (let [{{:strs [id title type html content]} :file, :keys [directory]} (.-data job)
        html? (not= 0 html)
        content (or content "")
        extension (or (when html? ".html")
                      (re-find #"\.[^.]{1,9}$" content)
                      ".link.txt")
        file-content (cond html? content
                           (re-find #"^https?://" content) (delay (-> (http/get content)
                                                                      deref
                                                                      :body))
                           :else content)]
    (vary-meta {:file-content file-content} assoc
               :output-path (path directory (str type "-" (pretty-file-name title) "." id extension)))))
