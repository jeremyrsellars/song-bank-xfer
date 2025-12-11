(ns us.sellars.song-transfer.elvanto.api
  (:require [clojure.data.json :as json]
            [org.httpkit.client :as http]))

(def options
  {:basic-auth [(System/getenv "ELVANTO_API_KEY") "password-not-used-with-api-key"]
   :headers {"Content-Type" "application/json; charset=UTF-8"
             "Accept"       "application/json; charset=UTF-8"}})

(def requests
  {:songs/get-page {:url "https://api.elvanto.com/v1/songs/getAll.json"
                    :request-body {:page_size 10 :page 1}
                    :parse (fn parse-page-of-songs [body] (get-in body ["songs" "song"]))
                    :response-shape ["generated_in"
                                     "status"
                                     {"songs"
                                      ["on_this_page"
                                       "page"
                                       "per_page"
                                       "total"
                                       {"song"
                                        ["album"
                                         "permalink"
                                         "learn"
                                         "allow_downloads"
                                         "id"
                                         "categories"
                                         "date_modified"
                                         "status"
                                         "item"
                                         "number"
                                         "title"
                                         "date_added"
                                         "notes"
                                         "artist"]}]}]}
   :songs/get-info {:url "https://api.elvanto.com/v1/songs/getInfo.json"
                    :request-body {:id "b0cd5f34-a2c1-11e2-9444-fd58bfb6527a" :files false}
                    :parse (fn parse-page-of-songs [body] (get-in body ["song" 0]))
                    :response-shape ["generated_in"
                                     "status"
                                     {"song"
                                      ["album"
                                       "permalink"
                                       "learn"
                                       "allow_downloads"
                                       "id"
                                       {"files" [{"file" ["id" "title" "type" "html" "content"]}]}
                                       {"categories" [{"category" ["id" "name"]}]}
                                       "date_modified"
                                       "status"
                                       "item"
                                       "number"
                                       "title"
                                       "date_added"
                                       "notes"
                                       "artist"]}]}
   :songs/get-arrangements {:url "https://api.elvanto.com/v1/songs/arrangements/getAll.json"
                            :request-body {:song_id "b0cd5f34-a2c1-11e2-9444-fd58bfb6527a" :files false}
                            :parse (fn parse-page-of-arrangements [body] (get-in body ["arrangements" "arrangement"]))
                            :response-shape ["generated_in"
                                             "status"
                                             {"arrangements"
                                              ["on_this_page"
                                               "page"
                                               "per_page"
                                               "total"
                                               {"arrangement"
                                                ["key_female"
                                                 "id"
                                                 "chord_chart_key"
                                                 {"files" [{"file" ["id" "title" "type" "html" "content"]}]}
                                                 "date_modified"
                                                 "chord_chart"
                                                 "bpm"
                                                 "lyrics"
                                                 "name"
                                                 "sequence"
                                                 "minutes"
                                                 "seconds"
                                                 "copyright"
                                                 "date_added"
                                                 "key_male"]}]}]}
   :song.arrangement/get-keys {:url "https://api.elvanto.com/v1/songs/keys/getAll.json"
                               :request-body {:arrangement_id "b0cdceb0-a2c1-11e2-9444-fd58bfb6527a" :files false}
                               :parse (fn parse-page-of-keys [body] (get-in body ["keys" "key"]))
                               :response-shape ["generated_in"
                                                "status"
                                                {"keys"
                                                 ["on_this_page"
                                                  "page"
                                                  "per_page"
                                                  "total"
                                                  {"key" ["id"
                                                          "date_added"
                                                          "date_modified"
                                                          "name"
                                                          "key_starting"
                                                          "key_ending"
                                                          "arrangement_id"
                                                          {"files" [{"file" ["id" "title" "type" "html" "content"]}]}]}]}]}})

(defn pathom-style-attrs
  [m]
  (reduce-kv (fn [attrs k v]
               (conj attrs
                     (cond (map? v)
                           {k (pathom-style-attrs v)}
                           (and (vector? v) (map? (first v)))
                           {k (pathom-style-attrs (first v))}
                           :else k)))
             [] m))

(defn post
  ([req-k body]
   (let [req (get requests req-k)]
     (assert req (str "Missing req for " req-k))
     (post (update req :request-body merge body))))
  ([req]
   (let [{:keys [parse url request-body]} req
         resp @(http/post url
                          (assoc options :body (json/json-str request-body)))
         _ (def resp resp)
         json (-> resp :body)
         raw (-> json json/read-str)
         parsed (parse raw)
         parsed (cond-> parsed (coll? parsed) (with-meta {:json json, :raw raw}))]
     parsed)))



#_(def page-of-songs (post :songs/get-page {}))
#_(def song-with-files (post :songs/get-info {:id "b0cd5f34-a2c1-11e2-9444-fd58bfb6527a" :files true}))
#_(-> song-with-files meta :raw pathom-style-attrs)
#_(def arrangements-with-files (post :songs/get-arrangements {:song_id "b0cd5f34-a2c1-11e2-9444-fd58bfb6527a" :files true}))
#_(-> arrangements-with-files meta :raw pathom-style-attrs)
#_(def keys-with-files (post :song.arrangement/get-keys {:song_id "b0cd5f34-a2c1-11e2-9444-fd58bfb6527a" :files true}))
#_(-> keys-with-files meta :raw pathom-style-attrs)
#_ (-> @resp :body json/read-str)
