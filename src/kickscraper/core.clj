(ns kickscraper.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json])
  (:gen-class))

(def ^:dynamic *sleepy-req?* false)

(defn maybe-sleepy-get
  [& args]
  (let [r (apply client/get args)]
    (when *sleepy-req?*
      (Thread/sleep 1000))
    r))

(defn get-projects-by-page-num
  [n]
  (select-keys (json/parse-string
                (:body
                 (maybe-sleepy-get (str "https://www.kickstarter.com/discover/advanced?google_chrome_workaround&category_id=51&woe_id=0&sort=end_date&seed=2485394&page=" n)
                             {:headers {"Accept" "application/json"}}))
                keyword)
               [:projects :has_more]))

(defn get-project-urls
  [& [max-pages]]
  (loop [page 1
         urls []]
    (let [{:keys [projects has_more]} (get-projects-by-page-num page)
          urls' (into urls
                      (map #(get-in % [:urls :web :project])
                           projects))]
      (if (and has_more
               (or (nil? max-pages)
                   (<= page max-pages)))
        (recur (inc page) urls')
        urls'))))

(def u1 "https://www.kickstarter.com/projects/166793082/extra-cleaning-on-demand?ref=category_ending_soon")

(defn project-url->id
  [url]
  (let [[_ profile-id proj-name] (re-find #"projects/(\d+)/([^?]+)" url)]
    (when (and profile-id proj-name)
      (format "%s__%s"
              proj-name
              profile-id))))

(defn id->html-filename
  [id]
  (str "./resources/html/" id))

(defn id->data0-filename
  [id]
  (str "./resources/data0/" id))

(defn file-exists?
  [filename]
  (-> filename
      clojure.java.io/as-file
      .exists))

(defn request-html
  [url]
  (println "DOWNLOADING! " url)
  (:body (maybe-sleepy-get url)))

(defn write-html-to-file
  [id html]
  (spit (id->html-filename id)
        html)
  html)

#_(defn fetch-html-for-id
  [id]
  (or (-> id
          id->html-filename
          read-file-if-exists)
      (->> id
           get-html-for-id
           (write-html-to-file id))))

(defn read-rsrc
  [filename]
  (when (-> filename
            clojure.java.io/as-file
            .exists)
    (println "SLURPING! " filename)
    (slurp filename)))

(defn read-html-rsrc
  [id]
  (read-rsrc (id->html-filename id)))

(defn write-html-rsrc
  [id html]
  (spit (id->html-filename id)
        html)
  html)

(defn html-rscr-exists?
  [id]
  (file-exists? (id->html-filename id)))

(defn extract-prop
  [prop h]
  (second (re-find (re-pattern (str prop "=\"([^\"]*)\""))
                   h)))

(defn extract-prop-double
  [prop h]
  (try
    (Double/parseDouble (extract-prop prop h))
    (catch Exception e nil)))

(defn get-all-html-rsrc-ids
  []
  (mapv #(.getName %)
        (filter #(.isFile %)
                (file-seq (clojure.java.io/file "./resources/html")))))

(defn parse-html-title [h] (second (re-find #"<title>([^>]*)</title>" h)))

(def parse-html-pledged (partial extract-prop-double "data-pledged"))
(def parse-html-backers-count (partial extract-prop-double "data-backers-count"))

(defn parse-html-pledge-amounts
  [h]
  (mapv (comp #(Double/parseDouble %) second)
        (re-seq #"Pledge <span class=\"money\">.(\d+)<"
                h)))

(defn parse-html-backers
  [h]
  (mapv (comp #(Double/parseDouble %) second)
        (re-seq #"(\d+) backers"
                h)))

(defn parse-html-pledge-amt-backers
  [h]
  (mapv #(vector % %2 (* % %2))
        (parse-html-pledge-amounts h)
        (parse-html-backers h)))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn covariance
  [xs ys]
  (let [x-bar (mean xs)
        y-bar (mean ys)
        dx (map (fn [x] (- x x-bar)) xs)
        dy (map (fn [y] (- y y-bar)) ys)]
    (mean (map * dx dy))))


(defn standard-deviation [coll]
  (let [avg (mean coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (if-not (zero? total)
      (-> (/ (apply + squares)
             total)
          (Math/sqrt))
      0.0)))

(defn correlation
  [x y]
  (/ (covariance x y)
     (* (standard-deviation x)
        (standard-deviation y))))



(defn safe-div-double
  [a b]
  (try
    (double (/ a b))
    (catch Exception e nil)))

(defn get-stats
  [v]
  (when (not-empty v)
    (let [v' (vec (sort v))
          c (count v)
          t (apply + v)
          avg (safe-div-double t c)
          median (get v' (quot c 2))
          p90 (get v' (quot (* c 9) 10))
          mx (apply max v)
          mn (apply min v)]
      {:count c
       :sum t
       :avg avg
       :median median
       :p90 p90
       :max mx
       :min mn
       :std-dev (standard-deviation v)})))


#_
(parse-html-pledge-amt-backers (slurp "./resources/html/learn-5-best-mobile-development-frameworks__1311831077"))

(defn html->data0
  [h]
  (let [pledge-amt-backers (parse-html-pledge-amt-backers h)
        hi-pledged (last (sort-by last pledge-amt-backers))
        hi-backed (last (sort-by second pledge-amt-backers))
        pledged (extract-prop-double "data-pledged" h)
        goal (extract-prop-double "data-goal" h)]
    {:title (parse-html-title h)
     :pledged pledged
     :backers-count (extract-prop-double "data-backers-count" h)
     :goal goal
     :success? (if ((fnil >= 0 0) pledged goal) 1 0)
     :duration (extract-prop-double "data-duration" h)
     :end-time (extract-prop-double "data-end_time" h)
     :pab pledge-amt-backers
     :pa-stats (get-stats (map first pledge-amt-backers))
     :b-stats (get-stats (map second pledge-amt-backers))
     :hi-pleged hi-pledged
     :hi-backed hi-backed}))

#_(html->data0 (slurp "./resources/html/learn-5-best-mobile-development-frameworks__1311831077"))


(defn write-data0-rsrc
  [id d]
  (spit (id->data0-filename id)
        (pr-str d))
  d)

(defn read-data0-rsrc
  [id]
  (read-string (read-rsrc (id->data0-filename id))))

(defn read-all-data0-rsrcs
  []
  (mapv (comp read-string slurp)
        (filter #(.isFile %)
                (file-seq (clojure.java.io/file "./resources/data0")))))

(defn data0-rsrc-exists?
  [id]
  (file-exists? (id->data0-filename id)))

(defn do-html->data0
  [id]
  (or (data0-rsrc-exists? id)
      (write-data0-rsrc id
                        (html->data0 (read-html-rsrc id)))))

(defn do-all-html->data0
  []
  (doseq [h (get-all-html-rsrc-ids)]
    (do-html->data0 h)))

(defn mk-all-data1
  [id]
  (throw (Exception. "NOT IMPLEMENTED")))

(defn correlate-keys
  [ks1 ks2 d]
  (def ks11 ks1)
  (def ks21 ks2)
  (def d1 d)
  (let [ks1-fn #(get-in % ks1)
        ks2-fn #(get-in % ks2)
        d' (filter (every-pred ks1-fn ks2-fn) d)]
    (if (not-empty d')
      (try
        (correlation (map ks1-fn d')
                     (map ks2-fn d'))
        (catch Exception e
          (println "FAIL:" ks2)
          0.0))
      0.0)))

(def cc-props
  [[:goal]
   [:backers-count]
   [:duration]
   [:pa-stats :count]
   [:pa-stats :max]
   [:pa-stats :min]
   [:pa-stats :avg]
   [:pa-stats :median]
   [:pa-stats :p90]
   [:pa-stats :std-dev]
   [:b-stats :count]
   [:b-stats :max]
   [:b-stats :min]
   [:b-stats :avg]
   [:b-stats :median]
   [:b-stats :p90]
   [:b-stats :std-dev]])

(defn analyze-data0
  [d0]
  (clojure.pprint/pprint 
   (sort-by second
            (for [k cc-props]
              [k (correlate-keys [:pledged] k d0)]))))

(defn update-html-rsrcs
  [& [pages]]
  (binding [*sleepy-req?* true]
    (doseq [url (get-project-urls pages)]
      (let [id (project-url->id url)]
        (or (html-rscr-exists? id)
            (write-html-rsrc id
                             (request-html url)))))))

#_ (update-html-rsrcs 360)

(defn do-analysis
  []
  (do-all-html->data0)
  (analyze-data0 (read-all-data0-rsrcs)))

#_ (do-analysis)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
