(ns kickscraper.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [com.hypirion.clj-xchart :as xc]
            [clojure-manifold.mds :as mds])
  (:gen-class))

(def ^:dynamic *sleepy-req?* false)

(defn maybe-sleepy-get
  [& args]
  (let [r (apply client/get args)]
    (when *sleepy-req?*
      (Thread/sleep 1000))
    r))

(def project-index-url "https://www.kickstarter.com/discover/advanced?google_chrome_workaround&category_id=51&woe_id=0&sort=end_date&seed=2485394&page=")

(def project-index-url "https://www.kickstarter.com/discover/advanced?google_chrome_workaround&category_id=51&woe_id=0&sort=most_funded&seed=2485394&page=")

(defn get-projects-by-page-num
  [n]
  (let [url (str project-index-url n)]
    (println "DOWNLOADING! " url)
    (select-keys (json/parse-string
                  (:body
                   (maybe-sleepy-get url
                                     {:headers {"Accept" "application/json"}}))
                  keyword)
                 [:projects :has_more])))

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
  (let [[_ profile-id proj-name] (re-find #"projects/([\d\w]+)/([^?]+)" url)]
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

(defn parse-html-pledged
  [h]
  (or (extract-prop-double "data-pledged" h)
      (Double/parseDouble (second (re-find #";pledged&quot;:([\d.]+),"
                                           h)))))

(defn parse-html-backers-count
  [h]
  (or (extract-prop-double "data-backers-count" h)
      (Integer/parseInt (clojure.string/replace (second (re-find #">([\d,]+) backers" h)) "," ""))))

(defn parse-html-goal
  [h]
  (try
    (or (extract-prop-double "data-goal" h)
        (Double/parseDouble (clojure.string/replace (second (re-find #"pledged of <span class=\"money\">\$([\d.,]+)</span>"
                                                                     h))
                                                    "," "")))
    (catch Exception e
      nil)))

(defn remove-commas
  [s]
  (clojure.string/replace s #"," ""))

(defn parse-html-pledge-amounts
  [h]
  (mapv (comp #(Double/parseDouble %) remove-commas second)
        (re-seq #"Pledge <span class=\"money\">.([\d.,]+)<"
                h)))

(defn parse-html-end-time
  [h]
  (or (extract-prop-double "data-end_time" h)
      (Long/parseLong (second (re-find #"deadline\&quot;:(\d+),"
                                          h)))))


(defn parse-html-backers
  [h]
  (def h1 h)
  (mapv (comp #(Double/parseDouble %) remove-commas second)
        (re-seq #"\n([\d,]+) backers"
                h)))

(defn parse-html-pledge-amt-backers
  [h]
  (mapv #(vector % %2 (* % %2))
        (parse-html-pledge-amounts h)
        (parse-html-backers h)))

(defn parse-html-duration
  [h]
  (def h1 h)
  (or (extract-prop-double "data-duration" h)
      (Integer/parseInt (second (re-find #"\((\d+)\sdays\)"
                                         h)))))

(defn parse-html-wordcount
  [h]
  (def h1 h)
  (count (re-seq #"\w+" (second
                         (re-find #"(?s)full-description(.+?)Report\sthis\sproject" h)))))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (try
    (nth (sort coll) (quot (count coll) 2))
    (catch Exception e
      -1)))

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

#_(get-stats [20 20 30 40 50 70 80 100 150 200 400 600 1000 2000 3000 5000])

(defn update-pab-percentiles
  [[amt] lo hi total m]
  (if-not (zero? total)
    (reduce (fn [agg x] (assoc agg (int x) amt))
            m
            (filter #(zero? (mod % 5))
                    (range (quot (* 100.0 lo) total)
                           (quot (* 100.0 hi) total))))
    m))

(defn pab-percentiles
  [pab]
  (let [total (apply + (map last pab))]
    (loop [[head & tail] pab
           x-lo 0
           x-hi 0
           agg {}]
      (if head
        (let [x-lo' x-hi
              x-hi' (+ x-lo (last head))]
          (recur tail
                 x-lo'
                 x-hi'
                 (update-pab-percentiles head
                                         x-lo'
                                         x-hi'
                                         total
                                         agg)))
        agg))))

#_
(parse-html-pledge-amt-backers (slurp "./resources/html/learn-5-best-mobile-development-frameworks__1311831077"))

(defn get-multiples
  [v]
  (mapv #(double (/ %2 (max % 0.1)))
        v
        (drop 1 v)))

(defn html->data0
  [h]
  (def h1 h)
  (let [pledge-amt-backers (parse-html-pledge-amt-backers h)
        hi-pledged (last (sort-by last pledge-amt-backers))
        hi-backed (last (sort-by second pledge-amt-backers))
        pledged (parse-html-pledged h)
        goal (parse-html-goal h)]
    {:title (parse-html-title h)
     :pledged pledged
     :backers-count (parse-html-backers-count h)
     :goal goal
     :success? ((fnil >= 0 0) pledged goal)
     :duration (parse-html-duration h)
     :end-time (parse-html-end-time h)
     :pab pledge-amt-backers
     :pab-pt (pab-percentiles pledge-amt-backers)
     :pa-stats (get-stats (map first pledge-amt-backers))
     :b-stats (get-stats (map second pledge-amt-backers))
     :pam-stats (get-stats (get-multiples (map first pledge-amt-backers)))
     :hi-pleged hi-pledged
     :hi-backed hi-backed
     :wc (parse-html-wordcount h)}))


#_ (clojure.pprint/pprint  (html->data0 (slurp "./resources/html/font-awesome-5__232193852")))

#_ (clojure.pprint/pprint  (html->data0 (slurp "./resources/html/light-table__ibdknox")))

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
        d' (->> d
                (filter (every-pred ks1-fn ks2-fn))
                (filter #(and (:goal %) (:pledged %)))
                (filter #(<= (:goal %) (:pledged %)))
                )]
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
   [:pam-stats :count]
   [:pam-stats :max]
   [:pam-stats :min]
   [:pam-stats :avg]
   [:pam-stats :median]
   [:pam-stats :p90]
   [:pam-stats :std-dev]
   [:b-stats :count]
   [:b-stats :max]
   [:b-stats :min]
   [:b-stats :avg]
   [:b-stats :median]
   [:b-stats :p90]
   [:b-stats :std-dev]
   [:pab-pt 10]
   [:pab-pt 25]
   [:pab-pt 50]
   [:pab-pt 75]
   [:pab-pt 90]
   [:pab-pt 95]
   [:hi-backed 0]
   [:hi-pleged 0]
   [:end-time]
   [:wc]])

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
        (println "CHECKING " id)
        (or (html-rscr-exists? id)
            (write-html-rsrc id
                             (request-html url)))))))

#_ (update-html-rsrcs 100)

(defn do-analysis
  []
  (do-all-html->data0)
  (analyze-data0 (read-all-data0-rsrcs)))

(defn print-csv
  [& ks]
  (println)
  (doseq [r (read-all-data0-rsrcs)]
    (println (clojure.string/join "," (map #(get-in r %) ks))))
  (println))

(defn ffff
  [x y m]
  (try
    (and (> (or (x m) -1) 0)
         (> (or (y m) -1) 0))
    (catch Throwable e      
      (clojure.pprint/pprint m)
      (throw e))))



(defn chart
  [d x y]
  (let [x-fn #(get-in % x -1)
        y-fn #(get-in % y -1)
        ds (filter :success? d)
        df (remove :success? d)
        ds' (filter (partial ffff x-fn y-fn)
                    ds)
        df' (filter (partial ffff x-fn y-fn)
                    df)
        xs (map x-fn ds')
        ys (map y-fn ds')
        xf (map x-fn df')
        yf (map y-fn df')]
    (println "====================")
    (println (double (mean xs)))
    (println (double (median xs)))
    (println (double (mean xf)))
    (println (double (median xf)))
    (println "====================")
    (xc/view (xc/xy-chart
              {"success" {:x xs
                          :y ys
                          :style {:marker-color :green}}
               "fail" {:x xf
                       :y yf
                       :style {:marker-color :red}}}
              
              {:title (str y " over " x)
               :render-style :scatter
               :theme :matlab

               :width 1500
               :height 950
               :legend {:visible? false}}))))

(defn ->json
  [x x-name]
  (println (json/generate-string
            (map-indexed #(-> %2
                              (select-keys [:pledged :success?])
                              (assoc :idx %
                                     x-name (get-in %2 x)))
                         (filter #(and (some-> % :pledged (> 0))
                                       (get-in % x)
                                       (< 0 (get-in % x)))
                                 (read-all-data0-rsrcs))))))

(defn mk-matrix-row
  [d]
  ((juxt :wc
         #(get-in % [:pa-stats :max])
         #(get-in % [:pa-stats :count])
         #(get-in % [:pam-stats :avg])
         #(get-in % [:pam-stats :median])
         #(get-in % [:pam-stats :max])) d))

(defn mk-matrix'
  []
  (->> (read-all-data0-rsrcs)
       (filter #(-> % :pledged (> 0)))
       (remove #(some nil? (mk-matrix-row %)))))

(defn mk-matrix
  []
  (mapv mk-matrix-row (mk-matrix')))

#_(def m2 (mds/mds  (mk-matrix) 2))

#_(def m2' (mapv (fn [xy m]
                 (assoc m
                        :x (first xy)
                        :y (second xy)))
               m2
               (mk-matrix')))

#_(chart m2' [:x] [:y])


#_ (->json [:wc] :word-count)

#_ (->json [:pam-stats :avg] :mult-avg)

#_ (->json [:duration] :duration)

#_ (->json [:backers-count] :backers-count)

#_ (print-csv [:pledged] [:pab-pt 50])

#_ (do-analysis)

#_(clojure.pprint/pprint (sort-by #(get % 2)  (mapv  (juxt :title :goal :pledged) (read-all-data0-rsrcs))))

#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:backers-count]  [:pledged])

#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pab-pt 75]  [:pledged])

#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:goal]  [:pledged])


#_(println (json/generate-string
          (take 10 (map-indexed #(-> %2
                                     (select-keys [:goal :pledged])
                                     (assoc :idx %))
                                (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs))))))

#_ (println (json/generate-string
          (map-indexed #(-> %2
                            (select-keys [:pledged :success?])
                            (assoc :idx %
                                   :levels (get-in %2 [:pa-stats :count]  [:pledged])))
                       (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)))))



#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:hi-pleged 0]  [:pledged])
#_ (chart (filter #(-> % :pledged (> 40000)) (read-all-data0-rsrcs)) [:hi-pleged 0]  [:pledged])

#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:hi-backed 0]  [:pledged])
#_ (chart (filter #(-> % :pledged (> 40000)) (read-all-data0-rsrcs)) [:hi-backed 0]  [:pledged])

; 15-19 levels
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pa-stats :count]  [:pledged])
#_ (chart (filter #(-> % :pledged (> 40000)) (read-all-data0-rsrcs)) [:pa-stats :count]  [:pledged])

; 30 is enough
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:duration]  [:pledged])
#_ (chart (filter #(-> % :pledged (> 20000)) (read-all-data0-rsrcs)) [:duration]  [:pledged])



; avg multi < 2
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pam-stats :avg]  [:pledged])
#_ (chart (filter #(-> % :pledged (> 40000)) (read-all-data0-rsrcs)) [:pam-stats :avg]  [:pledged])

 ; median multi < 2
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pam-stats :median]  [:pledged])

; 2 - 25
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pa-stats :min]  [:pledged])

; 500 - 10k
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pa-stats :max]  [:pledged])
#_ (chart (filter #(-> % :pledged (> 40000)) (read-all-data0-rsrcs)) [:pa-stats :max]  [:pledged])

; around 1k
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pa-stats :avg]  [:pledged])
#_ (chart (filter #(-> % :pledged (> 40000)) (read-all-data0-rsrcs)) [:pa-stats :avg]  [:pledged])

; 100 - 200
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pa-stats :median]  [:pledged])

; 2 - 5
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pam-stats :max]  [:pledged])

; 1-2
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:pam-stats :min]  [:pledged])

; low
#_ (chart (filter #(-> % :pledged (> 0)) (read-all-data0-rsrcs)) [:goal]  [:pledged])



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
