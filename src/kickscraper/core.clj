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
  (spit (id->filename id)
        html)
  html)

(defn fetch-html-for-id
  [id]
  (or (-> id
          id->filename
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

(defn get-all-html-rsrc-ids
  []
  (mapv #(.getName %)
        (filter #(.isFile %)
                (file-seq (clojure.java.io/file "./resources/html")))))

(defn parse-html-title [h] (second (re-find #"<title>([^>]*)</title>" h)))
(defn parse-html-pledged [h]
  (try
    (Double/parseDouble (second (re-find #"data-pledged=\"([^\"]*)\"" h)))
    (catch Exception e nil)))

(defn html->data0
  [h]
  {:title (parse-html-title h)
   :pledged (parse-html-pledged h)})

(defn write-data0-rsrc
  [id d]
  (spit (id->data0-filename id)
        (pr-str d))
  d)

(defn read-data0-rsrc
  [id]
  (read-string (read-rsrc (id->data0-filename id))))

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

(defn analyze-data1->final
  [id]
  (throw (Exception. "NOT IMPLEMENTED")))

(defn update-html-rsrcs
  [& [pages]]
  (binding [*sleepy-req?* true]
    (doseq [url (get-project-urls pages)]
      (let [id (project-url->id url)]
        (or (html-rscr-exists? id)
            (write-html-rsrc id
                             (request-html url)))))))

#_ (update-html-rsrcs 1)

(defn do-analysis
  []
  (do-all-html->data0)
  (analyze-data1->final (mk-all-data1)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
