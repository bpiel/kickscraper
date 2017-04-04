(ns kickscraper.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json])
  (:gen-class))

(def ^:dynamic *sleepy-req?* false)

(defn maybe-sleepy-get
  [& args]
  (apply client/get args)
  (when *sleepy-req?*
    (Thread/sleep 1000)))

(defn get-projects-by-page-num
  [n]
  (select-keys (json/parse-string
                (:body
                 (maybe-sleepy-get (str "https://www.kickstarter.com/discover/advanced?google_chrome_workaround&category_id=51&sort=end_date&seed=2485394&page=" n)
                             {:headers {"Accept" "application/json"}}))
                keyword)
               [:projects :has_more]))

(defn get-project-urls
  []
  (loop [page 1
         urls []]
    (let [{:keys [projects has_more]} (get-projects-by-page-num page)
          urls' (conj urls
                      (map #(get-in % [:TODO!!!!!!!!!!!!!!])
                           projects))]
      (if has_more
        (recur (inc page) urls')
        urls'))))

(defn project-url->id
  [url]
  (throw (Exception. "NOT IMPLEMENTED")))

(defn id->filename
  [id]
  (str "./resources/html/" id))

(defn read-file-if-exists
  [filename]
  (when (->  filename
            clojure.java.io/as-file
            .exists)
    (println "SLURPING! " filename)
    (slurp filename)))


(defn get-html-for-id
  [id]
  (println "DOWNLOADING! " id)
  (:body (client/get (format "http://www.zillow.com/homes/%s_id/" id))))


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



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
