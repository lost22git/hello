(ns app
  (:require ["react-dom/client" :as rdom]
            ["react" :refer [useState]]))

(defn ^:async fetch-cat []
  (js/fetch
   "https://api.thecatapi.com/v1/images/search"
   {:method :get}))

(defn ^:async inc-count [setState]
  (let [res (js-await (fetch-cat))
        data (js-await (.json res))]
    (setState #(update % :cats into data))))

(defn dec-count [setState]
  (setState #(update % :cats pop)))

(defn UI []
  (let [[state setState] (useState {:cats []})
        cat-list (:cats state)
        cat-count (count cat-list)]

    #jsx [:center
          [:h2 "Hello Squint"]
          [:div
           [:button {:onClick #(inc-count setState)} "+"]
           [:span cat-count]
           [:button {:disabled (zero? cat-count) :onClick #(dec-count setState)} "-"]]
          [:div
           (for [{:keys [url]} cat-list]
             #jsx [:img {:src url :width 400 :height 400}])]]))

(defonce root
  (rdom/createRoot (js/document.getElementById "app")))

(.render root #jsx [UI])
