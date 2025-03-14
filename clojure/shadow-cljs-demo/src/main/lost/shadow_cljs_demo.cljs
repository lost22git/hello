(ns lost.shadow-cljs-demo
  (:require [reagent.dom.client :as rdom]
            [re-frame.core :as rf]
            [cljs.core :as c]))

; (defn -main [& _]
;   (println "Hello NodeJS, Love from Shadow-cljs")
;   (js/console.log "Console logging..."))

;; === Event Handlers ===

(rf/reg-event-db ::init (fn [_ _] {:cats []}))

(rf/reg-event-db ::inc-count
                 (fn [db _]
                   (c/update db :cats c/conj {:url "https://cdn2.thecatapi.com/images/W3fUcHI8z.jpg"})))

(rf/reg-event-db ::dec-count
                 (fn [db _]
                   (c/update db :cats c/pop)))

;; === View ===

(rf/reg-sub ::cats-count
            (fn [db _] (count (:cats db))))

(defn cat-count-view []
  (let [count @(rf/subscribe [::cats-count])]
    [:div
     [:button {:on-click #(rf/dispatch [::inc-count])}
      "+"]
     [:span count]
     [:button {:disabled (zero? count)
               :on-click #(rf/dispatch [::dec-count])}
      "-"]]))

(rf/reg-sub ::cat-list
            (fn [db _]
              (:cats db)))

(defn cat-list-view []
  (let [cats @(rf/subscribe [::cat-list])]
    [:div
     (for [{:keys [url]} cats]
       [:img {:src url :width 400 :height 400}])]))

(defn ui []
  [:div
   [:h1 "Hello Browser, Love from Shadow-cljs"]
   [cat-count-view]
   [cat-list-view]])

;; === EntryPoint ===

(defonce root
  (rdom/create-root (js/document.getElementById "app")))

(defn render-ui []
  (rdom/render root [ui]))

(defn run []
  (render-ui))

(defn ^:dev/after-load clear-cache-and-render! []
  (rf/clear-subscription-cache!)
  (render-ui))

(defn ^:export init []
  (rf/dispatch-sync [::init])
  (run))

