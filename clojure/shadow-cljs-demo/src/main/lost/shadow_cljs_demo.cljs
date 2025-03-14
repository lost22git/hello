(ns lost.shadow-cljs-demo
  (:require [reagent.dom.client :as rdom]
            [re-frame.core :as rf]
            [day8.re-frame.http-fx]
            [ajax.core :as ajax]
            [cljs.core :as c]))

; (defn -main [& _]
;   (println "Hello NodeJS, Love from Shadow-cljs")
;   (js/console.log "Console logging..."))

;; === Event Handler ===

(rf/reg-event-db ::init (fn [_ _] {:cats []}))

(rf/reg-event-fx ::inc-count
                 (fn [_ _]
                   {:http-xhrio {:method :get
                                 :uri "https://api.thecatapi.com/v1/images/search"
                                 :timeout 3000
                                 :response-format (ajax/json-response-format {:keywords? true})
                                 :on-success [::fetch-cat-ok]
                                 :on-failure [:bad-http-result]}}))

(rf/reg-event-db ::fetch-cat-ok
                 (fn [db [_ data]]
                   (c/update db :cats c/into data)))

(rf/reg-event-db ::dec-count
                 (fn [db _]
                   (c/update db :cats c/pop)))

;; === Subscrition ===

(rf/reg-sub ::cat-list
            (fn [db _]
              (:cats db)))

(rf/reg-sub ::cats-count
            :<- [::cat-list] ;; sugar of (fn [_ _] (rf/subscribe [::cat-list]))
            (fn [cat-list _] (count cat-list)))

;; === View ===

(defn cat-count-view []
  (let [count @(rf/subscribe [::cats-count])]
    [:div
     [:button {:on-click #(rf/dispatch [::inc-count])}
      "+"]
     [:span count]
     [:button {:disabled (zero? count)
               :on-click #(rf/dispatch [::dec-count])}
      "-"]]))

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

