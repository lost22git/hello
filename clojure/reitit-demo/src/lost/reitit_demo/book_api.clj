(ns lost.reitit-demo.book-api
  (:require [lost.reitit-demo.util :as util]))

(defrecord Book [^long id
                 ^String title
                 ^java.time.Instant create-at
                 ^java.time.Instant update-at])

(defonce books
  (atom (->> ["Programming Clojure" "Clojure in action"]
             (map
              #(map->Book {:id (util/gen-id) :title % :create-at (java.time.Instant/now)}))
             (into []))))

(defn get-book-list
  "get book list"
  [_]
  (->> @books
       (assoc {:status 200} :body)))

(defn get-book-by-id
  "get book by id"
  [{{{:keys [id]} :path} :parameters}]
  (->> @books
       (filter #(= id (:id %)))
       (first)
       (assoc {:status 200} :body)))

(defn add-book
  "add a new book"
  [{{{:keys [title]} :body} :parameters}]
  (let [new-book
        (map->Book {:id (util/gen-id) :title title :create-at (java.time.Instant/now)})]
    (swap! books #(conj % new-book))
    {:status 200 :body new-book}))

(defn delete-book-by-id
  "delete book by id"
  [{{{:keys [id]} :path} :parameters}]
  (let [deleted (atom nil)]
    (swap!
     books
     (fn [old-books]
       (let [[a b] ((juxt filter remove) #(not= id (:id %)) old-books)]
         (reset! deleted (first b))
         a)))
    {:status 200 :body @deleted}))

(defn edit-book-by-id
  "edit book by id"
  [{{{:keys [id]} :path {:keys [title]} :body} :parameters}]
  (let [updated (atom nil)]
    (swap!
     books
     (fn [old-books]
       (let [[a b] ((juxt filter remove) #(not= id (:id %)) old-books)
             found (first b)]
         (if (nil? found) a
             (->> (assoc found :title title :update-at (java.time.Instant/now))
                  (reset! updated)
                  (conj a))))))
    (if-let [r @updated]
      {:status 200 :body r}
      {:status 200 :body (str "NOT FOUND book by id:" id)})))

(comment
  (get-book-list {}))
(comment
  (get-book-by-id {:parameters {:path {:id 1}}}))
(comment
  (add-book {:parameters {:body {:title "Getting Clojure"}}}))
(comment
  (delete-book-by-id {:parameters {:path {:id 8}}}))
(comment
  (edit-book-by-id {:parameters {:path {:id 2} :body {:title "Clojure in Action"}}}))

; === routes ===

(def routes-data
  ["/book"
   ["" {:get {:summary "get book list"
              :handler get-book-list}
        :post {:summary "add a new book"
               :parameters {:body {:title string?}}
               :handler add-book}}]
   ["/:id" {:get {:summary "get book by id"
                  :parameters {:path {:id int?}}
                  :handler get-book-by-id}
            :delete {:summary "delete book by id"
                     :parameters {:path {:id int?}}
                     :handler delete-book-by-id}
            :put {:summary "edit book by id"
                  :parameters {:path {:id int?} :body {:title string?}}
                  :handler edit-book-by-id}}]])
