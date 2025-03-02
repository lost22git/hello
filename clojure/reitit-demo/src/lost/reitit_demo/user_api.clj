(ns lost.reitit-demo.user-api
  (:require [lost.reitit-demo.util :as util]))

(defrecord User [^long id
                 ^String name
                 ^java.time.Instant create-at
                 ^java.time.Instant update-at])

(def users-state
  (atom (->> ["Alex" "Bob"]
             (map #(map->User {:id (util/gen-id)
                               :name %
                               :create-at (java.time.Instant/now)}))
             vec)))

(defn get-user-list
  "get user list"
  [_]
  (->> @users-state
       (assoc {:status 200} :body)))

(defn get-user-by-id
  "get user by id"
  [{{{:keys [id]} :path} :parameters}]
  (->> @users-state
       (filter #(= id (:id %)))
       (first)
       (assoc {:status 200} :body)))

(defn create-user
  "create a new user"
  [{{{:keys [name]} :body} :parameters}]
  (let [new-user
        (map->User {:id (util/gen-id) :name name :create-at (java.time.Instant/now)})]
    (swap! users-state #(conj % new-user))
    {:status 200 :body new-user}))

(defn delete-user-by-id
  "delete user by id"
  [{{{:keys [id]} :path} :parameters}]
  (let [deleted (atom nil)]
    (swap! users-state
           (fn [old-users-state]
             (let [[a b] ((juxt filter remove) #(not= id (:id %)) old-users-state)]
               (reset! deleted (first b))
               a)))
    {:status 200 :body @deleted}))

(defn edit-user-by-id
  "edit user by id"
  [{{{:keys [id]} :path {:keys [name]} :body} :parameters}]
  (let [updated (atom nil)]
    (swap! users-state
           (fn [old-users-state]
             (let [[a b] ((juxt filter remove) #(not= id (:id %)) old-users-state)
                   found (first b)]
               (if (nil? found) a
                   (->> (assoc found :name name :update-at (java.time.Instant/now))
                        (reset! updated)
                        (conj a))))))
    (if-let [r @updated]
      {:status 200 :body r}
      {:status 200 :body (str "NOT FOUND user by id:" id)})))

(comment
  (get-user-list {}))
(comment
  (get-user-by-id {:parameters {:path {:id 1}}}))
(comment
  (create-user {:parameters {:body {:name "Rich"}}}))
(comment
  (delete-user-by-id {:parameters {:path {:id 3}}}))
(comment
  (edit-user-by-id {:parameters {:path {:id 2} :body {:name "Bill"}}}))

; === routes ===

(def routes
  ["/users"
   {:tags #{"user-api"}
    :openapi {:security [{"auth" []}]}}
   ["" {:get {:summary "get user list"
              :handler get-user-list}
        :post {:summary "create a new user"
               :parameters {:body {:name string?}}
               :handler create-user}}]
   ["/:id" {:get {:summary "get user by id"
                  :parameters {:path {:id int?}}
                  :handler get-user-by-id}
            :delete {:summary "delete user by id"
                     :parameters {:path {:id int?}}
                     :handler delete-user-by-id}
            :put {:summary "edit user by id"
                  :parameters {:path {:id int?} :body {:name string?}}
                  :handler edit-user-by-id}}]])
