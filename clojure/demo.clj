#!/usr/bin/env -S clj -M

; def variables
(def m 1)
(def n 2)
; cal function
(assert (= 3 (+ m n)))

; let binding
; scope variables
(let [m 1]
  (let [n 2]
    (assert (= 3 (+ m n)))))

; binding: rebind dynamic variables in scope
(def ^:dynamic lang "Java")
(assert ((meta #'lang) :dynamic))
(binding [lang "Clojure"]
  (assert (= lang "Clojure")))
(assert (= lang "Java"))

;
; def functions
; ver1
(defn myhash
  [data] (.hashCode data))
; ver2
(def myhash (fn [data] (.hashCode data)))
; ver3
(def myhash #(.hashCode %))
(assert (= (.hashCode "haha") (myhash "haha")))

;; singly-linked-list '()
(def mylist '("alex" "bob"))
(def mylist (conj mylist "cindy")) ; cnoj: prepend
(assert (= (count mylist) 3))
(assert (= (first mylist) "cindy"))
(assert (= (rest mylist) '("alex" "bob")))
(assert (= (last mylist) "bob"))
(assert (= ["fred" "douglas" "cindy" "alex" "bob"]
           (into  mylist ["douglas" "fred"])))   ; into: union

;; set
(def myset #{"alex" "bob"})
(def myset (conj myset "cindy")) ; conj: add
(assert (= (count myset) 3))
(assert (contains? myset "cindy"))
(def myset (disj myset "cindy")) ; disj: del
(assert (= (count myset) 2))
(assert (not (contains? myset "cindy")))
(def myset (into myset '("cindy"))) ; into: A + B
(assert (= (count myset) 3))
(assert (contains? myset "cindy"))

;; map
(def mymap {"alex" 22 "bob" 33})
(def mymap (assoc mymap "cindy" 44)) ; assoc: add
(assert (= (count mymap) 3))
(assert (contains? mymap "cindy"))
(assert (= (get mymap "cindy") 44)) ; get
(assert (= (mymap "cindy") 44)) ; get
(assert (nil? (mymap "douglas"))) ; get not exists
(assert (= (mymap "douglas" 0) 0)) ; get not exists with default
(def mymap (zipmap (keys mymap) (vals mymap))) ; zipmap keys vals
(assert (= (count mymap) 3))
(def mymap (dissoc mymap "cindy")) ; dissoc: del
(assert (= (count mymap) 2))
(assert (nil? (mymap "cindy")))
(def mymap (merge mymap {"cindy" 44 "douglas" 55})) ; merge
(assert (= (count mymap) 4))
(assert (= (mymap "cindy") 44))
(assert (= (mymap "douglas") 55))
(def mymap (merge-with + mymap {"douglas" 11})) ; merge-with
(assert (= (count mymap) 4))
(assert (= (mymap "douglas") (+ 55 11)))

;; record
(defrecord Book [name pages])
(def book (->Book "The Clojure Book" 111))
; (def book (map->Book {:name "The Clojure Book" :pages 111}))
(assert (= (:name book) "The Clojure Book"))
(assert (= (:pages book) 111))

;; let destructing 
(let [[a b] [1 2]]
  (assert (= (+ a b) 3)))

(let [{:keys [first last]} {:first "foo" :last "bar"}]
  (assert (= first "foo"))
  (assert (= last "bar")))

;; function params destructing
(def add
  (fn [{:keys [x y]}]
    (+ x y)))
(assert (= (add {:x 1 :y 2 :z 0}) 3))
(defrecord Point [x y z])
(assert (= (add (->Point 1 2 0)) 3))

;; doto
(let [list (java.util.ArrayList.)]
  (doto list (.add 10) (.add 100) (.add 1000))
  (assert (= list [10 100 1000])))

;; pipe (aka. Threading Macros)
; -> (prev result as next first param)
(assert (= "HALO_CLOJURE"
           (-> "halo"
               (str "_")
               (str "clojure")
               clojure.string/upper-case)))
; ->> (prev result as next last param)
(assert (= "CLOJURE_HALO"
           (->> "halo"
                (str "_")
                (str "clojure")
                clojure.string/upper-case)))
 ; ->>
(assert (= "HaloClojure"
           (->> ["halo" "clojure"]
                (map clojure.string/capitalize)
                (reduce str ""))))
; as-> (prev result as next nth param)
(assert (= "_clojure_"
           (as-> "clojure" r
             (str r "_")
             (str "_" r))))
; some->
; some->>
; cond->
; cond->>

;; transduce = transform + reduce
(def xf (comp (filter #(.startsWith % "b"))
              (map clojure.string/upper-case)))
(-> (transduce xf conj ["foo" "bar"])
    (= ["BAR"])
    assert)

;; more seq ops
(assert (= (take 4 (repeat "ha"))
           (take 4 (repeatedly (fn [] "ha")))
           ["ha" "ha" "ha" "ha"]))

;; lazy-seq
(defn cons-lazy-seq
  ([] (cons-lazy-seq 0))
  ([n] (cons n (lazy-seq (cons-lazy-seq (+ n 2))))))

(assert (= (take 4 (cons-lazy-seq))
           [0 2 4 6]))

;; run!
(run! (comp println #(str "user-" %) clojure.string/upper-case)
      "JOse")

;; juxt
(->> ((juxt #(.toUpperCase %) #(.toLowerCase %)) "cLoJuRe")
     (= ["CLOJURE" "clojure"])
     assert)
(->> [1 2 3 4 5]
     ((juxt filter remove) even?)
     (= [[2 4] [1 3 5]])
     (assert))

;; complement
(assert (= [2 4] (filter even? [1 2 3 4 5])))
(assert (= [1 3 5] (filter (complement even?) [1 2 3 4 5])))

;; constantly
(assert (= 1 ((constantly 1) 1)))
(assert (= 1 ((constantly 1) 1 2)))
(assert (= 1 ((constantly 1) 1 2 "sdsf")))

;; fnil default param
(defn foo [x] (str "x=" x))
(assert (= "x=bar" ((fnil foo "bar") nil)))
(assert (= "x=foo" ((fnil foo "bar") "foo")))

;; for 
(->
 (for [{:keys [fid name]} [{:fid 1 :name "foo"} {:fid 2 :name "bar"} {:fid 3 :name "zoo"}]]
   [fid name])
 (= [[1 "foo"] [2 "bar"] [3 "zoo"]])
 (assert))
;; for :when
(->
 (for [{:keys [fid name]} [{:fid 1 :name "foo"} {:fid 2 :name "bar"} {:fid 3 :name "zoo"}]
       :when (odd? fid)]
   [fid name])
 (= [[1 "foo"] [3 "zoo"]])
 (assert))
;; for :while
(->
 (for [{:keys [fid name]} [{:fid 1 :name "foo"} {:fid 2 :name "bar"} {:fid 3 :name "zoo"}]
       :while (odd? fid)]
   [fid name])
 (= [[1 "foo"]])
 (assert))
;; for nested
(->
 (for [i (range 10)
       j (range 10)]
   [i j])
 count
 (= (* 10 10))
 assert)

;; infinity
(assert (infinite? ##Inf))
(assert (infinite? ##-Inf))

;; Pitfalls

(->> (let [filter :abc
           search "foo"]
       (filter (partial = search) ["foo" "bar" "foo"]))
     (= ["foo" "bar" "foo"])
     assert)

(->> (let [filter :abc
           search "foo"]
       (clojure.core/filter (partial = search) ["foo" "bar" "foo"]))
     (= ["foo" "foo"])
     assert)

(comment
  (use '[clojure.repl])
  (apropos "replace")
  (doc not-empty)
  (source filterv)
  (dir clojure.string))

(->
 "<![CDATA[<p><img src=\"https://bts-image.xyzcdn.net/aHR0cHM6Ly9qdXN0cG9kbWVkaWEuY29tL3BpYy9naWFkYWJhbm5lcjIwMjMuanBn.jpg\"/></p><figure><img src=\"https://image.xyzcdn.net/FrGGI4FK6g-OHLPZnEO_6vQLB03V.jpg\"/></figure><p><img src=\"https://bts-image.xyzcdn.net/aHR0cHM6Ly9qdXN0cG9kbWVkaWEuY29tL3BpYy9naWFkYWJhbm5lcnFyLmpwZw==.jpg\"/></p><p>– 本节目由GIADA出品，JustPod制作发行 –</p><p>【本期嘉宾】</p><p>陈果，哲学博士</p><p>陈鲁豫，媒体人、主持人</p><figure><img src="https://image.xyzcdn.net/FkcM2x94Soy67I0M0AwFEDAyFN7b.jpg"/></figure><p><a href=\"https://crmsys.redstone.com.cn/api/s?p=pages/productDetails/productDetails&amp;scene=3-14016\">专属礼券链接</a></p><p>【本期节目介绍】</p><p>岩中花述第六季的节目中，我们跟随过娱乐圈的「项目」，遨游过古典文学的海洋，在八平米的「陋室」中怡然，也在横断山的广袤中敬畏。在不同嘉宾的人生际遇中，我们一同面对历史与当下的疑问，探寻虚构与现实的边界，当然，也不断寻找着自己的定位。这一季似乎是与哲学有缘，本季最后一集节目中，鲁豫再次遇到了以哲学为业的嘉宾——陈果，聊人生与人心。</p><p>她是自小就对人生的意义充满好奇，总是疑问要过怎样的一生。父母说：愿你成为精神愉快的人；交好的老师说：在无常的世界中寻得快乐是一种大智慧；大学的民间校训说：去追求自由而无用的灵魂。</p><p>似乎她必然会走向哲学的道路，又或者无论如何她都会以哲学的视角去看待万事万物。</p><p>遇到困难或是被恶意刺痛时，她会在书架上挑一本书，向先贤求教，又或者是投入「买汰烧」和缝缝补补的烟火气，因为生活的很多问题无解，但「天总是会亮的，你需要一点耐心」。</p><p>十多年过去，互联网上闪现又淡出的那个人变了又仿佛没有变。她一直守着一方讲台和一片校园，修行着自己的人生课程，不论世间纷扰，兀自清然自得，只求自己内心的安定。</p><p>人生的道理不外乎此，我们终将回到德尔菲神庙前的那句箴言——认识你自己。而这条认识之路，终归要每一个人独自行走。</p><figure><img src=\"https://image.xyzcdn.net/FmKSOAnmkmWQQQH2RmZP17o-I9Cu.jpg\"/></figure><p>【内容提要】</p><p><strong>Part 1 金色童年与生活缝补</strong></p><p>04:48 象牙塔之外的生活也需要买菜烧饭、缝缝补补</p><p>07:42 平衡大脑与身体的能量，是身心的不二法门</p><p>12:17 一个老派的人，与70后共享更多社会经验</p><p>14:08 人生是艰难的，所以童年应该是金色的</p><p>15:56 回到更宽广的评价体系，任何人之间的比较其实没有意义</p><p>17:37 当优秀变为获取他人认可的渴望，也会成为对自我的压抑</p><p>21:03 如果一个人能一辈子傻乐，那必定是有大智慧</p><p>25:56 读书就是严肃的求教过程，邀请一位作者与自己对话</p><p><strong>Part 2 人文学科师与生</strong></p><p>27:43 文科基地班的训练，培养健全而非专业的人</p><p>32:06 碰到人生重要节点时，安静下来，聆听自己的心声</p><p>35:13 茨威格：冷静的哲思背后有对生活汹涌热烈的激情</p><p>39:27 以哲学的视角看待世间万物，不伪装、不美化、不虚构</p><p>44:44 遇到困境时，交给时间，这也是一种哲学思维</p><p>47:58 「自由而无用的灵魂」，恰是对唯实用论的反叛</p><p>51:22 老师是凡人，学生也是平常人，但师生关系是值得敬畏的</p><p>54:15 人文学科很难做到没有生命的注入</p><p><strong>Part 3 认识自己，活出自己</strong></p><p>56:40 更愿意做多元声音中的一种意见代表，而非意见领袖</p><p>62:06 生活未必邪不压正，但侦探小说里有一个理想的世界</p><p>67:01 每个人都有两面，被看到的那面越闪光，背面越黑暗</p><p>71:34 真正的成熟就在于你扪心自问，有没有活得更自由了</p><p>75:08 当新的经验注入生命，就长出了新的自己</p><p>– 嘉宾推荐的作品 –</p><p>《昨日的世界》（<em>Die Welt von Gestern</em>），斯特凡·茨威格（Stefan Zweig）著</p><p>「大侦探波洛系列」，阿加莎·克莉斯蒂（Agatha Christie<strong>）</strong>所著的系列侦探小说，对谈中提及的有：《东方快车谋杀案》《尼罗河上的惨案》《ABC谋杀案》《阳光下的罪恶》，以及作者的另一系列小说「马普尔小姐探案系列」</p><p>– 节目中提及的作品与人物 –</p><p>《好的爱情》《好的孤独》，陈果著，果麦文化 | 山东画报出版社，2024-6</p><p>李泽厚，哲学家、美学家、思想史学家，代表作《美的历程》</p><p>金岳霖，哲学家，清华大学哲学系创办者</p><p>南怀瑾，佛教居士，中国传统文化传播者</p><p>弗里德里希·威廉·尼采（Friedrich Wilhelm Nietzsche），出生于德国的哲学家，西方现代哲学开创者，代表作《悲剧的诞生》《查拉图斯特拉如是说》</p><p>莫拉维·贾拉鲁丁·鲁米，简称鲁米，出身大伊朗呼罗珊地区的诗人、伊斯兰教学者</p><p>纪伯伦·哈利勒·纪伯伦，黎巴嫩诗人、画家与作家，代表作品《先知》《沙与沫》等</p><p>大卫·苏切特（David Suchet），以扮演「大侦探波洛」系列电视剧里的名侦探赫尔克里·波洛闻名的英国演员</p><p>夏洛克·福尔摩斯（Sherlock Holmes），19世纪末侦探推理小说家柯南·道尔（Sir Arthur Ignatius Conan Doyle）创作的虚构人物</p><p>《古畑任三郎》，田村正和、西村雅彦主演的日本推理电视剧，1994年至2008年共拍摄43集</p><p>《神探可伦坡》（<em>Columbo</em>），彼得·福克（Peter Falk）主演的69集美国电视连续剧，1968年始播</p><p>《当尼采哭泣》（<em>When Nietzsche Wept</em>），Pinchas Perry执导的2007年美国电影</p><p>对谈中提及的其他人物及作品：刘兰芳、单田芳、田连元、《资治通鉴》《红楼梦》《包法利夫人》《约翰·克里斯多夫》</p><p>– 互动方式 –</p><p>GIADA官网：<a href=\"https://www.giada.cn/\">www.giada.cn</a></p><p>GIADA小红书：<a href=\"https://www.xiaohongshu.com/user/profile/5ed8b70e00000000010058e0?xhsshare=CopyLink&amp;appuid=5f22350f0000000001003304&amp;apptime=1694362176\">GIADA • 小红书 / RED</a></p><p>GIADA公众号：<a href=\"https://mp.weixin.qq.com/s/0fwCFTKTgL8wI9byKArtKg\">mp.weixin.qq.com</a></p><p>GIADA 微博：<a href=\"https://weibo.com/u/5243696839\">weibo.com</a></p>]]>"
 (clojure.string/replace #"<.*?>" ""))
