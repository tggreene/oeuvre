(ns tggreene.oeuvre.core-test
  (:require #?(:clj [clojure.test :refer :all]
                    :cljs [cljs.test :refer-macros [deftest is testing]])
            [tggreene.oeuvre :as o]))

(deftest fn->test
  (let [inc-x  (o/fn-> :x inc)
        sum-doubled-vals (o/fn->> vals (map (partial * 2)) (apply +))
        m {:x 1 :y 2 :z 3}]
    (is (= (inc-x m) 2))
    (is (= (sum-doubled-vals m) 12))))

(deftest condas->test
  (is (= 3
         (o/condas-> 1 number
                     (= 1 number) (inc number)
                     (= 2 number) (inc number))))
  (is (= 2
         (o/condas-> 1 number
                     (= 1 number) (inc number)
                     (= 1 number) (inc number)))))

(deftest if-all-let
  (is (true? (o/if-all-let [a 1] true)))
  (is (true? (o/if-all-let [a 1 b 2] true)))
  (is (= :else (o/if-all-let [a nil b 2] :true :else)))
  (is (nil? (o/if-all-let [a 1 b nil] true))))

(def original {:a {:b {:c 1
                       :d 2}
                   :e 3}})
(def target [[[:a :e] 3]
             [[:a :b :d] 2]
             [[:a :b :c] 1]])

(deftest path-vals-test
  (is (= (set (o/path-vals original)) (set target))))

(deftest assoc-in-path-vals-test
  (is (= (o/assoc-in-path-vals target) original)))

(def m  {:a {:b1 {:c1 "kikka"
                  :c2 "kakka"}
             :b2 "kukka"}})

(deftest dissoc-in-test
  (is (= (o/dissoc-in m [:a]) {}))
  (is (= (o/dissoc-in m [:b :c]) m))
  (is (= (o/dissoc-in {:a m} [:a :a]) {}))

  (is (= (o/dissoc-in m [:a :b2])
         {:a {:b1 {:c1 "kikka"
                   :c2 "kakka"}}}))

  (is (= (o/dissoc-in m [:a :b1 :c2])
         {:a {:b1 {:c1 "kikka"}
              :b2 "kukka"}}))

  (is (= (o/dissoc-in m [nil]) m)))

(deftest map-of-test
  (let [a 1 b true c [:abba :jabba]]
    (is (= (o/map-of a b c)
           {:a 1 :b true :c [:abba :jabba]}))))

(deftest deep-merge-test
  (testing "basics"
    (is (= (o/deep-merge {:a {:c 2}} {:a {:b 1}}) {:a {:b 1 :c 2}}))
    (is (= (o/deep-merge {:a 1} {:a 2}) {:a 2}))
    (is (= (o/deep-merge {:a {:b 1}} {:a {:b 2}}) {:a {:b 2}}))
    (is (= (o/deep-merge {:a {:b 1}} {:a {:b nil}}) {:a {:b nil}}))
    (is (= (o/deep-merge {:a 1} nil) nil))
    )

  (testing "sequentials"
    (is (= (o/deep-merge {:a [1]} {:a [2]}) {:a [2]}))
    (is (= (o/deep-merge :into {:a [1]} {:a [2]}) {:a [1 2]}))
    (is (= (o/deep-merge :into {:a #{:a}} {:a #{:b}}) {:a #{:b :a}}))))

(deftest wrap-into-test
  (is (= (o/wrap-into [] "foo") ["foo"]))
  (is (= (o/wrap-into [] ["a" "b"]) ["a" "b"]))
  (is (= (o/wrap-into #{} "a") #{"a"}))
  (is (= (o/wrap-into #{} ["a" "b"]) #{"a" "b"})))

(deftest assoc-if-test
  (is (= (o/assoc-if {} :a 5) {:a 5}))
  (is (= (o/assoc-if {:a 5} :b nil) {:a 5}))
  (is (= (o/assoc-if {:a 5} :a nil) {:a 5}))
  (is (= (o/assoc-if {} :a 1 :b false :c 2) {:a 1 :b false :c 2})))

(deftest where-fn-test
  (testing "fn?'s are returned as is"
    (let [f (constantly true)]
      (is (identical? f (o/where-fn f)))))

  (testing "ifn?'s are returned as is"
    (is (= :foo (o/where-fn :foo)))
    (is (= #{42} (o/where-fn #{42}))))

  (testing "map predicates"
    (let [p (o/where-fn {:foo 42})]
      (is (= (p {:foo 42}) true))
      (is (= (p {:foo 42
                 :bar 1337}) true))
      (is (= (p {:bar 1337}) false)))))

(deftest conjv-test
  (is (= (o/conjv [1 2] 3) [1 2 3]))
  (is (= (update-in {:a [1 2]} [:a] o/conjv 3) {:a [1 2 3]}))
  (is (= (-> [1 2] (o/conjv 3)) [1 2 3]))
  (testing "conjv to nil will create vec instead of seq"
    (is (vector? (:a (update-in {} [:a] o/conjv 1))))))

(deftest consv-test
  (is (= (o/consv [2 3] 1) [1 2 3]))
  (is (= (update-in {:a [2 3]} [:a] o/consv 1) {:a [1 2 3]}))
  (is (= (-> [2 3] (o/consv 1)) [1 2 3])))

(def test-coll [{:id 1 :foo "bar"}
                {:id 2 :foo "foo"}])

(deftest find-index-test
  (testing "map based where"
    (is (= (o/find-index test-coll {:id 1}) 0))
    (is (= (o/find-index test-coll {:id 2}) 1))
    (is (= (o/find-index test-coll {:id 2 :foo "foo"}) 1)))

  (testing "predicate where"
    (is (= (o/find-index test-coll (comp even? :id)) 1)))

  (testing "keyword where"
    (is (= (o/find-index test-coll :id) 0)))

  (testing "set where"
    (is (= (o/find-index ["a" "b" "c"] #{"c"}) 2)))

  (testing "value identity where"
    (is (= (o/find-index [4 3 2] 3) 1)))

  (testing "-> syntax"
    (is (= (-> test-coll (o/find-index {:id 2})) 1)))

  (testing "different coll types"
    (testing "seq"
      (is (= (o/find-index (seq test-coll) {:id 1}) 0)))))

(deftest find-first-test
  (is (= (o/find-first test-coll {:id 2}) (nth test-coll 1)))
  (is (= (-> test-coll (o/find-first {:id 2})) (nth test-coll 1))))

(deftest assoc-first-test
  (is (= (o/assoc-first test-coll {:id 2} {:id 2 :foo "zzz"}) (assoc-in test-coll [1 :foo] "zzz")))
  (testing "seq"
    (is (= (o/assoc-first (seq test-coll) {:id 2} {:id 2 :foo "zzz"}) (seq (assoc-in test-coll [1 :foo] "zzz"))))))

(deftest update-first-test
  (is (= (o/update-first test-coll {:id 2} #(assoc % :foo "zzz")) (assoc-in test-coll [1 :foo] "zzz")))
  (testing "rest args"
    (is (= (o/update-first test-coll {:id 2} assoc :foo "zzz") (assoc-in test-coll [1 :foo] "zzz"))))
  (testing "seq"
    (is (= (o/update-first (seq test-coll) {:id 2} assoc :foo "zzz") (seq (assoc-in test-coll [1 :foo] "zzz"))))))

(deftest map-keys-test
  (is (= (o/map-keys keyword {"a" 1 "b" 2}) {:a 1 :b 2})))

(deftest map-vals-test
  (is (= (o/map-vals inc {:a 1 :b 2}) {:a 2 :b 3})))

(deftest map-entries-test
  (is (= (o/map-entries (fn [[k v]] [k (inc v)]) {:a 1 :b 2}) {:a 2 :b 3})))

(deftest filter-keys-test
  (is (= {:a 1} (o/filter-keys #{:a} {:a 1 :b 2}))))

(deftest filter-vals-test
  (is (= {:a 1} (o/filter-vals #{1} {:a 1 :b 2}))))

(deftest filter-entries-test
  (is (= {:a 1} (o/filter-entries (comp #{1} second) {:a 1 :b 2}))))

(deftest remove-keys-test
  (is (= {:b 2} (o/remove-keys #{:a} {:a 1 :b 2}))))

(deftest remove-vals-test
  (is (= {:b 2} (o/remove-vals #{1} {:a 1 :b 2}))))

(deftest remove-entries-test
  (is (= {:b 2} (o/remove-entries (comp #{1} second) {:a 1 :b 2}))))

(deftest index-by-test
  (is (= {1 {:id 1 :v "foo"}
          2 {:id 2 :v "bar"}}
         (o/index-by :id [{:id 1 :v "foo"} {:id 2 :v "bar"}]))))

(deftest zip-test
  (is (= [[1 :a] [2 :b] [3 :c]] (o/zip [1 2 3] [:a :b :c]))))

(deftest build-tree-test
  (testing "Basic case, depth 1"
    (is (= [{:id 1
             :parent nil
             :children [{:id 2 :parent 1}
                        {:id 3 :parent 1}]}]
           (o/build-tree
             {:id-fn :id
              :parent-fn :parent
              :assoc-children-fn #(assoc %1 :children %2)}
             [{:id 1 :parent nil}
              {:id 2 :parent 1}
              {:id 3 :parent 1}]))))

  (testing "Basic case, modify items after building tree"
    (is (= [{:id 1
             :children [{:id 2}
                        {:id 3}]}]
           (o/build-tree
             {:id-fn :id
              :parent-fn :parent
              :item-fn #(dissoc % :parent)
              :assoc-children-fn #(assoc %1 :children %2)}
             [{:id 1 :parent nil}
              {:id 2 :parent 1}
              {:id 3 :parent 1}]))))

  (testing "Depth 2"
    (is (= [{:id 1
             :parent nil
             :children [{:id 2 :parent 1
                         :children [{:id 3 :parent 2}]}]}]
           (o/build-tree
             {:id-fn :id
              :parent-fn :parent
              :children-fn vec
              :assoc-children-fn #(assoc %1 :children %2)}
             [{:id 1 :parent nil}
              {:id 2 :parent 1}
              {:id 3 :parent 2}]))))

  (testing "Multiple roots"
    (is (= [{:id 1
             :parent nil
             :children [{:id 3 :parent 1}]}
            {:id 2
             :parent nil
             :children [{:id 4 :parent 2}]}]
           (o/build-tree
             {:id-fn :id
              :parent-fn :parent
              :assoc-children-fn #(assoc %1 :children %2)}
             [{:id 1 :parent nil}
              {:id 2 :parent nil}
              {:id 3 :parent 1}
              {:id 4 :parent 2}]))))

  (testing "Children as map"
    (is (= {1 {:id 1
               :parent nil
               :children {2 {:id 2 :parent 1}
                          3 {:id 3 :parent 1}}}}
           (o/build-tree
             {:id-fn :id
              :parent-fn :parent
              :children-fn #(o/index-by :id %)
              :assoc-children-fn #(assoc %1 :children %2)}
             [{:id 1 :parent nil}
              {:id 2 :parent 1}
              {:id 3 :parent 1}]))))

  (testing "Duplicate items"
    (is (= [{:id 1
             :parent nil
             :children [{:id 3 :parent 1}
                        {:id 4 :parent 1}]}
            {:id 2
             :parent nil
             :children [{:id 3 :parent 2}
                        {:id 4 :parent 2}]}]
           ;; In this case, user could separate :parents to items with :parent
           (o/build-tree
             {:id-fn :id
              :parent-fn :parent
              :assoc-children-fn #(assoc %1 :children %2)}
             (mapcat (fn [item]
                       (if (seq (:parents item))
                         (map #(-> item (dissoc :parents) (assoc :parent %)) (:parents item))
                         [(-> item (dissoc :parents) (assoc :parent nil))]))
                     [{:id 1 :parents nil}
                      {:id 2 :parents nil}
                      {:id 3 :parents [1 2]}
                      {:id 4 :parents [1 2]}])))))
  )

(deftest update-if-contains-test
  (is (nil? (o/update-if-contains nil :a inc)))
  (is (= (o/update-if-contains {} :a inc) {}))
  (is (= (o/update-if-contains {:a 0} :a inc) {:a 1}))
  (is (= (o/update-if-contains {:a 0} :a + 5) {:a 5})))


(deftest singular?-test
  (is (= false (o/singular? [])))
  (is (= true (o/singular? [1])))
  (is (= false (o/singular? [1 2]))))

(deftest multiple?-test
  (is (= false (o/multiple? [])))
  (is (= false (o/multiple? [1])))
  (is (= true (o/multiple? [1 2]))))

(deftest toggle-join-test
  (is (= #{:a :b :c} (o/toggle-join #{:a :b} :c)))
  (is (= #{:a :b :c} (o/toggle-join #{:a :b :c :d} :d))))
