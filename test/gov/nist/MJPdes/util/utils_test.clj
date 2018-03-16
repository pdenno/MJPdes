(ns gov.nist.MJPdes.util.utils-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer (cl-format pprint pp)]
            [clojure.spec.alpha :as s]
            [gov.nist.MJPdes.util.utils :as util]))

(alias 'core 'gov.nist.MJPdes.core)

(def parallel-test
  (core/preprocess-model
   (core/map->Model ; a function call to make a Model from the following map argument
    {:line     ; a key introducing the line
     {:m1 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })  
      :b1 (core/map->Buffer {:N 3})                              
      :m2 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
      :b2 (core/map->Buffer {:N 5})
      :m3-1 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
      :m3-2 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
      :m3-3 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 }) 
      :b3 (core/map->Buffer {:N 1})                              
      :m4 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 }) 
      :b4 (core/map->Buffer {:N 1})
      :m5 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })}
     :topology [:m1 :b1 :m2 :b2 [:PARALLEL-OR :m3-1 :m3-2 :m3-3] :b3 :m4 :b4 :m5]
     :entry-point :m1
     :params {:warm-up-time 2000 :run-to-time 20000}
     :jobmix {:jobType1 (core/map->JobType {:portion 0.8
                                            :w {:m1 1.0, :m2 1.0, :m3 3.0, :m4 1.0, :m5 1.0}})
              :jobType2 (core/map->JobType {:portion 0.2
                                            :w {:m1 1.0, :m2 1.0, :m3 3.6, :m4 1.0, :m5 1.0}})}})))

(deftest test-best-parallel
  (testing "that best-parallel eliminates all but one parallel machine."
    (let [best (set (util/best-parallel parallel-test [:m1 :m2 :m3-1 :m3-2 :m3-3 :m4]))]
      (is (or (= best #{:m1 :m2 :m3-1 :m4})
              (= best #{:m1 :m2 :m3-2 :m4})
              (= best #{:m1 :m2 :m3-3 :m4}))))))

(deftest test-buffers-to
  (testing "that buffers-to does the right thing."
    (is (= :b1 (util/buffers-to parallel-test :m1)))
    (is (= :b2 (util/buffers-to parallel-test :m2)))
    (is (= :b3 (util/buffers-to parallel-test :m3-1)))
    (is (= :b3 (util/buffers-to parallel-test :m3-2)))
    (is (= :b3 (util/buffers-to parallel-test :m3-3)))
    (is (= nil (util/buffers-to parallel-test :m3-bozo)))
    (is (= :b4 (util/buffers-to parallel-test :m4)))
    (is (= nil (util/buffers-to parallel-test :m5))) 
    (is (= nil (util/buffers-to parallel-test :bozo)))))

(deftest test-takes-from
  (testing "that takes-from does the right thing."
    (is (= nil (util/takes-from parallel-test :m1)))
    (is (= :b1 (util/takes-from parallel-test :m2)))
    (is (= :b2 (util/takes-from parallel-test :m3-1)))
    (is (= :b2 (util/takes-from parallel-test :m3-2)))
    (is (= :b2 (util/takes-from parallel-test :m3-3)))
    (is (= nil (util/takes-from parallel-test :m3-bozo)))
    (is (= :b3 (util/takes-from parallel-test :m4)))
    (is (= :b4 (util/takes-from parallel-test :m5))) 
    (is (= nil (util/takes-from parallel-test :bozo)))))

(deftest test-upstream?
  (testing "that upstream? does the right thing."
    (is (util/upstream? parallel-test :m1 :m2))
    (is (util/upstream? parallel-test :m2 :m3-1))
    (is (util/upstream? parallel-test :m3-1 :b4))
    (is (util/upstream? parallel-test :b4 :m5))
    (is (not (util/upstream? parallel-test :m3-3 :m1)))
    (is (not (util/upstream? parallel-test :b3 :b1)))
    (is (not (util/upstream? parallel-test :m2 :m1)))
    (is (not (util/upstream? parallel-test :m5 :m1)))))


#_(defn analyze-results [filename]
  "Read an output file and perform various calculations."
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (let [results (loop [mp (edn/read {:eof :eof} in)
                         res {:starve []}]
                    (if (not= :eof mp)
                      (recur (edn/read {:eof :eof} in)
                             (update-in res [:starve] conj (:m2 (:starved mp))))
                      res))]
      ;; Starvation
      (let [starve (:starve results)]
        {:starvation {:min (apply min starve)
                      :max (apply max starve)
                      :mean (mean starve)
                      :variance (variance starve)
                      :values starve}}))))

