(ns gov.nist.MJPdes.core-test
  (:require [clojure.test :refer :all]
            [clojure.edn :as edn]
            [clojure.pprint :refer (cl-format pprint)]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.set :refer (union difference intersection)]
            [incanter.stats :as stats :refer (sample-exp)]
            [gov.nist.MJPdes.core :as core]
            [gov.nist.MJPdes.util.log :as log]      
            [gov.nist.MJPdes.util.utils :as util :refer (ppprint ppp)]))

;;; POD If you recompile core.clj after evaluating this, it won't happen. 
(stest/instrument) ; Instrument everything

(def ^:private diag (atom nil))

;;; Do this in clojupyter!
#_(defn check-exponential-pdf
  []
  (into (sorted-map)
        (map (fn [[k v]] [k (count v)])
             (group-by #(Math/round (quot % 0.1))
                       (stats/sample-exp 100000 :rate 0.9)))))

(defn =* [x y tol] (< (- x tol) y (+ x tol)))

(def fdata 
  (seq [[0 :down  1.0903780292458096]
        [1 :up    1.5669655022427902]
        [2 :down 10.138318336024103]
        [3 :up   10.991435470545209]
        [4 :down 18.189385678401067]
        [5 :up   18.507587845132132]
        [6 :down 35.608712172391236]
        [7 :up   36.852624249573]
        [8 :down 63.65613563175827]
        [9 :up   64.5679392394569]]))

(def tmodel
  (core/map->Model
   {:line 
    {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :up&down fdata :future [0 :down 1.0903780292458096]})} 
    :topology [:m1]
    :clock 0.0
    :params {:current-job 0}
    :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 50.0}})}}))

(def tjob (-> (core/new-job tmodel) (assoc :starts 0)))
  
;;; fdata end-time = last time the machine starts up plus what remains to be achieved at that time.
(def tjob-done-at ; = 52.89181885143091
  "This works because it drop into a big uptime starting at 36.852624249573."
  (let [idle (+ (- 1.5669655022427902 1.0903780292458096)    ; 0.47658747299698057
                (- 10.991435470545209 10.138318336024103)    ; 0.8531171345211064
                (- 18.507587845132132 18.189385678401067)    ; 0.3182021667310657
                (- 36.852624249573    35.608712172391236))   ; 1.2439120771817613
        achieved (+ 1.0903780292458096
                    (- 10.138318336024103  1.5669655022427902) ; 8.571352833781312
                    (- 18.189385678401067 10.991435470545209)  ; 7.1979502078558575
                    (- 35.608712172391236 18.507587845132132)) ; 17.101124327259104
        last-up 36.852624249573]
    (+ last-up (- 50 achieved))))

(deftest job-tests-1
  (testing "whether job would end when expected given fixed up&down schedule"
    (let [dur  (util/job-requires tmodel tjob :m1)]
      (is (== 50.0 dur))
      (is (=* tjob-done-at (core/end-time tmodel 50.0 :m1) 0.0000000001)))))

(def fdata1
  (seq
   [[0 :down 10.138318336024103]
    [1 :up   10.991435470545209]
    [2 :down 18.189385678401067]
    [3 :up   18.507587845132132]
    [4 :down 35.608712172391236]
    [5 :up   36.852624249573]
    [6 :down 63.65613563175827]
    [7 :up   64.5679392394569]]))

(def tmodel1
  (core/map->Model
   {:line 
    {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :up&down fdata1 :future [0 :down 10.138318336024103]})} 
    :topology [:m1]
    :clock 0.0
    :params {:current-job 0}
    :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 50.0}})}}))

(deftest job-tests-2
  (testing "whether job would end when expected given fixed up&down schedule"
    (let [dur  (util/job-requires tmodel1 tjob :m1)]
      (is (== 50.0 dur))
      (is (=* 52.415231378433944  (core/end-time tmodel1 50.0 :m1) 0.0000000001)))))

;;;(== 53.982196880676725 (+ 52.89181885143091 1.0903780292458096)

(defn update-clock [m old new]
  (-> m
      (assoc :last-clock old)
      (assoc :clock new)))

(defn test-bl-log
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (let [m (-> (core/map->Model
               {:line {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                :topology [:m1]
                :params {:current-job 0 :warm-up-time 0.0 :run-to-time 5.0}
                :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 1.0}})}})
              (core/preprocess-model :check? false))]
    (binding [log/*log-steady* (ref (log/steady-form m))] ; create a log for computations.
      (-> m 
          (log/log {:act :bl, :m :m1, :clk 0.5})
          (log/log {:act :ub, :m :m1, :clk 0.5})
          (log/log {:act :bl, :m :m1, :clk 0.5})
          (log/log {:act :ub, :m :m1, :clk 0.5})
          (update-clock 0.0 0.5)
          (log/push-log) ; The above is realistic and should be cleaned by log/clean-log-buf.
          (log/log {:act :bl, :m :m1, :clk 1.0})
          (update-clock 0.5 1.0)
          (log/push-log)
          (log/log {:act :ub, :m :m1, :clk 2.0}) ; total block = 1
          (update-clock 1.0 2.0)
          (log/push-log)
          (log/log {:act :bl, :m :m1, :clk 3.0})
          (update-clock 2.0 3.0)
          (log/push-log)
          (log/log {:act :ub, :m :m1, :clk 4.0}) ; total block = 2
          (update-clock 3.0 4.0)
          (log/push-log)
          (update-clock 4.0 5.0)
          (log/log {:act :aj, :m :m1, :clk 5.0}) ; Need something to log/push-log
          (log/push-log)                     ; block percent = 2/5
          (core/calc-basics @log/*log-steady*)))))

(defn test-sl-log 
  "Write some block/unblock log to a file and make sure it adds up."
  []
  (let [model (-> (core/map->Model
                   {:line {:m1 (core/map->ExpoMachine {:name :m1 :W 1.0 :lambda 0.1 :mu 0.9})}
                    :topology [:m1]
                    :params {:current-job 0 :warm-up-time 0.0 :run-to-time 5.0}})
                  (core/preprocess-model :check? false))]
    (binding [log/*log-steady* (ref (log/steady-form model))] ; create a log for computations.
      (-> model
          (log/log {:act :st, :m :m1, :clk 0.5})
          (log/log {:act :us, :m :m1, :clk 0.5})
          (log/log {:act :st, :m :m1, :clk 0.5})
          (log/log {:act :us, :m :m1, :clk 0.5})
          (update-clock 0.0 0.5)
          (log/push-log) ; The above is realistic, but cleaned by log/clean-log-buf.
          (log/log {:act :st, :m :m1, :clk 1.0})
          (update-clock 0.5 1.0)
          (log/push-log)
          (log/log {:act :us, :m :m1, :clk 2.0}) ; 1 unit starved
          (update-clock 1.0 2.0)
          (log/push-log)
          (log/log {:act :st, :m :m1, :clk 3.0})
          (update-clock 2.0 3.0)
          (log/push-log)
          (log/log {:act :us, :m :m1, :clk 4.0}) ; 2 total units starved
          (update-clock 3.0 4.0)
          (log/push-log)
          (update-clock 4.0 5.0)
          (log/log {:act :aj, :m :m1, :clk 5.0}) ; Need something to log/push-log
          (log/push-log)
          (core/calc-basics @log/*log-steady*)))))

(deftest log-testing-1
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (== 0.4 (-> (test-bl-log) :blocked :m1)))))

(deftest log-testing-2
  (testing "whether job would end when expected given fixed up&down schedule"
    (is (== 0.4 (-> (test-sl-log) :starved :m1)))))

(deftest test-efficiency
  (testing "that the lazy-seq generating up&down events produce uptime consistent 
            values of mu and lambda."
    (let [mu 0.9
          lambda 0.1
          start (core/expo-up&down-init-event lambda mu)
          up&down (core/expo-up&down lambda mu start)
          result (loop [uptime 0.0
                        downtime 0.0
                        last start
                        up&down (drop 1 up&down)]
                   (let [[last-n last-event last-etime] last
                         [     n      event      etime :as this-one] (first (take 1 up&down))]
                     (cond (= n 1000000) {:up uptime :down downtime}
                           (= event last-event) (throw (ex-info "event generation fails." {}))
                           :else (recur
                                  (if (= event :down) (+ uptime   (- etime last-etime)) uptime)
                                  (if (= event :up)   (+ downtime (- etime last-etime)) downtime)
                                  this-one
                                  (drop 1 up&down)))))]
      (is (< 0.89 (/ (:up result) (+ (:up result) (:down result))) 0.91)))))

(def tf1
  (core/preprocess-model
   (core/map->Model
    {:line 
     {:m1 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 }) 
      :b1 (core/map->Buffer {:N 3})
      :m2 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
      :b2 (core/map->Buffer {:N 5})
      :m3 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
      :b3 (core/map->Buffer {:N 1})
      :m4 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 })
      :b4 (core/map->Buffer {:N 1})
      :m5 (core/map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
     :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
     :entry-point :m1 ; 20,000 200,000
     :params {:warm-up-time 2000 :run-to-time 20000}  
     :jobmix {:jobType1 (core/map->JobType {:portion 1.0
                                            :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})}})))



(def tp1 {:blocked
         {:m1 0.1860
          :m2 0.0346
          :m3 0.1046
          :m4 0.0398
          :m5 0.0},
         :starved
         {:m1 0.0
          :m2 0.0089
          :m3 0.0186
          :m4 0.0462
          :m5 0.1860}})

(def tp2 {:blocked
          {:m1 0.1501
           :m2 0.0001
           :m3 0.0258
           :m4 0.0061
           :m5 0.0},
          :starved
         {:m1 0.0
          :m2 0.0001
          :m3 0.0578
          :m4 0.0371
          :m5 0.1501}})

;;;; Arrows point to the lower value. This one (tp2) is :m2. 
;;;       :m1          :m2          :m3          :m4          :m5
;;; ST   0.0         0.0001       0.0578       0.0371        0.1501
;;; BL   0.1501      0.0001       0.0258       0.0061        0.0

(deftest bneck-test
  (testing "Check bottleneck identification"
    (is (= (:bottleneck-machine (core/calc-bneck tp1 tf1)) :m4))
    (is (= (:bottleneck-machine (core/calc-bneck tp2 tf1)) :m2))))

;;; m1 |---- 2 ----|---- 2 ----|---- 2 ----|BBBBB|---- 2 ----|
;;; m2             |-------------- 5 ------------|                                        
(defn load-m1
  "No warm-up. M2 takes almost the whole simulation to process a part."
  []
  (with-out-str
    (core/main-loop
     (core/map->Model
      {:line ; POD if you go really crazy with lambda and mu it will hang. Not investigated!
       {:m1 (core/map->ExpoMachine {:lambda 0.0001 :mu 100.0 :W 1.0}) 
        :b1 (core/map->Buffer {:N 1})
        :m2 (core/map->ExpoMachine {:lambda 0.0001 :mu 100.0 :W 1.0})}
       :report {:log? true :max-lines 3000 :diag-log-buf? true}
       :number-of-simulations 1
       :topology [:m1 :b1 :m2]
       :entry-point :m1
       :params {:warm-up-time 0 :run-to-time 10}
       :jobmix {:jobType1 (core/map->JobType {:portion 1.0 :w {:m1 2.0, :m2 5.0}})}}))))

;;; Note I haven't finished this test, of course. :diag-log-buf? isn't working. 

;;; POD I think that there still may be a bug with 'confused' log data.
;;; See the text around new-blocked? I think there is a situation that could be
;;; tested with something like "load-m1" above where block/unblock messages will be 
;;; processed in the wrong order. 

#{{:act :aj, :j 1, :jt :jobType1, :ends 2.0, :clk 0.0}
  {:act :st, :m :m2, :clk 0.0}
  {:act :bj, :bf :b1, :j 1, :n 0, :clk 2.0}
  {:act :aj, :j 2, :jt :jobType1, :ends 4.0, :clk 2.0}
  {:act :sm, :bf :b1, :j 1, :n 1, :clk 2.0}
  {:act :us, :m :m2, :clk 2.0}
  {:act :bj, :bf :b1, :j 2, :n 0, :clk 4.0}
  {:act :aj, :j 3, :jt :jobType1, :ends 6.0, :clk 4.0}
  {:act :bl, :m :m1, :clk 6.0}
  {:act :ej, :m :m2, :j 1, :ent 0.0, :clk 7.0}
  {:act :sm, :bf :b1, :j 2, :n 1, :clk 7.0}
  {:act :bj, :bf :b1, :j 3, :n 0, :clk 7.0}
  {:act :ub, :m :m1, :clk 7.0}
  {:act :aj, :j 4, :jt :jobType1, :ends 9.0, :clk 7.0}
  {:act :bl, :m :m1, :clk 9.0}}

;;; Instrument works best when down here. 
(stest/instrument)

:loaded-it-all
  
#_(def f0
  (map->Model
   {:line 
    {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0}) 
     :b1 (map->Buffer {:N 2})
     :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0})}
    :number-of-simulations 1
    :report {:log? true :max-lines 1000}
    :topology [:m1 :b1 :m2]
    :entry-point :m1
    :params {:warm-up-time 2000 :run-to-time 10000}
    :jobmix {:jobType1 (map->JobType {:portion 1.0 :w {:m1 2.0, :m2 0.8}})}}))

#_(defn runit []
  (with-open [w (clojure.java.io/writer "/tmp/f0.clj")]
    (core/main-loop f0 :out-stream w)))

#_(def f1
  (map->Model
   {:line 
    {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}}) 
     :b1 (map->Buffer {:N 3})
     :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
     :b2 (map->Buffer {:N 5})
     :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
     :b3 (map->Buffer {:N 1})
     :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
     :b4 (map->Buffer {:N 1})
     :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})}
    :number-of-simulations 10
    :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
    :entry-point :m1 ; 
    :params {:warm-up-time 2000 :run-to-time 20000}  
    :jobmix {:jobType1 (map->JobType {:portion 1.0
                                      :w {:m1 1.0,
                                          :m2 1.0,
                                          :m3 1.0,
                                          :m4 1.0,
                                          :m5 1.01}})}}))

#_(def f2
    (map->Model
   {:line 
    {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 }) ; the definition of machine :m1
     :b1 (map->Buffer {:N 3})                             ; the definition of buffer :b1
     :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
     :b2 (map->Buffer {:N 5})
     :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
     :b3 (map->Buffer {:N 1})
     :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 })
     :b4 (map->Buffer {:N 1})
     :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
    :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5] ; the arrangement of the line
    :entry-point :m1 ; where jobs start
    :params {:warm-up-time 20000 :run-to-time 100000}  
    :jobmix {:jobType1 (map->JobType {:portion 0.8 ; 80% of jobs will be of type :jobType1.
                                      :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})
             :jobType2 (map->JobType {:portion 0.2 ; 20% of jobs will be of type :jobType2.
                                      :w {:m1 1.0, :m2 1.0, :m3 1.3, :m4 1.0, :m5 1.0}})}}))

;;; Stuff from sinet/scada.clj
(declare job-map job-trace scada-patterns trim-patterns all-scada-patterns exceptional-msgs
         scada-gather-job scada-all-job-ids machine-paths)

(def des-model
   (core/map->Model 
    {:line     
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
     :topology [:m1 :b1 :m2 :b2
                {:type :parallel-or :name :m3 :machines [:m3-1 :m3-2 :m3-3]}
                :b3 :m4 :b4 :m5]
     :entry-point :m1
     :report {:atom? true :max-lines 3000 :up&down? false}
     :params {:warm-up-time 500 :run-to-time 1200}
     :jobmix {:jobType1 (core/map->JobType {:portion 0.8
                                       :w {:m1 1.0, :m2 1.0, :m3 3.0, :m4 1.0, :m5 1.0}})
              :jobType2 (core/map->JobType {:portion 0.2
                                            :w {:m1 1.0, :m2 1.0, :m3 3.6, :m4 1.2, :m5 1.0}})}}))

(deftest action-ordering
  (testing "that the log reports things in the correct order. (This one runs for a while.)"
    (with-out-str (core/main-loop des-model))
    (is (> (count @log/log-atom) 2900))
    (let [log (-> @log/log-atom
                  job-map
                  scada-patterns
                  exceptional-msgs
                  machine-paths)]
      (is (= (:machine-paths log)
             #{[:m1 :m2 :m3-1 :m4 :m5]
               [:m1 :m2 :m3-2 :m4 :m5]
               [:m1 :m2 :m3-3 :m4 :m5]})))))

;;;================================================================================================
;;; So far, all this stuff below is used just for checking the order in which machines are visited.
;;;================================================================================================
(defn job-trace 
  "Return all mentions of the job."
  [log job-id]
  (when-let [jcover (get (:job-map log) job-id)]
    (->> (subvec (:raw log) (:starts jcover) (-> jcover :ends inc))
         (remove #(and (contains? % :j) (not= (:j %) job-id)))
         (remove #((:exceptional log) (:act %)))
         vec)))
             
(defn scada-patterns 
  "Compute the problem's SCADA patterns. It is a vector of maps with keys
   (:id :form :njobs :relations). Run once per problem."
  [log]
  (let [raw (:raw log)]
    (assoc log
           :patterns
           (as-> raw ?pats 
             (all-scada-patterns ?pats)
             (trim-patterns ?pats 5 5)
             (map #(as-> % ?pat
                     (assoc ?pat :njobs (count (:jobs ?pat)))
                     (dissoc ?pat :jobs)) ; POD don't think the actual job will be useful.
                  ?pats)))))

(defn scada-gather-job
  "Return a 'job trace', every mention of of job-id in chronological order."
  [data job-id]
  (filter  #(= (:j %) job-id) data)) ; POD this is not a very efficient implementation!

(defn scada-all-job-ids
  "Return a vector of all job-ids found in the data"
  [data]
  (sort (distinct (map :j (filter #(contains? % :j) data)))))

(defn match-msg?
  "Return true if msg matches form"
  [msg form]
  (every? (fn [key]
           (or (and (= (key form) :*) (contains? msg key))
               (= (key form) (key msg))))
          (keys form)))

(defn match-pattern?
  "Return true if msgs matches pattern"
  [msgs pattern]
  (let [form (:form pattern)]
    (and (= (count msgs) (count form))
         (every? (fn [n] (match-msg? (nth msgs n) (nth form n)))
                 (range (count form))))))

(defn make-pattern
  "Create a SCADA pattern; they look like this: {:act :m2-start-job, :bf :b1, :n :*}, 
   {:act :ej, :m m1}..."
  [msgs pattern-id]
  {:id pattern-id
   :form
   (as-> msgs ?msgs
     (map #(reduce (fn [msg key] (dissoc msg key)) ; remove unnecessary keys
                   %
                   (difference (set (keys %))
                               #{:act :jt :bf :m :n}))  ; POD Long-term okay?
          ?msgs)
     (map #(reduce (fn [msg key] (if (contains? msg key) (assoc msg key :*) msg))
                   % ; wildcard certain msg vals
                   [:jt :n]) ; POD I ONLY WANT :jt and :n here
          ?msgs)
     (vec ?msgs))})
       
;;; POD Probably want start and stop point for every occurrence. 
(defn all-scada-patterns
  "Return a vector of all the job-trace patterns  found in the SCADA log.
   A 'job-trace-pattern' only includes msgs that have a :j. Run once per problem at init."
  [data]
  (let [pattern-id (atom -1)]
    (reduce (fn [patterns job-id]
              (let [job (scada-gather-job data job-id) ; get a job trace
                    matches (filter #(match-pattern? job %) patterns)] ; matches one already collected?
                (if (empty? matches)  ; no, make a new pattern.
                  (conj patterns (assoc (make-pattern job (swap! pattern-id inc)) :jobs [job-id]))
                  (reduce (fn [patterns id] ; yes, just update the job count.
                            (update-in patterns [id :jobs] #(conj % job-id)))
                          patterns (map :id matches)))))
            [] (scada-all-job-ids data))))

(defn trim-patterns
  "Trim up to ntrim patterns from the ends of the pattern vector if they account for only a few (njobs) jobs.
   These are quite likely to be fragments of the complete log for these jobs. Run once per problem."
  [patterns ntrim njobs]
  (let [size (count patterns)]
    (reduce (fn [patterns id]
              (let [p (some #(when (= (:id %) id) %) patterns)]
                (if (< (count (:jobs p)) njobs)
                  (remove #(= % p) patterns)
                  patterns)))
            patterns
            (into (range (- size ntrim) size) (range ntrim)))))

(defn exceptional-msgs
  "Return the set of the exceptional messages found in the scada log."
  [log]
  (let [patterns (:patterns log)
        ordinary (set (mapcat (fn [pat] (map :act (:form pat))) patterns))]
    (-> log
        (assoc :ordinary ordinary)
        (assoc :exceptional
               (->> (reduce (fn [excepts msg]
                              (if (ordinary (:act msg))
                                excepts ; dissoc for distinct members
                                (conj excepts (:act msg))))
                            []
                            (:raw log))
                    set)))))


(defn machine-paths [log]
  "Return the sequence of machines visited by each whole job"
  (assoc log :machine-paths
         (reduce (fn [paths job-id]
                   (conj paths
                         (->> (job-trace log job-id)
                              (map :m)
                              dedupe
                              vec)))
                 #{}
                 (->> log :job-map (map first)))))

(defn job-map 
  "Return a map indexed by job-id and containing maps of where 
   mention of that job starts and stops. The job must be whole to be reported."
  [raw]
  (let [job-map (as-> {} ?jmap
                  (reduce (fn [jmap msg]
                            (if-let [job-id (:j msg)]
                              (if (contains? jmap job-id)
                                (assoc-in jmap [job-id :ends] (:line msg))
                                (if (= (:mjpact msg) :aj) ; whole job started.
                                  (assoc jmap job-id {:starts (:line msg)})
                                  jmap))
                              jmap))
                          ?jmap
                          raw)
                  (reduce (fn [jmap job-id] ; whole job ended.
                            (if-let [end-line (-> (get jmap job-id) :ends)]
                              (if (= :ej (:mjpact (nth raw end-line)))
                                jmap
                                (dissoc jmap job-id))
                              (dissoc jmap job-id)))
                          ?jmap
                          (keys ?jmap)))]
    (-> {}
        (assoc :raw raw)
        (assoc :job-map job-map))))

        
