(ns thriftsi.core
  (:require [clojure.test :refer :all]))

(defn add [base amount]
  ((fnil + 0) base amount))

(defn substract [base amount]
  ((fnil - 0) base amount))

(defn update-balance [state operator party amount]
  (update-in state
             [:deposits party]
             operator
             amount))

(defn update-loan [state operator creditor deptor amount]
  (let [state (update-in state
                         [:loans [creditor deptor]]
                         operator
                         amount)]
    (if (= 0 (get-in state [:loans [creditor deptor]]))
      (update state :loans dissoc [creditor deptor])
      state)))

(defn assert-balance! [state party amount]
  (assert (<= amount (-> state :deposits party))
          "Not enough balance!"))

(defn spend [state from to amount]
  (assert-balance!  state from amount)

  (-> state
      (update-balance substract from amount)
      (update-balance add to amount)))

(deftest test-spend
  (is (= {:deposits {:a 5, :b 5}}
         (spend {:deposits {:a 10}}
                :a :b 5))))

(defn take-bank-loan [state creditor deptor amount]
  (-> state
      (update-loan add creditor deptor amount)
      (update-balance add deptor amount)))

(deftest test-take-bank-loan
  (is (= {:loans {[:bank :a] 10}
          :deposits {:a 10}}
         (take-bank-loan {}
                         :bank
                         :a
                         10))))


(defn lend [state creditor deptor amount]
  (assert-balance!  state creditor amount)

  (-> state
      (take-bank-loan creditor deptor amount)
      (update-balance substract creditor amount)))

(deftest test-lend
  (is (= {:deposits {:a 0, :b 10}
          :loans {[:a :b] 10}}
         (lend {:deposits {:a 10}}
               :a
               :b
               10))))

(defn pay-bank-loan [state deptor creditor amount]
  (assert-balance! state deptor amount)

  (-> state
      (update-balance substract deptor amount)
      (update-loan substract creditor deptor amount)))

(defn pay-loan [state deptor creditor amount]
  (-> state
      (update-loan substract creditor deptor amount)
      (update-balance substract deptor amount)
      (update-balance add creditor amount)))

(defn simulate [transactions]
  (interleave transactions
              (rest (reductions (fn [state [operation & parameters]]
                                  (apply (get (ns-publics 'thriftsi.core)
                                              operation)
                                         state
                                         parameters))
                                {}
                                transactions))))

(deftest test-simulate
  (testing "spending chain"
    (is (= '([take-bank-loan :bank :a 10]
             {:loans {[:bank :a] 10}, :deposits {:a 10}}
             [spend :a :b 10]
             {:loans {[:bank :a] 10}, :deposits {:a 0, :b 10}}
             [spend :b :a 10]
             {:loans {[:bank :a] 10}, :deposits {:a 10, :b 0}}
             [pay-bank-loan :a :bank 10]
             {:loans {}, :deposits {:a 0, :b 0}})
           (simulate '[[take-bank-loan :bank :a 10]
                       [spend :a :b 10]
                       [spend :b :a 10]
                       [pay-bank-loan :a :bank 10]]))))


  (testing "loan chain"
    (is (= '([take-bank-loan :bank :a 10]
             {:loans {[:bank :a] 10}, :deposits {:a 10}}
             [lend :a :b 10]
             {:loans {[:bank :a] 10, [:a :b] 10}, :deposits {:a 0, :b 10}}
             [pay-loan :b :a 10]
             {:loans {[:bank :a] 10}, :deposits {:a 10, :b 0}}
             [pay-bank-loan :a :bank 10]
             {:loans {}, :deposits {:a 0, :b 0}})
           (simulate '[[take-bank-loan :bank :a 10]
                       [lend :a :b 10]
                       [pay-loan :b :a 10]
                       [pay-bank-loan :a :bank 10]]))))

  (testing "twice lent money, c has no way to pay the loan to b"
    (is (= '([take-bank-loan :bank :a 10]
             {:loans {[:bank :a] 10}, :deposits {:a 10}}
             [spend :a :b 10]
             {:loans {[:bank :a] 10}, :deposits {:a 0, :b 10}}
             [lend :b :c 10]
             {:loans {[:bank :a] 10, [:b :c] 10}, :deposits {:a 0, :b 0, :c 10}}
             [spend :c :a 10]
             {:loans {[:bank :a] 10, [:b :c] 10}, :deposits {:a 10, :b 0, :c 0}}
             [pay-bank-loan :a :bank 10]
             {:loans {[:b :c] 10}, :deposits {:a 0, :b 0, :c 0}})
           (simulate '[[take-bank-loan :bank :a 10]
                       [spend :a :b 10]
                       [lend :b :c 10]
                       [spend :c :a 10]
                       [pay-bank-loan :a :bank 10]])))))
