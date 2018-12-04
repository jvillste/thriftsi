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

(defn simulate
  ([transactions]
   (simulate {}
             transactions))

  ([initial-state transactions]
   (interleave transactions
               (rest (reductions (fn [state [operation & parameters]]
                                   (apply (get (ns-publics 'thriftsi.core)
                                               operation)
                                          state
                                          parameters))
                                 initial-state
                                 transactions)))))

(deftest test-simulate
  (testing "spending chain"
    (is (= '([take-bank-loan :bank :a 1]
             {:loans {[:bank :a] 1}, :deposits {:a 1}}
             [spend :a :b 1]
             {:loans {[:bank :a] 1}, :deposits {:a 0, :b 1}}
             [spend :b :a 1]
             {:loans {[:bank :a] 1}, :deposits {:a 1, :b 0}}
             [pay-bank-loan :a :bank 1]
             {:loans {}, :deposits {:a 0, :b 0}})
           (simulate '[[take-bank-loan :bank :a 1]
                       [spend :a :b 1]
                       [spend :b :a 1]
                       [pay-bank-loan :a :bank 1]]))))

  (testing "loan chain"
    (is (= '([take-bank-loan :bank :a 1]
             {:loans {[:bank :a] 1}, :deposits {:a 1}}
             [lend :a :b 1]
             {:loans {[:bank :a] 1, [:a :b] 1}, :deposits {:a 0, :b 1}}
             [pay-loan :b :a 1]
             {:loans {[:bank :a] 1}, :deposits {:a 1, :b 0}}
             [pay-bank-loan :a :bank 1]
             {:loans {}, :deposits {:a 0, :b 0}})
           (simulate '[[take-bank-loan :bank :a 1]
                       [lend :a :b 1]
                       [pay-loan :b :a 1]
                       [pay-bank-loan :a :bank 1]]))))

  (testing "twice lent money, c has no way to pay the loan to b because there are no deposits in the system."
    (is (= '([take-bank-loan :bank :a 1]
             {:loans {[:bank :a] 1}, :deposits {:a 1}}
             [spend :a :b 1]
             {:loans {[:bank :a] 1}, :deposits {:a 0, :b 1}}
             [lend :b :c 1]
             {:loans {[:bank :a] 1, [:b :c] 1}, :deposits {:a 0, :b 0, :c 1}}
             [spend :c :a 1]
             {:loans {[:bank :a] 1, [:b :c] 1}, :deposits {:a 1, :b 0, :c 0}}
             [pay-bank-loan :a :bank 1]
             {:loans {[:b :c] 1}, :deposits {:a 0, :b 0, :c 0}})
           (simulate '[[take-bank-loan :bank :a 1]
                       [spend :a :b 1]
                       [lend :b :c 1]
                       [spend :c :a 1]
                       [pay-bank-loan :a :bank 1]]))))

  (testing "New bank loan can be used to resolve unpayable dept resulting from twice lent money."
    (is (= '([take-bank-loan :bank :a 1]
             {:loans {[:b :c] 1, [:bank :a] 1}, :deposits {:a 1, :b 0, :c 0}}
             [spend :a :c 1]
             {:loans {[:b :c] 1, [:bank :a] 1}, :deposits {:a 0, :b 0, :c 1}}
             [pay-loan :c :b 1]
             {:loans {[:bank :a] 1}, :deposits {:a 0, :b 1, :c 0}}
             [spend :b :a 1]
             {:loans {[:bank :a] 1}, :deposits {:a 1, :b 0, :c 0}}
             [pay-bank-loan :a :bank 1]
             {:loans {}, :deposits {:a 0, :b 0, :c 0}})
           (simulate {:loans {[:b :c] 1}, :deposits {:a 0, :b 0, :c 0}}
                     '[[take-bank-loan :bank :a 1]
                       [spend :a :c 1]
                       [pay-loan :c :b 1]
                       [spend :b :a 1]
                       [pay-bank-loan :a :bank 1]])))))
