(ns com.gfredericks.schpec.fns)

(defn commutative
  "Returns a mapping fn suitable for use in clojure.spec.test/check that
   specifies the given fn is commutative"
  [f]
  (fn [mapping]
    (let [{:keys [args ret]} mapping]
      (if (> (count args) 1)
        (= ret (apply f (reverse args)))
        true))))

(defn associative
  "Returns a mapping fn suitable for use in clojure.spec.test/check that
   specifies the given fn is associative"
  [f]
  (fn [mapping]
    (let [{:keys [args ret]} mapping]
      (= ret (f (first args) (apply f (rest args)))))))

(defn has-identity
  "Returns a mapping fn suitable for use in clojure.spec.test/check that
   specifies the given fn has the given identity value"
  [f i]
  (fn [mapping]
    (let [{:keys [args ret]} mapping]
      (= ret (f i ret)))))
