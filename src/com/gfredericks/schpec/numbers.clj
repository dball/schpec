(ns com.gfredericks.schpec.numbers
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen])
  (:import [java.math BigDecimal MathContext RoundingMode]))

(def finite?
  "Returns true if the given value is an actual finite number"
  (let [falsies #{Double/POSITIVE_INFINITY
                  Double/NEGATIVE_INFINITY
                  Double/NaN}]
    (fn [x]
      (and (number? x)
           (not (contains? falsies x))))))

(s/def ::finite
  (s/spec finite?
          :gen #(gen/double* {:infinite? false :NaN? false})))

(defn- bigdec-pred
  [precision scale]
  (fn [d]
    (and (or (not precision)
             (>= precision (.precision d)))
         (or (not scale)
             (let [d-scale (.scale d)]
               (and (not (neg? d-scale))
                    (>= scale d-scale)))))))

(defn bigdec-in
  "Specs a bigdec number. Options:

    :precision - the number of digits in the unscaled value (default none)
    :scale     - the number of digits to the right of the decimal (default none)
    :min       - minimum value (inclusive, default none)
    :max       - maximum value (inclusive, default none)

   A decimal satifies this spec if its precision and scale are not greater
   than the specified precision and scale, if given.

   Note that the java math definition of precision and scale may not be the
   same as e.g. your database. For example, -1E-75M has a precision of 1 and a
   scale of 75. For sanest results, you should specify both, though the spec
   does not require both."
  [& options]
  (let [{:keys [precision scale min max]} options
        dec-pred (bigdec-pred precision scale)]
    (letfn [(pred [d]
              (and (dec-pred d)
                   (or (not min)
                       (>= d min))
                   (or (not max)
                       (>= max d))))
            (gen []
              (let [min (or min
                            (and precision
                                 (-> BigDecimal/ONE
                                     (.movePointRight precision)
                                     dec
                                     .negate)))
                    max (or max
                            (and precision
                                 (-> BigDecimal/ONE
                                     (.movePointRight precision)
                                     dec)))
                    mc (when precision
                         (MathContext. precision RoundingMode/HALF_UP))]
                (letfn [(f [d]
                          (cond-> (bigdec d)
                            scale
                            (.setScale scale BigDecimal/ROUND_HALF_UP)
                            precision
                            (.round mc)))]
                  (gen/fmap f (gen/double* {:infinite? false
                                            :NaN? false
                                            :min min
                                            :max max})))))]
      (s/spec pred :gen gen))))

(s/def :specs.number.bigdec/precision
  pos-int?)

(s/def :specs.number.bigdec/scale
  (s/spec (fn [x] (and (int? x) (not (neg? x))))
          :gen #(gen/large-integer* {:min 0})))

(s/def :specs.number.bigdec/min
  (s/and bigdec?
         ::finite))

(s/def :specs.number.bigdec/max
  (s/and bigdec?
         ::finite))

(s/fdef bigdec-in
        :args (s/and (s/keys* :opt-un [:specs.number.bigdec/precision
                                       :specs.number.bigdec/scale
                                       :specs.number.bigdec/min
                                       :specs.number.bigdec/max])
               #(let [{:keys [min max precision scale]} %
                      dec-pred (bigdec-pred precision scale)]
                  (and (or (not (and min max))
                           (>= max min))
                       (or (not precision)
                           (pos? precision))
                       (or (not scale)
                           (not (neg? scale)))
                       (or (not (and precision scale))
                           (>= precision scale))
                       (or (not min)
                           (dec-pred min))
                       (or (not max)
                           (dec-pred max)))))
  :ret s/spec?
  :fn #(let [{:keys [ret args]} %
             {:keys [min max]} args]
         (and (or (not min)
                  (s/valid? ret min))
              (or (not max)
                  (s/valid? ret max)))))
