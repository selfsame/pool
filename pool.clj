(ns pool (:import [UnityEngine]))

(defprotocol IPool
  (reuse   [a])
  (recycle [a b])
  (stats   [a]))

(deftype Pool [^|System.Object[]|                pool 
               ^System.Int64 ^:volatile-mutable  idx]
  IPool
  (stats [a]
    (list (inc idx) '/ (.Length pool)))
  (reuse [a]
    (if (neg? idx) false
      (do (set! idx (dec idx))
          (aget pool (inc idx)))))
  (recycle [a b]
    (try 
      (set! idx (+ idx 1)) 
      (aset pool idx b) nil
      (catch Exception e nil))))

(defmacro def-pool [length type-sym & fields]
  (let [sym (with-meta (symbol (str "*" type-sym)) {:tag type-sym})
        pool (with-meta (symbol (str "<>" type-sym)) {:tag type-sym})
        return (with-meta (symbol (str "!" type-sym)) {:tag System.Boolean})
        o# (gensym)]
    `(~'let [~pool (new ~Pool (~'make-array ~'System.Object ~length) -1)]
      (~'defn ~return
        [~(with-meta (symbol "a") {:tag type-sym})] 
        (~'recycle ~pool ~'a))
      (~'defn ~sym [~@fields]
        (~'if-let [~(with-meta o# {:tag type-sym}) (~'reuse ~pool)]
          (~'do 
          ~@(map #(list 'set! (list (symbol (str "." %)) o#) %) 
              fields) ~o#)
        (new ~type-sym ~@fields)))
      (~'def ~pool ~pool)
      (quote (~pool ~sym ~return)))))

(comment) 
(deftype Pig [^:volatile-mutable color])
(def-pool 10 Pig color)   ; (<>Pig *Pig !Pig) 
(*Pig :blue)              ; #<Pig pool.Pig>
(stats <>Pig)             ; (0 / 10)
(!Pig (*Pig :red))        ; nil
(stats <>Pig)             ; (1 / 10)

(map !Pig (map *Pig (range 5)))
(map *Pig (range 5))







