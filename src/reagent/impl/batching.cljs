(ns reagent.impl.batching
  (:refer-clojure :exclude [flush])
  (:require [reagent.debug :refer-macros [assert-some]]
            [reagent.impl.util :refer [is-client]]))

;;; Update batching

(defonce mount-count 0)

(defn next-mount-count []
  (set! mount-count (inc mount-count)))

(defn fake-raf [f]
  (js/setTimeout f 16))

(def next-tick
  (if-not is-client
    fake-raf
    (let [w js/window]
      (.bind (or (.-requestAnimationFrame w)
                 (.-webkitRequestAnimationFrame w)
                 (.-mozRequestAnimationFrame w)
                 (.-msRequestAnimationFrame w)
                 fake-raf)
             w))))

(defn compare-mount-order
  [^clj c1 ^clj c2]
  (- (.-cljsMountOrder c1)
     (.-cljsMountOrder c2)))

(defn run-queue [a]
  ;; sort components by mount order, to make sure parents
  ;; are rendered before children
  (.sort a compare-mount-order)
  (dotimes [i (alength a)]
    (let [^js/React.Component c (aget a i)]
      (when (true? (.-cljsIsDirty c))
        (.forceUpdate c)))))


;; Set from ratom.cljs
(defonce ratom-flush (fn []))

(defn run-funs [fs]
  (dotimes [i (alength fs)]
    ((aget fs i)))) ;;按照顺序执行数组内的函数

(defn enqueue [^clj queue fs f]
  (assert-some f "Enqueued function")
  (.push fs f)
  (.schedule queue))

(deftype RenderQueue [^:mutable ^boolean scheduled?]
  Object
  (schedule [this]
    (when-not scheduled?
      (set! scheduled? true) ;;设置为已经调度，防止重新进入
      (next-tick #(.run-queues this))))

  (queue-render [this c]
    (when (nil? (.-componentQueue this))
      (set! (.-componentQueue this) (array)))
    (enqueue this (.-componentQueue this) c))

  (add-before-flush [this f]
    (when (nil? (.-beforeFlush this))
      (set! (.-beforeFlush this) (array)))
    (enqueue this (.-beforeFlush this) f))

  (add-after-render [this f]
    (when (nil? (.-afterRender this))
      (set! (.-afterRender this) (array)))
    (enqueue this (.-afterRender this) f))

  (run-queues [this]
    (set! scheduled? false) ;;停止调度
    (.flush-queues this)) ;;刷新队列

  (flush-before-flush [this]
    (when-some [fs (.-beforeFlush this)]
      (set! (.-beforeFlush this) nil)
      (run-funs fs)))

  (flush-render [this]
    (when-some [fs (.-componentQueue this)]
      (set! (.-componentQueue this) nil)
      (run-queue fs)))

  (flush-after-render [this]
    (when-some [fs (.-afterRender this)] ;;如果渲染后清除函数不为空才去执行
      (set! (.-afterRender this) nil)
      (run-funs fs)))

  (flush-queues [this]
    (.flush-before-flush this) ;;在清空队列前的动作
    (ratom-flush) ;;清空ratom
    (.flush-render this) ;;对渲染队列进行清空
    (.flush-after-render this)));; 结束渲染的收尾工作

(defonce render-queue (->RenderQueue false))

(defn flush []
  (.flush-queues render-queue))

(defn flush-after-render []
  (.flush-after-render render-queue))

(defn queue-render [^clj c]
  (when-not (.-cljsIsDirty c)
    (set! (.-cljsIsDirty c) true)
    (.queue-render render-queue c)))

(defn mark-rendered [^clj c]
  (set! (.-cljsIsDirty c) false))

(defn do-before-flush [f]
  (.add-before-flush render-queue f))

(defn do-after-render [f]
  (.add-after-render render-queue f))
;;渲染队列尚未进行调度，对渲染队列进行调度
(defn schedule []
  (when (false? (.-scheduled? render-queue))
    (.schedule render-queue)))
