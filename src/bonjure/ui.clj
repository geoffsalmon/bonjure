(ns bonjure.ui
  (:import (javax.swing JFrame JPanel JTable JList JLabel JTextField JButton)
           (javax.swing.event ListDataEvent)
           (java.awt BorderLayout)
           (java.awt.event WindowListener WindowAdapter WindowEvent)
           )
  (:use [clojure.contrib.swing-utils :only [do-swing add-action-listener]])
  (:require bonjure.querier)
  )


(defprotocol DataUI "UI Model that's interested in changes to data"
  (data-updated [this]))

(deftype ListModel [data latom]
  :as this
  javax.swing.ListModel
  (addListDataListener [l] (swap! latom conj l))
  (removeListDataListener [l] (swap! latom disj l))
  (getElementAt [i] (nth (:rows @data) i))
  (getSize [] (println "getSize" data @data (count (:rows @data))) (count (:rows @data)))
  DataUI
  (data-updated
   []
   (println "other method")
   (let [evt (ListDataEvent. this ListDataEvent/CONTENTS_CHANGED 0 (count (:rows @data)))]
     (doseq [l @latom] (.contentsChanged l evt)))
   )  
  )

(defn open-frame []
  (let [
        ui-data (atom (bonjure.querier/get-data)) ; data that UI model currently sees
        data-listener (fn [data]
                        (do-swing (reset! ui-data data)))
        frame (JFrame. "Bonjure")
        model (ListModel ui-data (atom []))
        jlist (JList. model)
        button (JButton. "Go")
        ]
    (println "ui-data" ui-data)
    (println "Initial data" (:data model))
    (data-updated model)
    (add-watch ui-data model (fn [key r old new] (data-updated model)))
    (add-action-listener button (fn [evt] (bonjure.querier/trigger)))
    
    (doto frame
;      (.setLayout (GridLayout. 2 2 3 3))
      (.add jlist BorderLayout/CENTER)
      (.add button BorderLayout/SOUTH)
      (.setSize 500 300)
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowClosing [evt]
                        (println "Window Closing" evt)
                        (bonjure.querier/unregister-listener data-listener))
         )
       )
      (.setVisible true))
    (bonjure.querier/register-listener data-listener)))

