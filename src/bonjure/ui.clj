(ns bonjure.ui
  (:import (javax.swing JFrame JTable JLabel JTextField JButton)
           (java.awt.event ActionListener WindowListener WindowAdapter WindowEvent)
           )

  (:require bonjure.querier)
  )

(defn open-frame []
  (let [frame (JFrame. "Bonjure")
        table (JTable. 4 2)

        callback (fn [data]
                   (println "Data changed")
                   )
        ]

    (doto frame
;      (.setLayout (GridLayout. 2 2 3 3))
      (.add table)
      (.setSize 500 300)
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowClosing [evt]
                        (println "Window Closing" evt)
                        (bonjure.querier/unregister-listener callback))
         )
       )
      (.setVisible true))
    (bonjure.querier/register-listener callback)))

