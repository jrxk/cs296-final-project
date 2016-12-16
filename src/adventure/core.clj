(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:spawn-room {:desc "The walls are made of wood. There is no window. To the west is a door. There is a Japanese sword next to you. Several books and some newspapers are on a desk. Under the desk is a strangely looking stone and a pen."
           :title "in the spawn room"
           :dir {:south :grue-pen
                 :west :hallway0}
           :contents #{"sword", "book", "paper", "stone", "pen"}}
   :grue-pen {:desc "It is very dark. You are about to be eaten by a grue. "
              :title "in the grue pen"
              :dir {:north :spawn-room
                    :south :death}
              :contents #{}
              :enemy "grue"
              :enemylevel 0}
   :hallway0 {:desc "There are two doors, one to the east, one to the northeast."
              :title "in the hallway"
              :dir {:north :hallway1
                    :east :spawn-room}
              :contents #{}}
   :hallway1 {:desc "There are two doors, one to the east, one to the southeast. An apple is on the floor. It doesn't smell good."
              :title "in the hallway"
              :dir {:east :room1
                    :south :hallway0}
              :contents #{"apple"}}
   :room1    {:desc "There is a door to the west. A window opens to the north. There is a garden out there. The windows is large. There is a table and a bed. On the bed is someone's final exam. There seems to be something under the bed."
              :title "in a mysterious room."
              :dir {:west :hallway1
                    :north :garden}
              :contents #{"final exam", "hammer", "box"}
              }
   :garden    {:desc "This is a beautiful garden. Lots of flowers. A fighting robot (probably named Bastion) stands there."
              :title "in a garden."
              :dir {:south :room1
                    :east :forest1
                    :north :temple
                    :west :cornfield}
              :contents #{"flower"}
               :enemy "robot"
               :enemylevel 0}
   :temple    {:desc "This is the atrium of the temple."
               :title "in temple."
               :dir {:south :garden
                     :east :cliff1
                     :up :temple-2
                     :west :forest0
                     :north :cliff2}
               :contents #{}}
   :temple-2  {:desc "This is the second floor of the temple. There is a giant beast called Roshan in the north side of this floor. He seems unbeatable."
               :title "on the second floor of the temple."
               :dir {:down :temple
                     :north :death}
               :contents #{}
               :enemy "Roshan"
               :enemylevel 5}
   :cliff1    {:desc "Below is an abyss. There is nothing. Try not to jump."
               :title "in a garden."
               :dir {:west :temple
                     :east :death
                     :down :death
                     :jump :death
                     :north :death}
               :contents #{}}
   :forest0   {:desc "This is a dimly lit forest, with large trees all around. One particularly large tree with some low branches stands here. There is a squirrel under the tree. You wouldn't fight a squirrel, would you?"
               :title "in a forest."
               :dir {:west :crop-science-building
                     :east :temple
                     :south :cornfield
                     :north :forest2}
               :contents #{}
               :enemy "squirrel"
               :enemylevel 10000}
   :cornfield {:desc "To you south is the building that you just escaped from. It looks like a dorm in a university surrounded by endless corns."
               :title "in a cornfield."
               :dir {:north :forest0
                     :east :garden}
               :contents #{"corn"}
               }
   :crop-science-building
               {:desc "This is the CS Department of Cornfield University. A professor is lecturing on the north side. You can get free pizza and soda here!"
               :title "in the Crop Science Building."
               :dir {:east :forest0
                     :north :cs225-classroom
                     :west :apartment}
               :contents #{"corn" "pizza" "soda"}
               }
   :cs225-classroom
               {:desc "This is the classroom for CS 225 in the Cornfield University. Professor Hinda Ceeren is lecturing. The lecture is super interesting. You decide to pick up a lecturenote."
                :title "in the CS 225 classroom"
                :dir {:south :crop-science-building}
                :contents #{"lecturenote"}
                }
   :apartment  {:desc "A nice apartment. It looks a bit old from the outside though. There are snacks and a TV in the living room."
                :title "in your apartment."
                :dir {:east :crop-science-building}
                :contents #{"snack"}
                }

   :death    {:desc "As you take your last breath, you feel relieved of your burdens. You only have your soul left."
              :title "dead"
              :dir {}
              :contents #{"soul"}}
   })


(def adventurer
  {:location :spawn-room
   :inventory #{}
   :tick 0
   :seen #{}
   :level 1
   :move-bed 0
   :break-window 0})

;(defn print-content-helper [key]
;  (println (str "There is a " (name key) ".")))
;
;(defn print-content [player]
;  (let [location (player :location)
;        contents ((the-map location) :contents)]
;    (map print-content-helper contents)
;    )
;  )

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    ;(print-content player)
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)
        broken (player :break-window)]
    (if (or (nil? dest) (and (= dest :garden) (= broken 0)))
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn print-inventory [player]
  (let [inventory (player :inventory)]
    (if (empty? inventory)
      (do (println "Your inventory is empty.")
          player)
      (do (println inventory) player))))

;Sleep is doing nothing.
(defn sleep [player]
  (println "You feel better after a dreamless sleep.")
  player)

; If obj is in the contents of current location,
; move obj to the inventory
(defn pick-up [obj player]
  (let [inventory (player :inventory)
        location (player :location)
        contents (-> the-map location :contents)
        moved (player :move-bed)]
    (if (and (contents obj) (not (and (= obj "box") (= moved 0)))) ; if the bed is not moved, you cannot get the locked box.
      (do (println "Taken.")
          (update-in player [:inventory] #(conj % obj)))
          ;(update-in (the-map location) [:contents] #(disj % obj))
          ;(print-inventory player)
          ;player)
      (do (println "There is no such item/the item is not obtainable.") player))))

(defn read_book [player]
  (let [inventory (player :inventory)]
  (if (inventory "book")
    (do (println "These are written in some ancient language. The only phrase that you can recognize is \"computer science\"")
        player)
    (do (println "You don't have a book.")
        player))))

(defn read_paper [player]
  (let [inventory (player :inventory)]
    (if (inventory "paper")
      (do (println "HANAMURA DAILY \n 14/12/16 \nDo not go gentle into that good night,\nOld age should burn and rave at close of day;\nRage, rage against the dying of the light.\n.\n.\n.\nPPAP!\nI have a pen. I have an apple. Ugh! Apple pen!...\nI have a pen. I have pineapple. Ugh! Pineapple pen!...\nApple pen! Pineapple pen!\nUgh!\nPen Pineapple Apple Pen!\n\nThis game is created by Jianfeng Xia using Clojure. The finals went well, but more challenges are yet to come.\nHint: You can use 'quit' to exit this endless loop!")
          player)
      (do (println "You don't have a paper.")
          player))))

;It is a bad choice to eat a bad apple.
(defn eat_apple [player]
  (let [inventory (player :inventory)]
    (if (inventory "apple")
      (do (println "You ate the apple! Unfortunately you died because of the poisonous apple.")
          (assoc-in player [:location] :death))
      (do (println "You don't have an apple.")
          player))))

(defn eat-pizza [player]
  (let [inventory (player :inventory)]
    (if (inventory "pizza")
      (do (println "You ate the pizza! You didn't hav lunch this morning. It really saves your life.")
          player)
      (do (println "You don't have a pizza.")
          player))))

(defn drink-soda [player]
  (let [inventory (player :inventory)]
    (if (inventory "soda")
      (do (println "You drank the soda! Be careful, it's bad for you teeth.")
          player)
      (do (println "You don't have a soda.")
          player))))

(defn attack [enemy player]
  (let [inventory (player :inventory)
        location (player :location)
        map-location (the-map location)
        enemy (-> the-map location :enemy)
        playerlevel (player :level)
        enemylevel (-> the-map location :enemylevel)]
    (if (= nil enemy)
      (do (println "There is no enemy.")
          player)
      (if (and (inventory "sword") (> playerlevel enemylevel))
         (do (println (str "Of course you can fight a " enemy "! You killed the " enemy " and leveled up! You are now level " playerlevel ". After a about 100 years, the " enemy "respawned. But don't worry, you know you can defeat it!"))
             (assoc-in map-location [:enemy] nil)
             (if (= enemy "Roshan") (println "Congratulations! You have beaten Roshan! As you know, it respawns every 100 years. So you can wait 100 years to be the hero again! If you're tired, just type 'quit' to exit this endless loop!\n100 years later...................."))
             (update-in player [:level] #(inc %)))
         (do (println (str "Sadly, you died fighting this " enemy))
             (assoc-in player [:location] :death))))))

(defn move [player]
  (let [location (player :location)
        map-location (the-map location)
        moved (player :move-bed)]
    (if (= moved 0)
      (do (println "You have moved the bed. There is a locked box here.")
          (assoc-in player [:move-bed] 1))
      (do (println "The bed has been moved.")))))

(defn open-box [player]
  (let [inventory (player :inventory)]
    (if (inventory "box")
      (do (println "It looks like you need to enter a password to open this box. The password contains 4 uppercase letters. You feel like you know the answer or you recently read it somewhere. But what is it?...")
          (println "Please enter the password below:")
          (let [password (read-line)]
          (if (= password "PPAP")
            (do (println "Yay! You opened the box and got a hammer! A handy tool to break a window!")
                (update-in player [:inventory] #(conj % "hammer"))
                )
            (do (println "The password seems incorrect.")
                player))))
      (do (println "You don't have a box.")
          player))))

(defn break-window [player]
  (let [inventory (player :inventory)
        location (player :location)]
    (if (and (inventory "hammer") (= location :room1) (= (player :break-window) 0))
      (do (println "You broke the window! A new world is open to you!")
          (assoc-in player [:break-window] 1))
      (do (println "There is no window/You don't have a hammer.")
          player) )))

(defn read-lecturenote [player]
  (let [inventory (player :inventory)]
    (if (inventory "lecturenote")
      (do (println "CS 225 Today's Announcements: \nMP10 Corn growing available. Due 12/25\nExam10: 12/18-12/19")
          player)
      (do (println "You don't have the lecturenote.")
          player))))

(defn eat-snack [player]
  (let [inventory (player :inventory)]
    (if (inventory "snack")
      (do (println "Thank you. Nothing is better than this after such a long day.")
          player)
      (do (println "You don't have a snack.")
          player))))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         [(:or :n :north)] (go :north player)
         [(:or :s :south)] (go :south player)
         [(:or :w :west)] (go :west player)
         [(:or :e :east)] (go :east player)
         [(:or :d :down)] (go :down player)
         [(:or :u :up)] (go :up player)
         [:jump] (go :jump player)
         [(:or :inventory :i)] (print-inventory player)
         [:sleep] (sleep player)
         [:pick :up _] (pick-up (name (command 2)) player)
         [:read :book] (read_book player)
         [:read :paper] (read_paper player)
         [:eat :apple] (eat_apple player)
         [(:or :attack :fight :kill) _] (attack (command 1) player)
         [:move :bed] (move player)
         [:open :box] (open-box player);
         [:break :window] (break-window player)
         [:eat :pizza] (eat-pizza player)
         [:drink :soda] (drink-soda player)
         [:read :lecturenote] (read-lecturenote player)
         [:eat :snack] (eat-snack player)
         _ (do (println "I don't understand you.")
               player)))



(defn -main
  [& args]
  (println "You wake up in a room wearing an armor. There is a Japanese sword next to you.")
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (if (= command "quit")
        (println "Exiting...")
        (recur local-map (respond pl (to-keywords command)))))))
