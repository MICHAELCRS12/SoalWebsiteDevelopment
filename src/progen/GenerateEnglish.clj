(ns progen.GenerateEnglish
  (:gen-class))


;Fungsi generator opsi
(defn gen-options [xs]

  (let [alphabet (mapv str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        xs-part  (map vec (shuffle (partition 2 xs)))
        xs-pair  (mapv #(conj %2 %) (range) xs-part)
        xs-true  (filterv #(= true (% 0)) xs-pair)]

    {:option xs-pair
     :jawaban (alphabet (get-in xs-true [0 2]))}))

;Fungsi Mapping
(defn mapping
  [maps]
  (let [{:keys [topic pembahasan soal]}
        maps {:keys [soal-text options]} soal]

    {:topic topic
     :pembahasan pembahasan
     :soal
     (merge
      {:soal-text soal-text}
      (gen-options options))
     :probelm-id (clojure.string/replace (.toString (java.util.UUID/randomUUID)) #"-" "")}))

;Fungsi generator soal
(defn generate [i xs]
  (->> xs
       (repeatedly i)
       (#(if (every? map? %)
           (map mapping %)
           (throw (Exception. "input must be a seq of maps"))))
       vec))


;SOAL 1
(generate
 10
 (fn []
   (let [topic :english

         a  (rand-nth ["Joni" "Aulia" "Rachel" "Jose" "Messi" "Leo" "Yayan" "Ahmad" "Leni" "Adit" "Tono" "Tio" "Aryo" "Arya" "he" "she"])
         b  (rand-nth ["uses" "brings" "buys" "sells" "has" "sees"])
         c  (rand-nth ["toaster" "rice cooker" "stove" "kettle" "water purifier" "vacuum cleaner" "electric fan" "television" "air conditioner" "oven/grill/roaster" "washing machine" "refrigerator" "speaker" "hair dryer" "freezer" "iron" "torch/flashlight" "air cooler" "gas stove" "electric stove"])

         pb  (str b)   ;jawaban benar
         ps  (str a)   ;jawaban salah satu
         ps2 (str c)   ;jawaban salah dua

         soal (str "\n\n" a " " b " " c " <br>\n Dari sentence di atas, kira-kira yang mana verbsnya? \n</p>\n\n\n")
         pem  (str "\n\nKalimat yang ada di soal merupakan kalimat yang menggunakan simple present tense. <br>\n Dimana, pada tense ini, " a " merupakan subject, " b " merupakan verb, dan " c " merupakan object. <br>\n Jadi, jawabanya adalah " b ".\n")]

     {:topic topic
      :soal
      {:soal-text soal
       :options
       [true pb
        false ps
        false ps2]}
      :pembahasan pem})))