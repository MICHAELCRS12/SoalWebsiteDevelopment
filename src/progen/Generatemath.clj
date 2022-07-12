(ns progen.Generatemath
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
   (let [topic :math

         a  (+ 1 (rand-int 51))  ;a
         x  (+ 1 (rand-int 51))  ;beda
         b  (+ a x)         ;u2
         c  (+ b x)         ;u3
         d  (+ c x)         ;u4
         pb (+ d x)         ;jawaban benar
         ps (+ d x (+ 1 (rand-int 11))) ;jawaban salah1
         psd (+ d x (+ 1 (rand-int 11)) (rand-int 10)) ;jawaban salah2
         pst (+ d x (+ 1 (rand-int 11)) (rand-int 10) (rand-int 9)) ;jawaban salah3

         soal (str "\n\nCoba, bisa nggak lanjutin pola bilangan ini? <br>\n " a ", " b ", " c ", "d", ... \n</p>\n\n\n")
         pem  (str "\n\nUntuk ngerjain ini, kita bisa pakai konsep pola aritmatika. <br>\n Dimana untuk pola ini, a atau U1 (bilangan pertama) = " a " dan b (beda) = U2 - U1 = " b " - " a " = " x ". <br> \n Jadi untuk ke bilangan selanjutnya, tinggal ditambah " x " aja. <br>\n Sehingga pola selanjutnya itu adalah " pb ". \n")]

     {:topic topic
      :soal
       {:soal-text soal
        :options
         [true pb
          false ps
          false psd
          false pst]}
      :pembahasan pem})))



;SOAL 2
(generate
 10
 (fn []
   (let [topic :math

         nama (rand-nth ["Badi" "Badu" "Adi" "Ali" "Rio" "Tio" "Joko" "Dani" "Ani" "Ari" "Hani" "Lani" "Sasa" "Roni"])
         p  (+ 1 (rand-int 16))
         l  (+ 1 (rand-int 16))
         n  (+ 1 (rand-int 16))
         pbnr (* n 2 (+ p l))
         psal (+ pbnr (+ 1 (rand-int 16)))
         psald (+ pbnr (rand-int 11) (rand-int 10))
         psalt (+ pbnr (rand-int 11) (rand-int 10) (rand-int 9))

         soal (str "\n\nSi" nama " lari pagi mengelilingi lapangan berbentuk persegi panjang sebanyak " n " kali. <br>\n Jika panjang lapangan sebesar " p " m dan lebar lapangan sebesar " l " m, <br>\n berapa meterkah jarak yang ditempuh " nama "? \n</p>\n\n\n")
         pem  (str "\n\nUntuk ngerjain ini, kita bisa menggunakan rumus dari keliling persegi panjang. <br>\n Dimana pada soal, diketahui bahwa panjang = " p " m dan lebar = " l " m. <br> \n Jadi untuk mengerjakannya, kita kalikan " n " kali " nama " mengelilingi lapangan dengan keliling lapangan (2 x (" p " + " l ")). <br>\n Sehingga jarak yang ditempuh " nama " adalah " pbnr " m. \n")]

     {:topic topic
      :soal
      {:soal-text soal
       :options
       [true pbnr
        false psal
        false psald
        false psalt]}
      :pembahasan pem})))


;Soal 3
(generate
 10
 (fn []
   (let [topic :math
         
         u1  (rand-nth [2 3 4 5 6 7])
         r   (rand-nth [2 3]) 
         u2  (* u1 (Math/pow r 1))
         u3  (* u1 (Math/pow r 2))
         pbenar (* u1 (Math/pow r 3))
         psalah (+ pbenar (rand-int 11))
         psalahd (+ pbenar (rand-int 11) (rand-int 10))
         psalaht (+ pbenar (rand-int 11) (rand-int 10) (rand-int 9))

         soal (str "\n\nCoba, bisa nggak lanjutin pola bilangan ini? <br>\n " u1 ", " u2 ", " u3 ", ... \n</p>\n\n\n")
         pem  (str "\n\nUntuk ngerjain ini, kita bisa pakai konsep pola geometri. <br>\n Dimana untuk pola ini, a atau U1 (bilangan pertama) = " u1 " dan r (rasio) = U2 / U1 = " u2 " / " u1 " = " r ". <br> \n Jadi untuk ke bilangan selanjutnya, tinggal dikalikan r, sebesar " r ". <br>\n Sehingga pola selanjutnya itu adalah " pbenar ". \n")]
     
     {:topic topic
      :soal
      {:soal-text soal
       :options
       [true pbenar
        false psalah
        false psalahd
        false psalaht]}
      :pembahasan pem})))


