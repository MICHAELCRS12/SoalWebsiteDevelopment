(ns progen.GenerateVerbal
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
   (let [topic :verbal-logic

         a  (rand-nth ["Sandal" "Rumah" "Tas" "Topi" "Lukisan" "Jam tangan" "Sepatu" "Handphone" "Mobil" "Motor" "Laptop" "Baju" "Celana" "Cermin" "Gaun" "Kamera" "Lantai" "Majalah" "Kulkas" "Pintu" "Vas"]) 
         b  (rand-nth ["mewah" "mahal" "murah" "bagus" "norak" "rusak" "besar" "kecil" "bersih" "kotor" "asli" "palsu" "baru" "lama" "cerah" "kusam" "rapi" "berantakan" "hitam" "putih"])
         
         pb ( str "non pertanyaan")         ;jawaban benar
         ps (str "pertanyaan")              ;jawaban salah
         
         soal (str "\n\n" a " " b " <br>\n Bentuk di atas merupakan ... \n</p>\n\n\n")
         pem  (str "\n\nPernyataan itukan kalimat yang bernilai benar atau salah. <br>\n Jadi kalo sesuatu itu bukan kalimat, ya bukan pernyataan yaaaa. <br>\n Kalimat itu minimal ada subjek dan predikat, kalau frasa itu menempati satu posisi aja (misal subjek aja), jadi bukan kalimat. \n")]

     {:topic topic
      :soal
      {:soal-text soal
       :options
       [true pb
        false ps]}
      :pembahasan pem})))

;Soal 2

(generate
 10
 (fn []
   (let [topic :verbal-logic

         a  (rand-nth ["Joni" "Aulia" "Rachel" "Jose" "Messi" "Leo" "Yayan" "Ahmad" "Leni" "Adit" "Nio" "Tio" "Aryo" "Arya"])
         b  (rand-nth ["supir" "pilot" "pengangkat galon" "ayah" "kuli bangunan" "kurir" "satpam" "montir" "pembalap" "nelayan"])
         c  (rand-nth ["laki-laki orang dewasa" "orang yang punya uang" "orang yang punya KTP" "orang yang kuat" "orang yang berpenghasilan" "kepala keluarga" "tulang punggung keluarga" "orang yang terampil" "orang yang berpengalaman" "orang yang berkecukupan"])
         d  (rand-nth ["Kalimat mana yang diperlukan untuk melengkapi argumen?" "Kalimat manakah yang dibutuhkan untuk melengkapi argumen?" "Untuk melengkapi argumen, kalimat mana yang diperlukan?"])

         pbnr (str "semua " b " itu " c ". ")         ;jawaban benar
         psas (str "semua " c " itu " b ". ")         ;jawaban salah 1
         psad (str "sebagian " b " itu " c ". ")      ;jawaban salah 2
         psat (str "sebagian " c " itu " b ". ")      ;jawaban salah 3
         psae (str "Argumen sudah lengkap.")          ;jawaban salah 4

         soal (str "\n\n" a " adalah seorang " b ", jadi " a " itu " c " <br>\n " d " \n</p>\n\n\n")
         pem  (str "\n\nArgumen ini tuh ga lengkap. Kenapa? <br>\n Soalnya ada hal yang gak disebutin, dengan anggapan 'ah kamu pasti ngerti' atau 'ah semua orang pasti tau lah', <br>\n padahal ga boleh gitu, karena jadinya asumsi. <br>\n Makanya perlu dilengkapin. " a " adalah seorang " b ", jadi " a " " c ". <br>\n Berarti pertama kalian tau kalo " a " itu " b ", <br>\n terus apa bisa langsung ambil kesimpulan " a " itu " c "? <br>\n Gak bisa ya, kurang infonya. Kalian harus tambahin apa nih? <br>\n Tambahin hubungan " b " sama " c ", terus tinggal pilih hubungan " b " sama " c " mana, <br>\n yang bikin kesimpulan " a " itu " c " jadi bener dan tidak tergoyahkan deh intinya. Gitu ya, selamat mencoba~ \n")]

     {:topic topic
      :soal
      {:soal-text soal
       :options
       [true pbnr
        false psas
        false psad
        false psat
        false psae]}
      :pembahasan pem})))