(ns minimi-virit-graafi.core
  (:gen-class))

;;Ohjelmassa luetaan koordinaatit tiedostosta pisteet_data.txt. Pisteet ovat tiedostossa muodossa:
;1,2
;0,0
;15,18
;Missä yhden pisteen koordinaatit ovat yhdellä rivillä ja rivin ensimmäinen luku on x-koordinaatin arvo ja toinen luku y-koordinaatin arvo.
;Ohjelmassa oletetaan, että kaikkien pisteiden välillä on kaari. Näistä kaarista valitaan kaaria, siten että syntyy minimi virittävä graafi, jonka tiedot tulostetaan
;tiedostoon min-virit-graafi.txt.


(defn string-to-int 
  [string]
  (Integer. string))
  
(defn parse 
  [string] 
  (map #(clojure.string/split % #",") 
    (clojure.string/split string #"\n")))

;Jos tiedostossa on rivit:
;1,2
;3,4
;5,6
;Tuloksena on vektori: [["1" "2"] ["3" "4"] ["5" "6"]]
(defn datan-luku
  "Funktio, joka lukee tiedostosta ja jakaa tiedot riven ja pilkkujen mukaan"
  [tiedoston-nimi]
  (into [] (parse (slurp tiedoston-nimi))))

(defn eka-termi 
  ([[x y]] x)) 

(defn toka-termi
  ([[x y]] y)) 

(defn sekvenssi-mapiksi
  "Muuttaa muotoa [luku luku] olevan sekvenssin mapiksi {:x luku, :y luku}"
  ([alkio] (sekvenssi-mapiksi [] alkio))
  ([sekvenssi alkio] 
   (conj sekvenssi {:x (string-to-int (eka-termi alkio)) :y (string-to-int (toka-termi alkio))})))
 
(defn sekvensit-mapeiksi
  "Muuttaa sekvenssin sisältämät sekvenssi alkiot mapeiksi ja muodostaa näistä vektorin"
  ([sekvenssi] 
   (reduce sekvenssi-mapiksi [] sekvenssi)))

;Jos tiedostossa on rivit:
;1,2
;3,4
;5,6
;Tuloksena on vektori, jossa on mapit:
;[{:x 1, :y 2} {:x 3, :y 4} {:x 5, :y 6}]
(defn pisteiden-koordinaatit
  "Yhdistää funktiot datan-luku ja sekvensit-mapeiksi"
  [tiedoston-nimi] 
  (sekvensit-mapeiksi (datan-luku tiedoston-nimi)))

(defn etaisyys
    "Laskee kahden pisteen välisen etäisyyden tasossa, missä syötteet on muotoa  {:x luku, y:luku} [{:x luku, y:luku}...] "
    [alkio [talkio]] 
    (Math/sqrt (+ (Math/pow (- (alkio :x) (talkio :x) ) 2) (Math/pow (- (alkio :y) (talkio :y)) 2))))


;Kyseinen funktio laskee etäisyyden alkion ja sekvenssin jokaisen alkion välillä ja antaa alkioille indeksit.
;Syöteen oletetaan olevan muotoa {:x luku, :y luku} [{:x luku, y:luku}...] j, missä j on kokonaisluku. 
;Funktion mielekäs käyttö perustuu ideaan että on olemassa alkuperäinen sekvenssi, missä alkion järjestys on sama kuin syötteen j arvo.
;Syötteenä olevassa sekvenssissä on taas ne alkiot, jotka tulevat syöte alkion j:n jälkeen alkuperäisessä sekvenssissä. 
;Funktion tuloksena on vektori:
;[{:distance luku, :lahto j, :loppu j+1} {:distance luku, :lahto j, :loppu j+2}...{:distance luku, :lahto j, :loppu j+listan pituus}]
(defn etaisyys-sekvenssi
  "Laskee etäisyyden alkion ja sekvenssin jokaisen alkion välillä ja antaa alkioille indeksit."
  ([alkio sekvenssi j] 
   (etaisyys-sekvenssi [] alkio sekvenssi (+ j 1) j))
  ([tulos alkio sekvenssi i j]
   (if (empty? sekvenssi) 
     tulos 
     (etaisyys-sekvenssi
       (conj tulos {:distance (etaisyys alkio sekvenssi) :lahto j :loppu i}) 
       alkio 
       (into [] (rest sekvenssi)) 
       (inc i) 
       j))))



;Kyseinen funktio muodostaa vektorin jossa on laskettu etäisyys kaikkien pisteparien välillä.
;Tulos on muotoa: 
;[{:distance luku, :lahto 0, :loppu 1} {:distance luku :lahto 0, :loppu 2}... {:distance luku, :lahto 1, :loppu 2} {:distance luku, :lahto 1, :loppu 3}...] 
;lahto ja loppu luku viittaa sekvenssin pisteen sijaintiin syötteenä olevassa sekvenssissä. Esim {:distance luku, :lahto 0, :loppu 1} 
;kertoo etäisyyden sekvenssin ensimmäisen ja toisen alkion välillä.
(defn kaikki-kaaret
  "Laskee etäisyydet kaikkien pisteparien välillä"
  ([sekvenssi]
   (kaikki-kaaret [] sekvenssi 0))
  ([tulos sekvenssi indeksi] ;;sek-loppuosa tarkoittaa sekvenssin loppuosaa
   (if 
     (empty? (rest sekvenssi)) 
     tulos 
     (kaikki-kaaret      ;Käydään läpi järjestyksessä kaikki listan alkiot
       (into tulos (etaisyys-sekvenssi (first sekvenssi) (rest sekvenssi) indeksi)) ;Huom jokaisella kieroksella on aina voimassa sekvenssi = sek-loppuosa
       (rest sekvenssi)  
       (inc indeksi)))))

(defn distancen-luku
    "Olettamuksena, että syöte on mappi"
    [mappi] (mappi :distance))

(defn sort-distance
    "Järjestää sekvenssin mapit etäisyyksien mukaan"
    [sekvenssi]
    (sort-by distancen-luku sekvenssi))

(defn vertailu
  "Tarkistaa onko alkio-mapin lahto ja loppu indeksit jo joukossa. Palauttaa true jos molemmat indeksit on jo joukossa"
  [alkio-mappi joukko]
  (and (some #(= % (alkio-mappi :lahto)) joukko) (some #(= % (alkio-mappi :loppu)) joukko)))

(defn kaarien-poisto 
  "Palauttaa sekvenssin mapit joiden lahto tai loppu arvot ei ole syötteen joukossa."
  [sekvenssi joukko] 
  (filter #(not (vertailu % joukko)) sekvenssi))

(defn mahdolliset-kaaret
  "Palauttaa vain ne sekvenssin mapit joiden lahto tai loppu on syötteen joukossa"
  [sekvenssi joukko]
  (into (filter #(not= nil (joukko (:lahto %))) sekvenssi) 
        (filter #(not= nil (joukko (:loppu %)))) sekvenssi))

(defn seuraavaksi-add
  "Palauttaa pienimmän alkion sekvenssistä, joille lahto tai loppu on syötteen joukossa, mutta ei molemmat"
  [sekvenssi joukko]
  (apply min-key :distance (mahdolliset-kaaret (kaarien-poisto sekvenssi joukko) joukko)))



;Syötteen oletetaan sisältävän kaikki graafin kaaret ja niiden etäisyydet.
;Syötteen oletetaan olevan muotoa 
;[{:distance luku, :lahto 0, :loppu 1} {:distance luku :lahto 0, :loppu 2}... {:distance luku, :lahto 1, :loppu 2} {:distance luku, :lahto 1, :loppu 3}...]
(defn minimi-graafi
  "Muodostaa minimi virittävän graafin syötteenä olevasta sekvenssistä. Eli muodostaa pienimmän virittävän puun"
    ([sekvenssi listan-pituus] ;listan pituus tarkoittaa graafin pisteiden määrää
     (let [x (apply min-key :distance sekvenssi)] ;valitaan lyhin kaari
       (minimi-graafi 
         sekvenssi 
         (conj #{} (x :lahto) (x :loppu)) ;lisätään käytyjen pisteiden indeksit
         (- listan-pituus 1) 
         (conj [] x))))
    ([sekvenssi joukko laskuri tulos]
     (if
       (= 1 laskuri) ;lisätään kaaria kunnes ollaan lisätty listan-pituus - 1 kaarta
       tulos
       (let [y (seuraavaksi-add sekvenssi joukko)]
         (minimi-graafi
           (kaarien-poisto sekvenssi joukko) ;Poistetaan tarkastelusta kaaret, joiden molemmat pisteet kuuluvat jo graafiin
           (conj joukko (:lahto y) (:loppu y)) ;lisätään joukkoon seuraavaksi lisättävän pisteen indeksit
           (- laskuri 1)
           (conj tulos y)))))) ;"Lisätään pienimmän etäisyyden omaava alkion sekvenssistä, joille lahto tai loppu on syötteen joukossa, mutta ei molemmat"

;;Syötteen oletetaan muotoa [{:x 1, :y 1} {:x 5, :y 8} {:x 10, :y 12} {:x 4, :y 3}..]
(defn yhdistys
  [sekvenssi]
  "Yhdistetään funktiot minimi-graafi ja kaikki kaaret"
  (minimi-graafi (kaikki-kaaret sekvenssi) (count sekvenssi)))
         
;;Liittää alkioon kyseisen alkion koordinaatit syöte vektorista. Alkion oletetaan olevan muotoa {:distance 2.8284271247461903, :lahto 0, :loppu 1}
;;ja vektorin muotoa [{:x 1, :y 2} {:x 3, :y 4} {:x 5, :y 6}]. Tulos on muotoa {:alkuid 0, :alku [1 2], :loppuid 1, :paaty [3 4]}.
;Oletuksena on, että alkiot ovat vektorissa, siinä järjestyksessä, että etäisyydet täsmäävät yhdistämisen jälkeen
(defn liitos 
  "Liittää alkioon kyseisen alkion koordinaatit syöte vektorista."
  [alkio vektori tulos] 
  (conj tulos {:alkuid (alkio :lahto)  :alku-koord [((get vektori (alkio :lahto)):x) ((get vektori (alkio :lahto)):y)] 
               :loppuid (alkio :loppu)  :loppu-koord [((get vektori (alkio :loppu)):x) ((get vektori (alkio :loppu)):y)] 
               :distance (alkio :distance)}))  


;;Koordinaatit vektori oletetaan olevan muotoa  [{:x 1, :y 2} {:x 3, :y 4} {:x 5, :y 6}] ja alkio-listan muotoa 
;[{:distance 2.8284271247461903, :lahto 0, :loppu 1} {:distance 2.8284271247461903, :lahto 1, :loppu 2}]
(defn liitos-sekvenssille 
  "Liittää sekvenssin alkioille kyseisen alkion koordinaatit syöte vektorista."
    ([koordinaatit-vektori alkio-lista] 
     (liitos-sekvenssille koordinaatit-vektori  alkio-lista []))
    ([koordinaatit-vektori  alkio-lista tulo] 
     (if (empty? alkio-lista) 
      tulo 
      (recur 
        koordinaatit-vektori 
        (rest alkio-lista) 
        (liitos (first alkio-lista) koordinaatit-vektori  tulo)))))



(defn tiedostosta-tulos
  [tiedoston-nimi]
  "Lukee tiedosta koordinaatit ja muodostaa minimi virittävän graafin"
  (liitos-sekvenssille 
    (pisteiden-koordinaatit tiedoston-nimi)
    (yhdistys (pisteiden-koordinaatit tiedoston-nimi))))



;Funktio tulostaa vektorin tiedot tiedostoon, joka on toisena syötteenä
;Syöteen oletetaan olevan muotoa on muotoa:
;[{:alkuid 0, :alku-koord [1 2], :loppuid 1, :loppu-koord [3 4], :distance 2.8284271247461903} 
;{:alkuid 1, :alku-koord [3 4], :loppuid 2, :loppu-koord [5 6], :distance 2.8284271247461903}] "tiedoston-nimi"

(defn pisteiden-tulostus 
  [vektori tiedoston-nimi] 
  (if 
    (empty? vektori) 
    nil 
    (do 
      (let [fv (first vektori)] 
        (with-open [w (clojure.java.io/writer tiedoston-nimi :append true)]
          (.write w 
           (str "Lähtopiste:" " järjestysnumero:"  (fv :alkuid)  " x-koordi:"(get (fv :alku-koord) 0) " y-koordi:"(get (fv :alku-koord) 1) 
             " || Loppupiste:" " järjestysnumero:" (fv :loppuid)  " x-koordi:"(get (fv :loppu-koord) 0) " y-koordi:"(get (fv :loppu-koord) 1)
             " || Pisteiden välinen etäisyys:" (fv :distance)))
         (.newLine w)))
      (recur (rest vektori) tiedoston-nimi))))

(defn graafin-tulostus
  [luku-tiedosto syote-tiedosto]
  "Yhdistää pisteiden-tulostu ja tiedosta-tulos funktiot"
  (pisteiden-tulostus (tiedostosta-tulos luku-tiedosto) syote-tiedosto))

(defn graafin-tulostus
  [luku-tiedosto syote-tiedosto]
  "Yhdistää pisteiden-tulostu ja tiedosta-tulos funktiot"
  (if (> (count (pisteiden-koordinaatit luku-tiedosto)) 1)
    (pisteiden-tulostus (tiedostosta-tulos luku-tiedosto) syote-tiedosto)
    (println "Tiedostassa vain yksi piste.")))




;Kirjoittaa pisteiden koordinaatit ja lisätään järjestys numeron listan luvuille. Järjestys pysyy samana kuin syöte listassa.
;Syötteen oletetaan olevan muotoa [{:x luku, :y luku}...{:x luku, :y luku}]
(defn alku-jarjestys
  "Kirjoittaa pisteiden koordinaatit ja lisätään järjestys numeron listan luvuille."
  ([piste-lista tiedoston-nimi] (alku-jarjestys piste-lista 0 tiedoston-nimi))
  ([piste-lista jar-num tiedoston-nimi] 
   (if (empty? piste-lista) 
     (with-open [w (clojure.java.io/writer tiedoston-nimi :append true)] (.newLine w)) 
     (do 
       (with-open [w (clojure.java.io/writer tiedoston-nimi :append true)] 
         (.write w 
           (str "Järjestysnumero:" jar-num " || koordinaatit: x:" ((first piste-lista) :x) " y:"  ((first piste-lista) :y))) 
         (.newLine w)) 
       (recur (rest piste-lista) (inc jar-num) tiedoston-nimi)))))

(defn alku-jarjestys-tulostus
  "Yhdistää funktiot alku-jarjestys ja pisteiden-koordinaatit"
  [luku-tiedosto syote-tiedosto]
  (alku-jarjestys (pisteiden-koordinaatit luku-tiedosto) syote-tiedosto))
 
 

  

(defn -main  []
  
  ;alku-jarjestys-tulostus:
  ;Lukee tiedosta koordinaatit ja tulostaa koordinaatit samassa järjestyksessä ja lisää järjestysnumerot.
  ;;Ensimmäinen argumentti on tiedoston nimi, josta tiedot luetaan ja jälkimmäinen johon tulostetaan
  
  ;graafin tulostus:
  ;Lukee tiedosta koordinaatit ja tulostaa minimi virittävän graafin
  ;Ensimmäinen argumentti on tiedoston nimi, josta tiedot luetaan ja jälkimmäinen johon tulostetaan
  (try 
    (alku-jarjestys-tulostus "pisteet_data.txt" "min_virit_graafi.txt") 
    (graafin-tulostus "pisteet_data.txt" "min_virit_graafi.txt")
    (catch Exception e (println (str "Tarkista tiedoston sisalto || " "caught exception: " (.getMessage e))))))
