(ns minimi-virit-graafi.core-test
  (:require [clojure.test :refer :all]
            [minimi-virit-graafi.core :refer :all]))

;;testi.txt tiedostossa on rivit:
;1,2
;3,4
;5,6

(deftest datanluku-test
  (testing  
    (is (= (datan-luku "testi.txt") [["1" "2"] ["3" "4"] ["5" "6"]])))) 


(deftest string-to-int-test-adder
  (testing  
   (is (= (string-to-int "3") 3))))


(deftest sekvenssi-mapiksi-test
  (testing  
   (is (= (sekvenssi-mapiksi (seq [1 2])) [{:x 1, :y 2}]))))

(deftest sekvensit-mapeiksi-test
  (testing  
   (is (= (sekvensit-mapeiksi (seq [(seq [1 2]) (seq [3 4])])) [{:x 1, :y 2} {:x 3, :y 4}]))))

(deftest pisteiden-koordinaatit-test
  (testing  
   (is (= (pisteiden-koordinaatit "testi.txt") [{:x 1, :y 2} {:x 3, :y 4} {:x 5, :y 6}]))))


(deftest pisteiden-koordinaatit-test
  (testing  
   (is (= (etaisyys {:x 5, :y 3} [{:x 8, :y 7}]) 5.0))))

(deftest etaisyys-sekvenssi-test
  (testing  
   (is (= (etaisyys-sekvenssi {:x 1, :y 2}  [{:x 3, :y 4} {:x 5, :y 6}] 0) 
          [{:distance 2.8284271247461903, :lahto 0, :loppu 1} {:distance 5.656854249492381, :lahto 0, :loppu 2}]))))

(deftest kaikki-kaaret-test
  (testing
    (is (= (kaikki-kaaret [{:x 1, :y 2} {:x 3, :y 4} {:x 5, :y 6}])
           [{:distance 2.8284271247461903, :lahto 0, :loppu 1} 
            {:distance 5.656854249492381, :lahto 0, :loppu 2} {:distance 2.8284271247461903, :lahto 1, :loppu 2}]))))

(deftest vertailu-test
  (testing
    (is (= (vertailu {:distance 2.8284271247461903, :lahto 0, :loppu 2} #{1 2 3} ) nil))
    (is (= (vertailu {:distance 2.8284271247461903, :lahto 0, :loppu 2} #{0 1 2 3} ) true))))

(deftest kaarien-poisto-test
  (testing
    (is (= (kaarien-poisto 
             [{:distance 2.8284271247461903, :lahto 0, :loppu 1} {:distance 5.656854249492381, :lahto 0, :loppu 2}]
             #{0 1 3}) [{:distance 5.656854249492381, :lahto 0, :loppu 2}]))))

(deftest seuraavaksi-add-test
  (testing
    (is (= (seuraavaksi-add 
             [{:distance 2.8284271247461903, :lahto 0, :loppu 1} 
              {:distance 5.656854249492381, :lahto 0, :loppu 2} 
              {:distance 2.8284271247461903, :lahto 1, :loppu 2}] #{0 1 3})
           {:distance 2.8284271247461903, :lahto 1, :loppu 2}))))

;(deftest liitos-sekvenssille-test
  ;(testing
    ;(is (=(liitos-sekvenssille [{:x 1, :y 2} {:x 3, :y 4} {:x 5, :y 6}] [{:distance 2.8284271247461903, :lahto 0, :loppu 1} {:distance 2.8284271247461903, :lahto 1, :loppu 2}]) [{:alkuid 0, :alku-koord [1 2], :loppuid 1, :loppu-koord [3 4], :distance 2.8284271247461903} {:alkuid 1, :alku-koord [3 4], :loppuid 2, :loppu-koord [5 6], :distance 2.8284271247461903}]))))

(deftest liitos-sekvenssille-test
  (testing
    (is (=(liitos-sekvenssille [{:x 1, :y 2} {:x 3, :y 4} {:x 5, :y 6}] [{:distance 2.8284271247461903, :lahto 0, :loppu 1} {:distance 2.8284271247461903, :lahto 1, :loppu 2}]) 
         [{:alkuid 0, :alku-koord [1 2], :loppuid 1, :loppu-koord [3 4], :distance 2.8284271247461903} 
          {:alkuid 1, :alku-koord [3 4], :loppuid 2, :loppu-koord [5 6], :distance 2.8284271247461903}]))))



;testi2.txt tiedostossa on rivit 
;5,10
;1,2
;8,7
(deftest tiedostosta-tulos-test
  (testing
    (is (= (tiedostosta-tulos "testi2.txt") 
           [{:alkuid 0, :alku-koord [5 10], :loppuid 2, :loppu-koord [8 7], :distance 4.242640687119285} 
            {:alkuid 1, :alku-koord [1 2], :loppuid 2, :loppu-koord [8 7], :distance 8.602325267042627}]))))

