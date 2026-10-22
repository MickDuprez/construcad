(ns modeller.sections-au
  "Australian hot rolled sections, taken from the ASI 'Hot Rolled and
  Structural Steel Products', 8th edition (March 2019), Tables 9, 11, 15, 17,
  19 and 22. Dimensions in mm, mass in kg/m, gross area :Ag in mm2 (kept to
  check built outlines against). Tapered flange beams (Table 13) are not in
  yet: they need a tapered-flange builder.")

(def hot-rolled
  [
   {:section/designation "610UB125" :section/builder :i-section :mass 125 :d 611.6 :bf 229.0 :tf 19.6 :tw 11.9 :r1 14.0 :Ag 16000}
   {:section/designation "610UB113" :section/builder :i-section :mass 113 :d 607.0 :bf 228.0 :tf 17.3 :tw 11.2 :r1 14.0 :Ag 14500}
   {:section/designation "610UB101" :section/builder :i-section :mass 101 :d 602.0 :bf 228.0 :tf 14.8 :tw 10.6 :r1 14.0 :Ag 13000}
   {:section/designation "530UB92.4" :section/builder :i-section :mass 92.4 :d 533.0 :bf 209.0 :tf 15.6 :tw 10.2 :r1 14.0 :Ag 11800}
   {:section/designation "530UB82.0" :section/builder :i-section :mass 82.0 :d 528.2 :bf 209.0 :tf 13.2 :tw 9.6 :r1 14.0 :Ag 10500}
   {:section/designation "460UB82.1" :section/builder :i-section :mass 82.1 :d 460.4 :bf 191.0 :tf 16.0 :tw 9.9 :r1 11.4 :Ag 10500}
   {:section/designation "460UB74.6" :section/builder :i-section :mass 74.6 :d 457.4 :bf 190.0 :tf 14.5 :tw 9.1 :r1 11.4 :Ag 9520}
   {:section/designation "460UB67.1" :section/builder :i-section :mass 67.1 :d 453.8 :bf 190.0 :tf 12.7 :tw 8.5 :r1 11.4 :Ag 8580}
   {:section/designation "410UB59.7" :section/builder :i-section :mass 59.7 :d 406.4 :bf 178.0 :tf 12.8 :tw 7.8 :r1 11.4 :Ag 7640}
   {:section/designation "410UB53.7" :section/builder :i-section :mass 53.7 :d 402.6 :bf 178.0 :tf 10.9 :tw 7.6 :r1 11.4 :Ag 6890}
   {:section/designation "360UB56.7" :section/builder :i-section :mass 56.7 :d 358.6 :bf 172.0 :tf 13.0 :tw 8.0 :r1 11.4 :Ag 7240}
   {:section/designation "360UB50.7" :section/builder :i-section :mass 50.7 :d 355.6 :bf 171.0 :tf 11.5 :tw 7.3 :r1 11.4 :Ag 6470}
   {:section/designation "360UB44.7" :section/builder :i-section :mass 44.7 :d 352.0 :bf 171.0 :tf 9.7 :tw 6.9 :r1 11.4 :Ag 5720}
   {:section/designation "310UB46.2" :section/builder :i-section :mass 46.2 :d 307.2 :bf 166.0 :tf 11.8 :tw 6.7 :r1 11.4 :Ag 5930}
   {:section/designation "310UB40.4" :section/builder :i-section :mass 40.4 :d 304.0 :bf 165.0 :tf 10.2 :tw 6.1 :r1 11.4 :Ag 5210}
   {:section/designation "310UB32.0" :section/builder :i-section :mass 32.0 :d 298.0 :bf 149.0 :tf 8.0 :tw 5.5 :r1 13.0 :Ag 4080}
   {:section/designation "250UB37.3" :section/builder :i-section :mass 37.3 :d 256.2 :bf 146.0 :tf 10.9 :tw 6.4 :r1 8.9 :Ag 4750}
   {:section/designation "250UB31.4" :section/builder :i-section :mass 31.4 :d 251.6 :bf 146.0 :tf 8.6 :tw 6.1 :r1 8.9 :Ag 4010}
   {:section/designation "250UB25.7" :section/builder :i-section :mass 25.7 :d 248.0 :bf 124.0 :tf 8.0 :tw 5.0 :r1 12.0 :Ag 3270}
   {:section/designation "200UB29.8" :section/builder :i-section :mass 29.8 :d 207.0 :bf 134.0 :tf 9.6 :tw 6.3 :r1 8.9 :Ag 3820}
   {:section/designation "200UB25.4" :section/builder :i-section :mass 25.4 :d 203.2 :bf 133.0 :tf 7.8 :tw 5.8 :r1 8.9 :Ag 3230}
   {:section/designation "200UB22.3" :section/builder :i-section :mass 22.3 :d 201.6 :bf 133.0 :tf 7.0 :tw 5.0 :r1 8.9 :Ag 2870}
   {:section/designation "200UB18.2" :section/builder :i-section :mass 18.2 :d 198.0 :bf 99.0 :tf 7.0 :tw 4.5 :r1 11.0 :Ag 2320}
   {:section/designation "180UB22.2" :section/builder :i-section :mass 22.2 :d 179.0 :bf 90.0 :tf 10.0 :tw 6.0 :r1 8.9 :Ag 2820}
   {:section/designation "180UB18.1" :section/builder :i-section :mass 18.1 :d 175.0 :bf 90.0 :tf 8.0 :tw 5.0 :r1 8.9 :Ag 2300}
   {:section/designation "180UB16.1" :section/builder :i-section :mass 16.1 :d 173.0 :bf 90.0 :tf 7.0 :tw 4.5 :r1 8.9 :Ag 2040}
   {:section/designation "150UB18.0" :section/builder :i-section :mass 18.0 :d 155.0 :bf 75.0 :tf 9.5 :tw 6.0 :r1 8.0 :Ag 2300}
   {:section/designation "150UB14.0" :section/builder :i-section :mass 14.0 :d 150.0 :bf 75.0 :tf 7.0 :tw 5.0 :r1 8.0 :Ag 1780}
   {:section/designation "310UC158" :section/builder :i-section :mass 158 :d 327.2 :bf 311.0 :tf 25.0 :tw 15.7 :r1 16.5 :Ag 20100}
   {:section/designation "310UC137" :section/builder :i-section :mass 137 :d 320.6 :bf 309.0 :tf 21.7 :tw 13.8 :r1 16.5 :Ag 17500}
   {:section/designation "310UC118" :section/builder :i-section :mass 118 :d 314.6 :bf 307.0 :tf 18.7 :tw 11.9 :r1 16.5 :Ag 15000}
   {:section/designation "310UC96.8" :section/builder :i-section :mass 96.8 :d 308.0 :bf 305.0 :tf 15.4 :tw 9.9 :r1 16.5 :Ag 12400}
   {:section/designation "250UC89.5" :section/builder :i-section :mass 89.5 :d 260.0 :bf 256.0 :tf 17.3 :tw 10.5 :r1 14.0 :Ag 11400}
   {:section/designation "250UC72.9" :section/builder :i-section :mass 72.9 :d 253.8 :bf 254.0 :tf 14.2 :tw 8.6 :r1 14.0 :Ag 9320}
   {:section/designation "200UC59.5" :section/builder :i-section :mass 59.5 :d 209.8 :bf 205.0 :tf 14.2 :tw 9.3 :r1 11.4 :Ag 7620}
   {:section/designation "200UC52.2" :section/builder :i-section :mass 52.2 :d 206.4 :bf 204.0 :tf 12.5 :tw 8.0 :r1 11.4 :Ag 6660}
   {:section/designation "200UC46.2" :section/builder :i-section :mass 46.2 :d 203.4 :bf 203.0 :tf 11.0 :tw 7.3 :r1 11.4 :Ag 5900}
   {:section/designation "150UC37.2" :section/builder :i-section :mass 37.2 :d 161.8 :bf 154.0 :tf 11.5 :tw 8.1 :r1 8.9 :Ag 4730}
   {:section/designation "150UC30.0" :section/builder :i-section :mass 30.0 :d 157.6 :bf 153.0 :tf 9.4 :tw 6.6 :r1 8.9 :Ag 3860}
   {:section/designation "150UC23.4" :section/builder :i-section :mass 23.4 :d 152.4 :bf 152.0 :tf 6.8 :tw 6.1 :r1 8.9 :Ag 2980}
   {:section/designation "100UC14.8" :section/builder :i-section :mass 14.8 :d 97.0 :bf 99.0 :tf 7.0 :tw 5.0 :r1 10.0 :Ag 1890}
   {:section/designation "380PFC" :section/builder :channel :mass 55.2 :d 380 :bf 100 :tf 17.5 :tw 10.0 :r1 14.0 :Ag 7030}
   {:section/designation "300PFC" :section/builder :channel :mass 40.1 :d 300 :bf 90 :tf 16.0 :tw 8.0 :r1 14.0 :Ag 5110}
   {:section/designation "250PFC" :section/builder :channel :mass 35.5 :d 250 :bf 90 :tf 15.0 :tw 8.0 :r1 12.0 :Ag 4520}
   {:section/designation "230PFC" :section/builder :channel :mass 25.1 :d 230 :bf 75 :tf 12.0 :tw 6.5 :r1 12.0 :Ag 3200}
   {:section/designation "200PFC" :section/builder :channel :mass 22.9 :d 200 :bf 75 :tf 12.0 :tw 6.0 :r1 12.0 :Ag 2920}
   {:section/designation "180PFC" :section/builder :channel :mass 20.9 :d 180 :bf 75 :tf 11.0 :tw 6.0 :r1 12.0 :Ag 2660}
   {:section/designation "150PFC" :section/builder :channel :mass 17.7 :d 150 :bf 75 :tf 9.5 :tw 6.0 :r1 10.0 :Ag 2250}
   {:section/designation "125PFC" :section/builder :channel :mass 11.9 :d 125 :bf 65 :tf 7.5 :tw 4.7 :r1 8.0 :Ag 1520}
   {:section/designation "100PFC" :section/builder :channel :mass 8.33 :d 100 :bf 50 :tf 6.7 :tw 4.2 :r1 8.0 :Ag 1060}
   {:section/designation "75PFC" :section/builder :channel :mass 5.92 :d 75 :bf 40 :tf 6.1 :tw 3.8 :r1 8.0 :Ag 754}
   {:section/designation "310UBP149" :section/builder :i-section :mass 149 :d 318 :bf 316 :tf 20.6 :tw 20.5 :r1 16.5 :Ag 19000}
   {:section/designation "310UBP110" :section/builder :i-section :mass 110 :d 308 :bf 311 :tf 15.4 :tw 15.3 :r1 16.5 :Ag 14000}
   {:section/designation "310UBP78.8" :section/builder :i-section :mass 78.8 :d 299 :bf 306 :tf 11.1 :tw 11.1 :r1 16.5 :Ag 10100}
   {:section/designation "200UBP122" :section/builder :i-section :mass 122 :d 230 :bf 220 :tf 25.0 :tw 25.0 :r1 11.4 :Ag 15600}
   {:section/designation "200x200x26EA" :section/builder :angle :mass 76.8 :r1 18.0 :r2 5.0 :b 200 :t 26.0 :Ag 9780}
   {:section/designation "200x200x20EA" :section/builder :angle :mass 60.1 :r1 18.0 :r2 5.0 :b 200 :t 20.0 :Ag 7660}
   {:section/designation "200x200x18EA" :section/builder :angle :mass 54.4 :r1 18.0 :r2 5.0 :b 200 :t 18.0 :Ag 6930}
   {:section/designation "200x200x16EA" :section/builder :angle :mass 48.7 :r1 18.0 :r2 5.0 :b 200 :t 16.0 :Ag 6200}
   {:section/designation "200x200x13EA" :section/builder :angle :mass 40.0 :r1 18.0 :r2 5.0 :b 200 :t 13.0 :Ag 5090}
   {:section/designation "150x150x19EA" :section/builder :angle :mass 42.1 :r1 13.0 :r2 5.0 :b 150 :t 19.0 :Ag 5360}
   {:section/designation "150x150x16EA" :section/builder :angle :mass 35.4 :r1 13.0 :r2 5.0 :b 150 :t 15.8 :Ag 4520}
   {:section/designation "150x150x12EA" :section/builder :angle :mass 27.3 :r1 13.0 :r2 5.0 :b 150 :t 12.0 :Ag 3480}
   {:section/designation "150x150x10EA" :section/builder :angle :mass 21.9 :r1 13.0 :r2 5.0 :b 150 :t 9.5 :Ag 2790}
   {:section/designation "125x125x16EA" :section/builder :angle :mass 29.1 :r1 10.0 :r2 5.0 :b 125 :t 15.8 :Ag 3710}
   {:section/designation "125x125x12EA" :section/builder :angle :mass 22.5 :r1 10.0 :r2 5.0 :b 125 :t 12.0 :Ag 2870}
   {:section/designation "125x125x10EA" :section/builder :angle :mass 18.0 :r1 10.0 :r2 5.0 :b 125 :t 9.5 :Ag 2300}
   {:section/designation "125x125x8EA" :section/builder :angle :mass 14.9 :r1 10.0 :r2 5.0 :b 125 :t 7.8 :Ag 1900}
   {:section/designation "100x100x12EA" :section/builder :angle :mass 17.7 :r1 8.0 :r2 5.0 :b 100 :t 12.0 :Ag 2260}
   {:section/designation "100x100x10EA" :section/builder :angle :mass 14.2 :r1 8.0 :r2 5.0 :b 100 :t 9.5 :Ag 1810}
   {:section/designation "100x100x8EA" :section/builder :angle :mass 11.8 :r1 8.0 :r2 5.0 :b 100 :t 7.8 :Ag 1500}
   {:section/designation "100x100x6EA" :section/builder :angle :mass 9.16 :r1 8.0 :r2 5.0 :b 100 :t 6.0 :Ag 1170}
   {:section/designation "90x90x10EA" :section/builder :angle :mass 12.7 :r1 8.0 :r2 5.0 :b 90 :t 9.5 :Ag 1620}
   {:section/designation "90x90x8EA" :section/builder :angle :mass 10.6 :r1 8.0 :r2 5.0 :b 90 :t 7.8 :Ag 1350}
   {:section/designation "90x90x6EA" :section/builder :angle :mass 8.22 :r1 8.0 :r2 5.0 :b 90 :t 6.0 :Ag 1050}
   {:section/designation "75x75x10EA" :section/builder :angle :mass 10.5 :r1 8.0 :r2 5.0 :b 75 :t 9.5 :Ag 1340}
   {:section/designation "75x75x8EA" :section/builder :angle :mass 8.73 :r1 8.0 :r2 5.0 :b 75 :t 7.8 :Ag 1110}
   {:section/designation "75x75x6EA" :section/builder :angle :mass 6.81 :r1 8.0 :r2 5.0 :b 75 :t 6.0 :Ag 867}
   {:section/designation "75x75x5EA" :section/builder :angle :mass 5.27 :r1 8.0 :r2 5.0 :b 75 :t 4.6 :Ag 672}
   {:section/designation "65x65x10EA" :section/builder :angle :mass 9.02 :r1 6.0 :r2 3.0 :b 65 :t 9.5 :Ag 1150}
   {:section/designation "65x65x8EA" :section/builder :angle :mass 7.51 :r1 6.0 :r2 3.0 :b 65 :t 7.8 :Ag 957}
   {:section/designation "65x65x6EA" :section/builder :angle :mass 5.87 :r1 6.0 :r2 3.0 :b 65 :t 6.0 :Ag 748}
   {:section/designation "65x65x5EA" :section/builder :angle :mass 4.56 :r1 6.0 :r2 3.0 :b 65 :t 4.6 :Ag 581}
   {:section/designation "55x55x6EA" :section/builder :angle :mass 4.93 :r1 6.0 :r2 3.0 :b 55 :t 6.0 :Ag 628}
   {:section/designation "55x55x5EA" :section/builder :angle :mass 3.84 :r1 6.0 :r2 3.0 :b 55 :t 4.6 :Ag 489}
   {:section/designation "50x50x8EA" :section/builder :angle :mass 5.68 :r1 6.0 :r2 3.0 :b 50 :t 7.8 :Ag 723}
   {:section/designation "50x50x6EA" :section/builder :angle :mass 4.46 :r1 6.0 :r2 3.0 :b 50 :t 6.0 :Ag 568}
   {:section/designation "50x50x5EA" :section/builder :angle :mass 3.48 :r1 6.0 :r2 3.0 :b 50 :t 4.6 :Ag 443}
   {:section/designation "50x50x3EA" :section/builder :angle :mass 2.31 :r1 6.0 :r2 3.0 :b 50 :t 3.0 :Ag 295}
   {:section/designation "45x45x6EA" :section/builder :angle :mass 3.97 :r1 5.0 :r2 3.0 :b 45 :t 6.0 :Ag 506}
   {:section/designation "45x45x5EA" :section/builder :angle :mass 3.10 :r1 5.0 :r2 3.0 :b 45 :t 4.6 :Ag 394}
   {:section/designation "45x45x3EA" :section/builder :angle :mass 2.06 :r1 5.0 :r2 3.0 :b 45 :t 3.0 :Ag 263}
   {:section/designation "40x40x6EA" :section/builder :angle :mass 3.50 :r1 5.0 :r2 3.0 :b 40 :t 6.0 :Ag 446}
   {:section/designation "40x40x5EA" :section/builder :angle :mass 2.73 :r1 5.0 :r2 3.0 :b 40 :t 4.6 :Ag 348}
   {:section/designation "40x40x3EA" :section/builder :angle :mass 1.83 :r1 5.0 :r2 3.0 :b 40 :t 3.0 :Ag 233}
   {:section/designation "30x30x6EA" :section/builder :angle :mass 2.56 :r1 5.0 :r2 3.0 :b 30 :t 6.0 :Ag 326}
   {:section/designation "30x30x5EA" :section/builder :angle :mass 2.01 :r1 5.0 :r2 3.0 :b 30 :t 4.6 :Ag 256}
   {:section/designation "30x30x3EA" :section/builder :angle :mass 1.35 :r1 5.0 :r2 3.0 :b 30 :t 3.0 :Ag 173}
   {:section/designation "25x25x6EA" :section/builder :angle :mass 2.08 :r1 5.0 :r2 3.0 :b 25 :t 6.0 :Ag 266}
   {:section/designation "25x25x5EA" :section/builder :angle :mass 1.65 :r1 5.0 :r2 3.0 :b 25 :t 4.6 :Ag 210}
   {:section/designation "25x25x3EA" :section/builder :angle :mass 1.12 :r1 5.0 :r2 3.0 :b 25 :t 3.0 :Ag 143}
   {:section/designation "150x100x12UA" :section/builder :angle :mass 22.5 :d 100 :r1 10.0 :r2 5.0 :b 150 :t 12.0 :Ag 2870}
   {:section/designation "150x100x10UA" :section/builder :angle :mass 18.0 :d 100 :r1 10.0 :r2 5.0 :b 150 :t 9.5 :Ag 2300}
   {:section/designation "150x90x16UA" :section/builder :angle :mass 27.9 :d 90 :r1 10.0 :r2 5.0 :b 150 :t 15.8 :Ag 3550}
   {:section/designation "150x90x12UA" :section/builder :angle :mass 21.6 :d 90 :r1 10.0 :r2 5.0 :b 150 :t 12.0 :Ag 2750}
   {:section/designation "150x90x10UA" :section/builder :angle :mass 17.3 :d 90 :r1 10.0 :r2 5.0 :b 150 :t 9.5 :Ag 2200}
   {:section/designation "150x90x8UA" :section/builder :angle :mass 14.3 :d 90 :r1 10.0 :r2 5.0 :b 150 :t 7.8 :Ag 1820}
   {:section/designation "125x75x12UA" :section/builder :angle :mass 17.7 :d 75 :r1 8.0 :r2 5.0 :b 125 :t 12.0 :Ag 2260}
   {:section/designation "125x75x10UA" :section/builder :angle :mass 14.2 :d 75 :r1 8.0 :r2 5.0 :b 125 :t 9.5 :Ag 1810}
   {:section/designation "125x75x8UA" :section/builder :angle :mass 11.8 :d 75 :r1 8.0 :r2 5.0 :b 125 :t 7.8 :Ag 1500}
   {:section/designation "125x75x6UA" :section/builder :angle :mass 9.16 :d 75 :r1 8.0 :r2 5.0 :b 125 :t 6.0 :Ag 1170}
   {:section/designation "100x75x10UA" :section/builder :angle :mass 12.4 :d 75 :r1 8.0 :r2 5.0 :b 100 :t 9.5 :Ag 1580}
   {:section/designation "100x75x8UA" :section/builder :angle :mass 10.3 :d 75 :r1 8.0 :r2 5.0 :b 100 :t 7.8 :Ag 1310}
   {:section/designation "100x75x6UA" :section/builder :angle :mass 7.98 :d 75 :r1 8.0 :r2 5.0 :b 100 :t 6.0 :Ag 1020}
   {:section/designation "75x50x8UA" :section/builder :angle :mass 7.23 :d 50 :r1 7.0 :r2 3.0 :b 75 :t 7.8 :Ag 921}
   {:section/designation "75x50x6UA" :section/builder :angle :mass 5.66 :d 50 :r1 7.0 :r2 3.0 :b 75 :t 6.0 :Ag 721}
   {:section/designation "75x50x5UA" :section/builder :angle :mass 4.40 :d 50 :r1 7.0 :r2 3.0 :b 75 :t 4.6 :Ag 560}
   {:section/designation "65x50x8UA" :section/builder :angle :mass 6.59 :d 50 :r1 6.0 :r2 3.0 :b 65 :t 7.8 :Ag 840}
   {:section/designation "65x50x6UA" :section/builder :angle :mass 5.16 :d 50 :r1 6.0 :r2 3.0 :b 65 :t 6.0 :Ag 658}
   {:section/designation "65x50x5UA" :section/builder :angle :mass 4.02 :d 50 :r1 6.0 :r2 3.0 :b 65 :t 4.6 :Ag 512}
   ])

(def hollow-samples
  "Placeholder hollow sections, entered by hand and not yet checked against
  the tubular catalogue (not part of the hot rolled publication)."
  [{:section/designation "100x100x6.0SHS" :section/builder :rhs :b 100 :d 100 :t 6.0}
   {:section/designation "101.6x4.0CHS" :section/builder :chs :od 101.6 :t 4.0}])

(def rows (into hot-rolled hollow-samples))
