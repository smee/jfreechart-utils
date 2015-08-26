(ns chart-utils.colors
  (:require [chart-utils.jfreechart :as cjf])
  (:import java.awt.Color))

(defn tripel2color [[^int r ^int g ^int b]]
  (Color. r g b))

;;;;;;;;;; color scale functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn magnification 
  "Magnification. See http://www.caida.org/~youngh/colorscales/nonlinear.html.
x is in [0,1], alpha should vary between -1 and 1, and a good range for beta is [0.5, 5.5]."
  [x alpha beta]
  (/ 1 (Math/pow (Math/cosh (* beta (+ alpha (dec (* 2 x))))) 2)))

(defn transformation [x alpha beta]
  (/ (Math/tanh (* beta (+ alpha (dec (* 2 x))))) beta))

(comment
  (use '[incanter core charts]) 
  (let [x (range 0 1.05 0.025)
        chart (doto (incanter.charts/xy-plot x (repeat 0) :series-label "magnification" :legend true) 
                (incanter.charts/add-lines x (repeat 0) :series-label "transformation")
                incanter.core/view)] 
    (sliders [alpha (range -2 2.025 0.025)
              beta (range -5 5 0.1)]
             (let [t-min (transformation (apply min x) alpha beta)
                   t-max (transformation (apply max x) alpha beta)] 
               (println [t-min t-max])
               (incanter.core/set-data chart [x (map #(magnification % alpha beta) x)] 0)
               (incanter.core/set-data chart [x (map #(/ (- (transformation % alpha beta) t-min) (- t-max t-min)) x)] 1))))
  )

;;;;;;;;;;;;;;; for chart-utils.jfreechart/heat-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-color-scale-segments [& val-col-pairs]
  (let [indices (map first val-col-pairs)
        min (apply min indices)
        max (apply max indices)
        val-col-pairs (partition 2 1 (sort-by first val-col-pairs))] 
    (fn [x]
      (if-let [pair (some #(when (<= (ffirst %) x (first (second %))) %) val-col-pairs)] 
        (let [[[from [r1 g1 b1]] [to [r2 g2 b2]]] pair
              scaled-x (/ (- x from) (- to from))
              dr (- r2 r1)
              dg (- g2 g1)
              db (- b2 b1)]
          [(int (+ r1 (* scaled-x dr)))
           (int (+ g1 (* scaled-x dg)))
           (int (+ b1 (* scaled-x db)))])
        (-> val-col-pairs last second second)))))

(defn segments-to-heat-map-scale [color-scale values]
  (map #(vector % (tripel2color (color-scale %))) values))

(defn create-color-scale [min max triples]
  (let [triples (vec triples)
        |colors| (count triples)
        last-idx (dec |colors|)
        step (double (/ (- max min) |colors|))
        first-color (tripel2color (first triples))
        last-color (tripel2color (nth triples last-idx))]
    (fn [x]
      (let [idx (->> (/ (- x min) step) 
                  (clojure.core/min last-idx)
                  (clojure.core/max 0))]
        (cond
          (zero? idx) first-color
          (= last-idx idx) last-color
          :else (let [i (int idx)
                      i+1 (inc i)
                      scaled-x (- x min (* i step))       
                      [r1 g1 b1] (nth triples i)
                      [r2 g2 b2] (nth triples i+1)
                      delta-r (- r2 r1)
                      delta-g (- g2 g1)
                      delta-b (- b2 b1)]
                  (tripel2color [(int (+ r1 (* scaled-x delta-r)))
                                 (int (+ g1 (* scaled-x delta-g)))
                                 (int (+ b1 (* scaled-x delta-b)))])))))))

(defn divergence-colorscale
  "From 'brownblue' of http://www.mathworks.com/matlabcentral/fileexchange/17555-light-bartlein-color-maps"
  [min max]
  (create-color-scale min max [[24 79 162]
                               [70 99 174]
                               [109 153 206]
                               [160 190 225]
                               [207 226 240]
                               [241 244 245]
                               [244 218 200]
                               [248 184 139]
                               [225 146 65]
                               [187 120 54]
                               [144 100 44]]))

(defn same-brightness-rainbow 
  "Scale from https://mycarta.wordpress.com/2012/12/06/the-rainbow-is-deadlong-live-the-rainbow-part-5-cie-lab-linear-l-rainbow/
Rainbow with constant perceived brightness."
  [min max]
  (let [colors [[4	4	4]
                [10	3	8]                
                [13	4	11]
                [16	5	14]
                [18	5	16]
                [21	6	18]
                [22	7	19]
                [24	8	21]
                [26	8	22]
                [27	9	24]
                [28	10	25]
                [30	11	26]
                [31	12	27]
                [32	12	28]
                [33	13	29]
                [35	14	31]
                [36	14	32]
                [37	15	32]
                [38	15	33]
                [39	16	34]
                [40	17	35]
                [41	17	36]
                [42	18	38]
                [43	19	38]
                [44	19	39]
                [46	20	41]
                [46	20	45]
                [46	21	50]
                [45	21	55]
                [45	21	60]
                [45	22	64]
                [45	23	67]
                [45	23	71]
                [45	24	75]
                [45	24	77]
                [45	25	81]
                [45	25	84]
                [44	26	87]
                [44	27	90]
                [45	27	92]
                [45	28	95]
                [44	29	98]
                [44	29	100]
                [44	30	103]
                [44	31	106]
                [44	31	109]
                [44	32	110]
                [44	33	113]
                [44	34	116]
                [43	34	118]
                [42	35	121]
                [40	38	120]
                [38	40	119]
                [36	42	120]
                [34	44	120]
                [33	46	120]
                [32	47	120]
                [31	49	121]
                [30	50	122]
                [30	51	123]
                [29	52	123]
                [29	53	125]
                [28	55	125]
                [28	56	126]
                [27	57	127]
                [28	58	128]
                [28	59	129]
                [27	60	129]
                [27	61	131]
                [27	62	132]
                [27	63	133]
                [28	64	134]
                [27	65	135]
                [27	66	136]
                [27	68	137]
                [27	69	138]
                [25	71	136]
                [22	73	134]
                [21	74	133]
                [20	76	131]
                [17	78	129]
                [16	79	128]
                [15	81	126]
                [14	82	125]
                [10	84	123]
                [10	85	122]
                [9	87	120]
                [8	88	119]
                [7	89	118]
                [6	91	117]
                [4	92	115]
                [4	94	114]
                [4	95	114]
                [3	96	112]
                [1	98	111]
                [1	99	110]
                [0	100	109]
                [0	101	108]
                [0	103	107]
                [0	104	106]
                [0	105	105]
                [0	107	104]
                [0	108	101]
                [0	110	100]
                [0	111	99]
                [0	112	98]
                [0	114	96]
                [0	115	95]
                [0	116	93]
                [0	118	92]
                [0	119	90]
                [0	120	89]
                [0	121	88]
                [0	123	86]
                [0	124	85]
                [0	125	83]
                [0	127	82]
                [0	128	80]
                [0	129	79]
                [0	131	77]
                [0	132	75]
                [0	133	73]
                [0	134	72]
                [0	136	70]
                [0	137	68]
                [0	138	66]
                [0	139	65]
                [0	141	64]
                [0	142	63]
                [0	143	61]
                [0	145	60]
                [0	146	60]
                [0	147	58]
                [0	149	57]
                [0	150	56]
                [0	151	55]
                [0	153	53]
                [0	154	52]
                [0	155	51]
                [0	157	50]
                [0	158	48]
                [0	159	47]
                [0	160	45]
                [0	162	44]
                [0	163	42]
                [0	164	41]
                [0	165	39]
                [0	167	36]
                [0	168	34]
                [0	169	31]
                [0	170	23]
                [0	169	8]
                [9	170	0]
                [20	171	0]
                [29	172	0]
                [35	173	0]
                [40	174	0]
                [45	175	0]
                [48	176	0]
                [52	177	0]
                [55	178	0]
                [59	179	0]
                [61	180	0]
                [64	181	0]
                [66	182	0]
                [68	183	0]
                [71	184	0]
                [73	185	0]
                [76	186	0]
                [78	187	0]
                [79	188	0]
                [81	189	0]
                [83	190	0]
                [85	191	0]
                [87	192	0]
                [92	193	0]
                [99	193	0]
                [106	193	0]
                [114	193	0]
                [119	194	0]
                [125	194	0]
                [130	194	0]
                [135	195	0]
                [140	195	0]
                [145	195	0]
                [149	196	0]
                [153	196	0]
                [157	197	0]
                [161	197	0]
                [165	197	0]
                [169	198	0]
                [172	198	0]
                [176	199	0]
                [180	199	0]
                [184	199	0]
                [186	200	0]
                [190	201	0]
                [193	201	0]
                [197	201	0]
                [200	202	0]
                [201	201	24]
                [203	202	51]
                [206	202	65]
                [207	203	77]
                [209	203	87]
                [212	203	95]
                [213	204	103]
                [215	205	109]
                [218	205	116]
                [219	206	121]
                [221	207	127]
                [223	207	132]
                [226	207	138]
                [227	208	143]
                [229	209	147]
                [231	209	151]
                [232	210	155]
                [235	211	159]
                [237	211	164]
                [238	212	168]
                [240	212	172]
                [243	213	175]
                [243	214	179]
                [245	214	183]
                [248	215	186]
                [248	216	189]
                [248	218	193]
                [247	219	195]
                [247	220	198]
                [247	222	201]
                [248	223	204]
                [247	224	206]
                [247	226	209]
                [247	227	211]
                [247	229	214]
                [247	230	216]
                [247	231	218]
                [247	232	220]
                [248	234	224]
                [247	235	225]
                [247	236	229]
                [247	238	231]
                [247	239	232]
                [248	240	235]
                [248	242	237]
                [247	243	239]
                [248	244	241]
                [248	246	244]
                [248	247	246]
                [248	248	248]
                [249	249	249]
                [251	251	251]
                [252	252	252]
                [253	253	253]
                [254	254	254]
                [255	255	255]]]
    (create-color-scale min max colors))) 

;;;;;;;;;;;;;;;; experiments with colors and color scales ;;;;;;;;;;;;;;;;;;
(defn color-distance 
  "Natural color distance metric, see http:;www.compuphase.com/cmetric.htm"
  [^Color c1 ^Color c2]
  (let [rmean (* 0.5 (+ (.getRed c1) (.getRed c2)))
        r (- (.getRed c1) (.getRed c2))
        g (- (.getGreen c1) (.getGreen c2))
        b (- (.getBlue c1) (.getBlue c2))
        weight-r (+ 2 (/ rmean 256))
        weight-g 4.0
        weight-b (+ 2 (/ (- 255 rmean) 256))]
    (Math/sqrt (+ (* weight-r r r) (* weight-g g g) (* weight-b b b)))))
