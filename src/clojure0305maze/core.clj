(ns clojure0305maze.core)

(defn maze
  "Returns a random maze carved out of walls; walls is a set of
   2-item sets #{a b} where a and b are locations.
   The returned maze is a set of the remaining walls."
  [walls]
  ;(println "walls"  walls)
  ;resenje za 3x3
  ;walls #{#{[2 0] [2 1]} #{[1 1] [1 2]} #{[1 1] [0 1]} #{[0 2] [1 2]} #{[1 1] [2 1]} #{[0 2] [0 1]} #{[1 0] [1 1]} #{[1 0] [2 0]} #{[2 2] [2 1]} #{[0 0] [0 1]} #{[0 0] [1 0]} #{[2 2] [1 2]}}  
  ;(println "map seq --" (map seq walls))
  ;map seq -- (([2 0] [2 1]) ([1 1] [1 2]) ([1 1] [0 1]) ([0 2] [1 2]) ([1 1] [2 1]) ([0 2] [0 1]) ([1 0] [1 1]) ([1 0] [2 0]) ([2 2] [2 1]) ([0 0] [0 1]) ([0 0] [1 0]) ([2 2] [1 2]))  
  (let [paths (reduce (fn [index [a b]]        ;reduce uvek ima oblik (funkcija) 
                                               ;FUNKCIJA  nesto se radi sa ulaznim parametrima (samo se u startu definisu ulazni parametri)
                                               ;parametri se pune u nastavku funkcje
                                               ;{}-oblik izlaza   
                                               ;(map seq walls)-ulazni parametri za reduce
                        (merge-with into index {a [b] b [a]}))  ;merge-with (into je funkcija za merge-with koja ubacuje nove vrednosti u postojece)
                                                                ;u prvom krugu donos=([2 0] [2 1]) => index=[2 0] a=2 b=1 pa je resenje [2 0] [[2 1]]
                                                                      ;za [2 0] susedna polja su [[2 1]]
                                                                      ;za [2 1] susedne polja su [[2 0]] - istovremeno  KAKO ISTOVREMENO? NEMAM POJMA
                                                                ; ...
                                                                ; u nekom od sledecih krugova donos=([1 0] [2 0]) => index=[1 0] a=2 b=0 pa je resenje [1 0] [[2 0]]
                                                                      ;za [1 0] susedna polja su [[2 0]]
                                                                      ;za [2 0] susedna polja su [[2 1] [1 0]] - istovremeno  KAKO ISTOVREMENO? NEMAM POJMA
                                                                
                      {}                ;definise kojeg je oblika izlaz
                      (map seq walls))  ;map pravi list() od seta setova - to su ujedno ulazni parametri za funkciju reduce
        start-loc (rand-nth (keys paths))] ;start-loc je slucajno odabrana lokacija iz lokacija u paths i od nje se krece sa popunjavanjem maze tabele
    ;KRAJ LET
    
    ;(println "paths" paths)
    ;u promenjivoj paths su smesteni vectori mogucih kretanja iz odredjenog polja
    ;paths {[0 0] [[0 1] [1 0]], [2 2] [[2 1] [1 2]], [1 0] [[1 1] [2 0] [0 0]], [0 2] [[1 2] [0 1]], [0 1] [[1 1] [0 2] [0 0]], [1 2] [[1 1] [0 2] [2 2]], [1 1] [[1 2] [0 1] [2 1] [1 0]], [2 1] [[2 0] [1 1] [2 2]], [2 0] [[2 1] [1 0]]}    
    
    ;LOOP - pocetne vrednosti
    (loop [new-walls walls                                  ;formiraj promenjivu new-walls sa vrednostima walls
           unvisited (disj (set (keys paths)) start-loc)]   ;formiraj promenjivu unvisited sa svim mogucim vrednostima osim pocetne
      ;if-let uslov
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]   ;u okviru [loc (when-let [s (seq unvisited)] (rand-nth s))] za if-let je dodeljivanje vrednosti za loc
                                                                  ;ako je loc nil onda se ide na false uslov. ako ima nesto ide se na true uslov
                                                                  ;when-let u okviru [s (seq unvisited)] je dodeljivanje vrednosti za s
                                                                  ;ukoliko s postoji izvrsice se komanda (rand-nth s) sa obzirom da when ima samo 
                                                                  ;true opciju pa ce se ta vrednost dodeliti i promenjivoj loc
                                                                  ;ukoliko ne postoji s tj. s je nil nece se izvrsiti komanda i s=nil
                                                                  ;a samim tim je i loc je nil
        
        ;if-let = true
        (let [walk (take 10 (iterate (comp rand-nth paths) loc))     ;u promenjivu walk smesta slucajno odabranu vrednost iz map-e paths
                                                                      ;gde je prva vrednost loc
                                                                      ;i sve to ponavlja beskonacno puta
              steps (zipmap (take-while unvisited walk) (next walk))] ;take-while: deo slucajnog hoda ali tako da se ne ukljucuju posecene lokacije, vec samo unvisited
                                                                      ;next walk je beskonacna petlja. ali  (take-while unvisited walk)  nije
                                                                      ;pa zato zipmap gleda samo prvih n clanova kolekcije (next walk)
                                                                      ;i pridruzuje vrednosti trenutnog koraka vrednost narednog koraka
                                                                      ;(pravi parove trnutni korak, naredni korak)
                                                                      ;steps {[2 2] [2 3], [2 3] [1 3], [3 3] [3 2], [3 0] [3 1], [1 3] [0 3], [0 3] [0 2], [2 0] [3 0], [3 1] [2 1], [2 1] [2 2], [1 2] [2 2], [3 2] [3 1]}
                                                                      
          
          (println "loc" loc)
          (println "walk" walk)
          (println "steps" steps)
          (println "unvisited" unvisited)
          (println "RRRRRRRRRRRRRRR" (take-while unvisited walk))

          ;rekurzija za LOOP. Izvrsi ovo pa se ponovo vrati na loop tj. promeni neke vrednosti pa ponovo na loop
          (recur (reduce disj new-walls (map set steps))  ;funkcija disj-disjoin (iz new-walls izbaci sve vrednosti (map set steps))         
                 (reduce disj unvisited (keys steps))))   ;funkcija disj-disjoin (iz unvisited izbacuje sve steps (korake))
        
        ;if-let = false. ujedno i KRAJ izvrsavanja LOOP
        new-walls))
    ;kraj LOOP
    ))

(defn grid
  [w h]
  (set (concat                                                       ;spaja koordinate svih susednih polja, tj mogucnosti iz kojeg polja je moguce ici u sledece
         (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})       ;pravi set parova vektora {[i j] [i+1 j  ]} gde i=0-38, j=0-39 
         (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))    ;pravi set parova vektora {[i j] [i   j+1]} gde i=0-39, j=0-38

(defn draw
  [w h maze]
  (println "maze" w h maze)
  ;resenje
  ;maze 60 60 #{#{[1 3] [1 2]} #{[2 0] [2 1]} #{[2 2] [3 2]} #{[0 2] [1 2]} #{[1 1] [2 1]} #{[0 2] [0 1]} #{[1 0] [1 1]} #{[0 0] [0 1]} #{[2 3] [3 3]}}
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
      (doto (proxy [javax.swing.JPanel] []
              (paintComponent [^java.awt.Graphics g]
                (let [g (doto ^java.awt.Graphics2D (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (java.awt.BasicStroke. 0.4)))]
                  (.drawRect g -1 -1 w h)
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (let [[xc yc] (if (= xa xb)
                                    [(dec xa) ya]
                                    [xa (dec ya)])]
                      (.drawLine g xa ya xc yc))))))
        (.setPreferredSize (java.awt.Dimension. 
                             (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(draw 60 60 (maze (grid 4 4)))  ;prvo poziva grid i rezultat prenosi u maze




