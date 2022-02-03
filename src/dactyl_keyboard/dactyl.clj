(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))


(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;bru trs import
(def trs-model (import "E:/manuform/dactyl-manuform-master/src/dactyl_keyboard/TRRSBreakout.stl"))
(def pro-micro-model (import "E:/manuform/dactyl-manuform-master/src/dactyl_keyboard/proMicro.stl"))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)

;(def α (/ π 12))                        ; curvature of the columns
(def α (/ π 8))                        ; curvature of the columns
;(def α (/ π 6))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
;default (def centerrow (- nrows 3))             ; controls front-back tilt
(def centerrow (- nrows 2))             ; controls front-back tilt
;(def centerrow (- nrows 1.5))             ; controls front-back tilt
;(def centercol 3)                       ; controls left-right tilt / tenting (higher number is more tenting)
;(def tenting-angle (/ π 10))            ; or, change this for more precise tenting control
(def centercol 3)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 10))            ; or, change this for more precise tenting control
(def column-style
  (if (> nrows 4) :standard :orthographic))  ; options include :standard, :orthographic, and :fixed
; (def column-style :fixed)

(defn column-offset [column] (cond
  (= column 2) [0 8 -1] ; org [0 2.82 -4.5]
  (= column 3) [0 4 -1] ; org [0 0 0]
  (>= column 4) [0 -1 0]            ; original [0 -5.8 5.64] [0 -12 5.64]
  :else [0 0 0]))

;bru- changed y offset to keep hole from forming near the thumbs. Needs to be <-8 with the current settings
;(def thumb-offsets [0 -8 0]) ;org [0 -8 6]
(def thumb-offsets [5 -16 -20]) ;org [0 -8 6]

(def keyboard-z-offset 20)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2 org 26

(def extra-width 0.5)                   ; extra space between the base of keys; original= 2
;(def extra-width 1.5)                   ; extra space between the base of keys; original= 2
(def extra-height -2.0)                  ; original= 0.5
;(def extra-height 0)                  ; original= 0.5

(def wall-z-offset -5)                 ; length of the first downward-sloping part of the wall (negative) org 15
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative) org 8
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.4) ;; Was 14.1, then 14.25
(def keyswitch-width 14.4)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 plate-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ plate-thickness 2)]))))
        plate-half (union top-wall left-wall (with-fn 100 side-nub))]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

;(def sa-length 18.25)
(def sa-length 17)
;(def sa-double-length 37.5)
(def sa-double-length (* 2 sa-length))
;(def sa-cap {1 (let [bl2 (/ 18.5 2)
(def sa-cap {1 (let [bl2 (/ sa-length 2)
                     m (/ (- sa-length 1) 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ sa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))
(def column-base-angle (* β (- centercol 2)))

(def spreadRad (deg2rad 2))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn rotate-z-fn column row shape]
  (let [column-angle (* β (- centercol column))
        shape-rotate (fn [shape] (if-not (vector? shape) (rotate column-angle [0 0 1] shape) shape))
        placed-shape (->> shape
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                ;replace this with rotate-z-fn. Might still need to figure out what this angle needs to be. I could also just make this an extra configurable parameter to start with
                                ;makes the y rotation
                                (rotate-y-fn  column-angle)
                                (rotate-z-fn spreadRad column )
                                ;spreads out into columns
                                (translate-fn [(- (* (- column centercol) (* column-x-delta (+ 1 (* 0.02 (- (Math/abs (- column 2)))))))) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0])
                                )]
    (->> (case column-style
          :orthographic placed-shape-ortho
          :fixed        placed-shape-fixed
                        placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
    (fn [angle obj] (rotate angle [1 0 0] obj))
    (fn [angle obj] (rotate angle [0 1 0] obj))
    (fn [angle column obj] 
      (let [modang (* angle (- (- centercol 0.5) column))]
      (rotate modang [0 0 1] obj)))
    column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

;bru - need to figure out what this is supposed to be
;this is an extention to the other ones.
(defn rotate-around-z [angle column position]
  (let [modang (* angle (- centercol column))]
  (mmul
   [[(Math/cos modang)     (- (Math/sin modang)) 0]
    [(Math/sin modang)     (Math/cos modang) 0]
    [0                    0 1]]
   position)))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y rotate-around-z column row position))


(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> (sa-cap (if (= column 5) 1 1))
                (key-place column row)))))

; (pr (rotate-around-y π [10 0 1]))
; (pr (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0]))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
         thumb-offsets))
; (pr thumborigin)
;bru - since the functions themselves can't be modified, consider the following:
;create list/matrix with the default values (rotation + translation) of the thumb clusters
;then create a function to apply whatever adjustment to these values
;use these values as part of the functions
;lets set the values as follows:
;for tr, tl, mr, ml, br, bl: 
;rot x, y, z, trans x, y ,z
(def origThumbPos [[10 -23 10 -12 -16 3]
                   [10 -23 10 -32 -15 -2]
                   [-6 -34 48 -29 -40 -13]
                   [6 -34 40 -51 -25 -12]
                   [-16 -33 54 -37.8 -55.3 -25.3]
                   [-4 -35 52 -56.3 -43.3 -23.5]])


(defn rotateTKeyX [deg row] 
  (conj (mapv + [deg 0 0] row) 
        (row 3)
        (* (row 4) (Math/cos (deg2rad deg)) (Math/cos (deg2rad deg)))
        (+ (row 5) (* (row 4) (Math/sin (deg2rad deg)) (Math/cos (deg2rad deg))))))
(defn rotateTKeyY [deg row] 
  (conj (mapv + [0 deg 0] row) 
        (* (row 3) (Math/cos (deg2rad deg)) (Math/cos (deg2rad deg)))
        (row 4)
        (+ (row 5) (* (row 3) (Math/sin (deg2rad deg)) (Math/cos (deg2rad deg))))))
(defn rotateTKeyZ [deg row] 
  (conj (mapv + [0 0 deg] row) 
        (* (row 3) (Math/cos (deg2rad deg)) (Math/cos (deg2rad deg)))
        (+ (row 4) (* (row 3) (Math/sin (deg2rad deg)) (Math/cos (deg2rad deg))))
        (row 5)))

(defn rotateThumb [rotVec thumbMat]
    (vec (for [ckey thumbMat]
          (->> ckey
              (rotateTKeyX (rotVec 0))
              (rotateTKeyY (rotVec 1))
              (rotateTKeyZ (rotVec 2))))))

(defn modThumbOffset [pos thumbMat]
  (vec (for [ckey thumbMat]
         (apply conj (subvec ckey 0 3)
                (mapv + pos (subvec ckey 3))))))


(def thumbPos (rotateThumb [0 0 0] (modThumbOffset thumborigin origThumbPos)))
;(println thumbPos)
;(println origThumbPos)
;(println (modThumbOffset thumborigin origThumbPos))
;(println thumborigin)
(def thumbDegOffset [0 20 -13])

(defn thumb-tr-place [shape]
  (->> shape
      ;  (rotate (deg2rad  10) [1 0 0])
      ;  (rotate (deg2rad -23) [0 1 0])
      ;  (rotate (deg2rad  -3) [0 0 1])
       ;(translate [-12 -16 3])
       (rotate (deg2rad  ((thumbPos 0)0)) [1 0 0])
       (rotate (deg2rad ((thumbPos 0)1)) [0 1 0])
       (rotate (deg2rad  ((thumbPos 0)2)) [0 0 1])
       (translate (subvec (thumbPos 0) 3))
       (rotate (deg2rad (thumbDegOffset 0)) [1 0 0])
       (rotate (deg2rad (thumbDegOffset 1)) [0 1 0])
       (rotate (deg2rad (thumbDegOffset 2)) [0 0 1])
       ))
(defn thumb-tl-place [shape]
  (->> shape
       ;(rotate (deg2rad  10) [1 0 0])
       ;(rotate (deg2rad -23) [0 1 0])
       ;(rotate (deg2rad  10) [0 0 1])
       ;(translate thumborigin)
       ;(translate [-32 -15 -2])))
       (rotate (deg2rad  ((thumbPos 1)0)) [1 0 0])
       (rotate (deg2rad ((thumbPos 1)1)) [0 1 0])
       (rotate (deg2rad  ((thumbPos 1)2)) [0 0 1])
       (translate (subvec (thumbPos 1) 3))
       (rotate (deg2rad (thumbDegOffset 0)) [1 0 0])
       (rotate (deg2rad (thumbDegOffset 1)) [0 1 0])
       (rotate (deg2rad (thumbDegOffset 2)) [0 0 1])
       ;(translate thumborigin)
       ))
(defn thumb-mr-place [shape]
  (->> shape
       ;(rotate (deg2rad  -6) [1 0 0])
       ;(rotate (deg2rad -34) [0 1 0])
       ;(rotate (deg2rad  48) [0 0 1])
       ;(translate thumborigin)
       ;(translate [-29 -40 -13])
       ;))
       (rotate (deg2rad  ((thumbPos 2)0)) [1 0 0])
       (rotate (deg2rad ((thumbPos 2)1)) [0 1 0])
       (rotate (deg2rad  ((thumbPos 2)2)) [0 0 1])
       (translate (subvec (thumbPos 2) 3))
       (rotate (deg2rad (thumbDegOffset 0)) [1 0 0])
       (rotate (deg2rad (thumbDegOffset 1)) [0 1 0])
       (rotate (deg2rad (thumbDegOffset 2)) [0 0 1])
       ;(translate thumborigin)
       ))
(defn thumb-ml-place [shape]
  (->> shape
       ;(rotate (deg2rad   6) [1 0 0])
       ;(rotate (deg2rad -34) [0 1 0])
       ;(rotate (deg2rad  40) [0 0 1])
       ;(translate thumborigin)
       ;(translate [-51 -25 -12])))
       (rotate (deg2rad  ((thumbPos 3)0)) [1 0 0])
       (rotate (deg2rad ((thumbPos 3)1)) [0 1 0])
       (rotate (deg2rad  ((thumbPos 3)2)) [0 0 1])
       (translate (subvec (thumbPos 3) 3))
       (rotate (deg2rad (thumbDegOffset 0)) [1 0 0])
       (rotate (deg2rad (thumbDegOffset 1)) [0 1 0])
       (rotate (deg2rad (thumbDegOffset 2)) [0 0 1])
       ;(translate thumborigin)
       ))
(defn thumb-br-place [shape]
  (->> shape
       ;(rotate (deg2rad -16) [1 0 0])
       ;(rotate (deg2rad -33) [0 1 0])
       ;(rotate (deg2rad  54) [0 0 1])
       ;(translate thumborigin)
       ;(translate [-37.8 -55.3 -25.3])
       ;))
       (rotate (deg2rad  ((thumbPos 4)0)) [1 0 0])
       (rotate (deg2rad ((thumbPos 4)1)) [0 1 0])
       (rotate (deg2rad  ((thumbPos 4)2)) [0 0 1])
       (translate (subvec (thumbPos 4) 3))
       (rotate (deg2rad (thumbDegOffset 0)) [1 0 0])
       (rotate (deg2rad (thumbDegOffset 1)) [0 1 0])
       (rotate (deg2rad (thumbDegOffset 2)) [0 0 1])
       ;(translate thumborigin)
       ))
(defn thumb-bl-place [shape]
  (->> shape
       ;(rotate (deg2rad  -4) [1 0 0])
       ;(rotate (deg2rad -35) [0 1 0])
       ;(rotate (deg2rad  52) [0 0 1])
       ;(translate thumborigin)
       ;(translate [-56.3 -43.3 -23.5])
       ;))
       (rotate (deg2rad  ((thumbPos 5)0)) [1 0 0])
       (rotate (deg2rad ((thumbPos 5)1)) [0 1 0])
       (rotate (deg2rad  ((thumbPos 5)2)) [0 0 1])
       (translate (subvec (thumbPos 5) 3))
       (rotate (deg2rad (thumbDegOffset 0)) [1 0 0])
       (rotate (deg2rad (thumbDegOffset 1)) [0 1 0])
       (rotate (deg2rad (thumbDegOffset 2)) [0 0 1])
       ;(translate thumborigin)
       ))


(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-ml-place shape)
   (thumb-br-place shape)
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)
   (thumb-tl-place shape)))

(def larger-plate
  (let [plate-height (+ (/ (- sa-double-length mount-height) 3) 1)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        ]
    (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))


;(def thumb
;  (union
;   (thumb-1x-layout single-plate)
;   (thumb-15x-layout single-plate)
;   (thumb-15x-layout larger-plate)
;   ))

(def thumb
  (->>
   ;(rotate (deg2rad  -4) [1 0 0])
   ;(rotate (deg2rad -35) [0 1 0])
   ;(rotate (deg2rad  52) [0 0 1])
    (union
     (thumb-1x-layout single-plate)
     (thumb-15x-layout single-plate)
     (thumb-15x-layout larger-plate)
     )
    ;(translate (mapv * [-1 -1 -1] thumborigin))
  )
)

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  1.15) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  1.15) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -1.15) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -1.15) post-adj) 0] web-post))

(def thumb-connectors
  (union
      (triangle-hulls    ; top two
             (thumb-tl-place thumb-post-tr)
             (thumb-tl-place thumb-post-br)
             (thumb-tr-place thumb-post-tl)
             (thumb-tr-place thumb-post-bl))
      (triangle-hulls    ; bottom two on the right
             (thumb-br-place web-post-tr)
             (thumb-br-place web-post-br)
             (thumb-mr-place web-post-tl)
             (thumb-mr-place web-post-bl))
      (triangle-hulls    ; bottom two on the left
             (thumb-bl-place web-post-tr)
             (thumb-bl-place web-post-br)
             (thumb-ml-place web-post-tl)
             (thumb-ml-place web-post-bl))
      (triangle-hulls    ; centers of the bottom four
             (thumb-br-place web-post-tl)
             (thumb-bl-place web-post-bl)
             (thumb-br-place web-post-tr)
             (thumb-bl-place web-post-br)
             (thumb-mr-place web-post-tl)
             (thumb-ml-place web-post-bl)
             (thumb-mr-place web-post-tr)
             (thumb-ml-place web-post-br))
      (triangle-hulls    ; top two to the middle two, starting on the left
             (thumb-tl-place thumb-post-tl)
             (thumb-ml-place web-post-tr)
             (thumb-tl-place thumb-post-bl)
             (thumb-ml-place web-post-br)
             (thumb-tl-place thumb-post-br)
             (thumb-mr-place web-post-tr)
             (thumb-tr-place thumb-post-bl)
             (thumb-mr-place web-post-br)
             (thumb-tr-place thumb-post-br))
      (triangle-hulls    ; top two to the main keyboard, starting on the left
             (thumb-tl-place thumb-post-tl)
             (key-place 0 cornerrow web-post-bl)
             (thumb-tl-place thumb-post-tr)
             (key-place 0 cornerrow web-post-br)
             (thumb-tr-place thumb-post-tl)
             (key-place 1 cornerrow web-post-bl)
             (thumb-tr-place thumb-post-tr)
             (key-place 1 cornerrow web-post-br)
             (key-place 2 lastrow web-post-tl)
             (key-place 2 lastrow web-post-bl)
             (thumb-tr-place thumb-post-tr)
             (key-place 2 lastrow web-post-bl)
             (thumb-tr-place thumb-post-br)
             (key-place 2 lastrow web-post-br)
             (key-place 3 lastrow web-post-bl)
             (key-place 2 lastrow web-post-tr)
             (key-place 3 lastrow web-post-tl)
             (key-place 3 cornerrow web-post-bl)
             (key-place 3 lastrow web-post-tr)
             (key-place 3 cornerrow web-post-br)
             (key-place 4 cornerrow web-post-bl))
      (triangle-hulls
             (key-place 1 cornerrow web-post-br)
             (key-place 2 lastrow web-post-tl)
             (key-place 2 cornerrow web-post-bl)
             (key-place 2 lastrow web-post-tr)
             (key-place 2 cornerrow web-post-br)
             (key-place 3 cornerrow web-post-bl)
             )
      (triangle-hulls
             (key-place 3 lastrow web-post-tr)
             (key-place 3 lastrow web-post-br)
             (key-place 3 lastrow web-post-tr)
             (key-place 4 cornerrow web-post-bl))
  ))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 10)
(def left-wall-z-offset  3)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))


(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
    (hull
      (place1 post1)
      (place1 (translate (wall-locate1 dx1 dy1) post1))
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 post2)
      (place2 (translate (wall-locate1 dx2 dy2) post2))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))
    (bottom-hull
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))
      ))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))
; for back wall: gets expanded as follows
; keywallbravce 0 0 0 1 wptl
; wall brace keyplace 0 0 0 1 wptl
; hull (keyplace 0 0) wptl
;      (keyplace 0 0) translave (walllocate 0 1) wptl
(def case-walls
  (union
   ; back wall
   (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   ; right wall
   (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y       1 0 web-post-br))
   (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))
   (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 1 0 web-post-br)
   ; left wall
   (for [y (range 0 lastrow)] (union (wall-brace (partial left-key-place y 1)       -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
                                     (hull (key-place 0 y web-post-tl)
                                           (key-place 0 y web-post-bl)
                                           (left-key-place y  1 web-post)
                                           (left-key-place y -1 web-post))))
   (for [y (range 1 lastrow)] (union (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
                                     (hull (key-place 0 y       web-post-tl)
                                           (key-place 0 (dec y) web-post-bl)
                                           (left-key-place y        1 web-post)
                                           (left-key-place (dec y) -1 web-post))))
   (wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) 0 1 web-post)
   (wall-brace (partial left-key-place 0 1) 0 1 web-post (partial left-key-place 0 1) -1 0 web-post)
   ; front wall
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   (key-wall-brace 3 lastrow   0 -1 web-post-bl 3 lastrow 0.5 -1 web-post-br)
   (key-wall-brace 3 lastrow 0.5 -1 web-post-br 4 cornerrow 1 -1 web-post-bl)
   (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
   (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
   ; thumb walls
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-tr-place  0 -1 thumb-post-br)
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-mr-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-ml-place -0.3  1 web-post-tr thumb-ml-place  0  1 web-post-tl)
   (wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
   (wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)
   ; thumb corners
   (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)
   ; thumb tweeners
   (wall-brace thumb-mr-place  0 -1 web-post-bl thumb-br-place  0 -1 web-post-br)
   (wall-brace thumb-ml-place  0  1 web-post-tl thumb-bl-place  0  1 web-post-tr)
   (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
   (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr)))
   (hull
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
     (thumb-tl-place thumb-post-tl))
   (hull
     (left-key-place cornerrow -1 web-post)
     (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-tl-place thumb-post-tl))
   (hull
     (left-key-place cornerrow -1 web-post)
     (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
     (key-place 0 cornerrow web-post-bl)
     (key-place 0 cornerrow (translate (wall-locate1 -1 0) web-post-bl))
     (thumb-tl-place thumb-post-tl))
   (hull
     (thumb-ml-place web-post-tr)
     (thumb-ml-place (translate (wall-locate1 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
     (thumb-tl-place thumb-post-tl))
  ))
;added here so that connectors can reference it
(defn wallY [column] 
      (->>
        (map +
             ;[0 0 (+ (/ web-thickness -2) plate-thickness)]
             [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0]
             (wall-locate3 0 1))
        (key-position column 0)
        (second)))

(pr (wallY 0))
      
(def rj9-start  (map + [0 -3  0] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def rj9-position  [(first rj9-start) (second rj9-start) 11])
(def rj9-cube   (cube 14.78 13 22.38))
(def rj9-space  (translate rj9-position rj9-cube))
(def rj9-holder (translate rj9-position
                  (difference rj9-cube
                              (union (translate [0 2 0] (cube 10.78  9 18.38))
                                     (translate [0 0 5] (cube 10.78 13  5))))))


(def trs-start  (map + [0 -3  0] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def trs-position  [(first trs-start) (second trs-start) 11])
(def trs-cylinder (cylinder (/ 5.85 2), 20))
(def trs-cut
  (translate [3, 0, 0] (rotate (/ pi -2) [1 0 0] trs-cylinder)))

(def trs-space (translate trs-position trs-cut))


(def usb-holder-position (key-position 1 0 (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0])))
(def usb-holder-size [6.5 10.0 13.6])
(def usb-holder-thickness 4)
(def usb-holder
    (->> (cube (+ (first usb-holder-size) usb-holder-thickness) (second usb-holder-size) (+ (last usb-holder-size) usb-holder-thickness))
         (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))
(def usb-holder-hole
    (->> (apply cube usb-holder-size)
         (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))

(def teensy-width 20)
(def teensy-height 12)
(def teensy-length 33)
(def teensy2-length 53)
(def teensy-pcb-thickness 2)
(def teensy-holder-width  (+ 7 teensy-pcb-thickness))
(def teensy-holder-height (+ 6 teensy-width))
(def teensy-offset-height 5)
(def teensy-holder-top-length 18)
(def teensy-top-xy (key-position 0 (- centerrow 1) (wall-locate3 -1 0)))
(def teensy-bot-xy (key-position 0 (+ centerrow 1) (wall-locate3 -1 0)))
(def teensy-holder-length (- (second teensy-top-xy) (second teensy-bot-xy)))
(def teensy-holder-offset (/ teensy-holder-length -2))
(def teensy-holder-top-offset (- (/ teensy-holder-top-length 2) teensy-holder-length))

;bru place promicro
(def trs-box-dims [18 (+ 0.5 14.750) (+ 0.5 6.70)])
(def trs-box (->>
               (apply cube trs-box-dims)
               (translate [0 -1 (/ (last trs-box-dims) 2)]))
) ;trs-box makes box to the same dimensions of trs connector for easier referencing

;(def test-pos (key-position 0 0 (map + (wall-locate2 0 1) [0 (+ -3.8 2) 0 ])))
;(def test-pos (key-position 0 0 (map + (wall-locate2 0 1) [0 -1 0 ])))
;(println (key-position 0 0 (map + (wall-locate1 0 1) [0 0 0 ])))
;(println (key-position 0 0 (map + (wall-locate2 0 1) [0 0 0 ])))
;(println (key-position 0 0 (map + (wall-locate3 0 1) [0 0 0 ])))
;(println (wall-locate1 0 1))
;(println (wall-locate2 0 1))
;(println (wall-locate3 0 1))
;(println web-post-tl)
;(def test-pos (key-position 0 0 (map + (wall-locate2 0 1) [0 (+ wall-xy-offset -4) 0 ])))
;wall-xy-offset - was 8 originally. with 8, was using -1 offset. -1 = 8 - 9
;(def test-pos (key-position 1 0 (wall-locate2 0 1)))
;change static values into variables
(def trshold-thick 2)
(def trs-depth 20)
(def trs-height 16)
(def trshold-wireOffset -3)
(def trshole-clipsize 2)
(def trshole-outsize 3)
(def tol 0.2)
(def trs-holder-dims [(+ trshold-wireOffset (+ trshold-thick (first trs-box-dims)))
                      (+ (* 2 trshold-thick) (second trs-box-dims))
                      (+ (* 2 trshold-thick) (last trs-box-dims))])
(def trs-holder-cuts 
  (union
    (->> (cylinder trshole-outsize 10)
         (rotate (deg2rad 90) [1 0 0])
         (translate [-3.5 -9 3.75])
         )
    (->> (cube trshole-clipsize 10 trshole-clipsize)
         (translate [1 8 3])
    )
    (->> (cube trshole-clipsize 10 trshole-clipsize)
         (translate [-8 8 3])
    )
    (->> 
      trs-box
      (resize (mapv + (repeat 3 tol) trs-box-dims))
      (translate [0 0 (/ tol -2)])
      )
    (->> 
      trs-box
      (resize (mapv + (repeat 3 tol) trs-box-dims))
      (translate [10 0 (/ tol -2)]))))

(def trs-holder
    (difference
      (->> (apply cube trs-holder-dims)
           (translate [(/ (+ (* -1 trshold-thick) trshold-wireOffset) 2) -1  (- (/ (last trs-box-dims) 2) 0)]))
      (->>
        (cube 50 (+ (second trs-box-dims) tol) 50)
        (translate [0 (* -1 (+ (second trs-box-dims) tol)) 0]))
      ;(->> 
      ;  trs-box
      ;  (resize (mapv + (repeat 3 tol) trs-box-dims))
      ;  (translate [0 -10 (/ tol -2)])
      ;  )
      trs-holder-cuts))
      ;trs-model))


(def wallStart [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0])
(def wallStartl (wall-locate2 0 1))
(def wallStartk (key-position 0 0 [0 0 0]))
(def wallPos 
  (->> 
    (map + wallStart wallStartl wallStartk)
    (map * [1 1 0])))

;(def test-pos (key-position 0 0 (map + (wall-locate1 0 1) [0 -0.5 0 ])))
(println wallPos)
;offset from the wall is -0.4 for trs
;(def test-pos (map + wallPos [5 (+ (- -0.4 1.5) (/ (last trs-holder-dims) -2)) 0]))
;This is used to define the position for the trs connector
(def test-pos [(+ (first wallPos) 9) (+ -5 (- (wallY 0) (/ (last trs-holder-dims) 2))) 0])

(def trs-cuts-placed
  (->> 
    trs-holder-cuts
    (rotate (deg2rad 90) [0 1 0])
    (rotate (deg2rad 180) [1 0 0])
    ;this is done to adjust the aligment with the wall after the key's z tilt
    (rotate (deg2rad (* 3 (- (- centercol 0.5) 0))) [0 0 1])
    ;(cube 10 10 10)
    ;(translate [0 0 1])
    ;(translate [(first test-pos) (second test-pos) 10])
    ;(translate [(first test-pos) (second test-pos) (/ (second trs-holder-dims) 2)])
    (translate [(first test-pos) (second test-pos) (- (/ (first trs-holder-dims) 2) (/ (+ (* -1 trshold-thick) trshold-wireOffset) 2))])
    )
)
(def trs-placed 
  (->> 
    trs-holder
    (rotate (deg2rad 90) [0 1 0])
    (rotate (deg2rad 180) [1 0 0])
    ;this is done to adjust the aligment with the wall after the key's z tilt
    (rotate (deg2rad (* 3 (- (- centercol 0.5) 0))) [0 0 1])
    ;(cube 10 10 10)
    ;(translate [0 0 1])
    (translate [(first test-pos) (second test-pos) (- (/ (first trs-holder-dims) 2) (/ (+ (* -1 trshold-thick) trshold-wireOffset) 2))])
    )
)

;bru-usb
(def usbPcbDims [13 14 1.5])
(def usbBoltDiam 3)
(def usbConnDims [7.5 5.7 2.2])
(def usbBoltHole (binding [*fn* 16] (cylinder (/ usbBoltDiam 2) 10)))
;inner tan is 5.9. 1.5 from edge to center. middle to inner tan is 5.9/2
(def usbBoltLoc [(+ (/ usbBoltDiam 2) (/ 5.9 2)) 0.2 0])
(def usbBoltHolePlaced 
  (->> usbBoltHole
       (translate usbBoltLoc)))

(def usb-model
  (union 
    (difference 
      (->> 
        (apply cube usbPcbDims)
        (translate [0 0 (/ (last usbPcbDims) 2)])
      )
      usbBoltHolePlaced
      (mirror [1 0 0] usbBoltHolePlaced)
    )
    (->>
      (apply cube usbConnDims)
      (translate [0
                  (+ 0.5 (- (/ (second usbPcbDims) 2) (/ (second usbConnDims) 2)))
                  (+ (last usbPcbDims) (/ (last usbConnDims) 2))])
    ) 
  )
)

(def usbHoldCutDims [(+ (* 2 tol) (first usbPcbDims)) 
                     (second usbPcbDims)
                     (+ (* 2 tol) (+ (last usbPcbDims) (last usbConnDims)))])
(def usbHoldCutOutDims [(+ (* 2 tol) (first usbConnDims)) 10 (+ (* 2 tol) (last usbConnDims))])
(def usbHoldCut
  (union 
    (->>
      (apply cube usbHoldCutDims)
      (translate [0 tol (/ (last usbHoldCutDims) 2)])
    )
    (->>
      (apply cube usbHoldCutOutDims)
      (translate [0 (/ (second usbPcbDims) 2) (+ (- (last usbPcbDims) tol) (/ (last usbHoldCutOutDims) 2))])
    )
  )
)
(def usbHoldBaseDims [(+ 4 (first usbPcbDims)) (- (second usbPcbDims) 2) 2])
(def usbHoldBase 
  (difference 
    (->>
      (apply cube usbHoldBaseDims)
      (translate [0 2 (/ (last usbHoldBaseDims) -2)])
    )
    usbBoltHolePlaced
    (mirror [1 0 0] usbBoltHolePlaced)
  )
)

(def usbTestWallDims [20 3 30])
(def usbTestWall
  (->>
    (apply cube usbTestWallDims)
    (translate [0 (+ 1 (-  (/ (second usbPcbDims) 2) (/ (second usbTestWallDims) 2)))  0])
  )
)

;(def usbHoldPos (key-position 1 0 (map + (wall-locate2 0 1) [0 1 0])))
(def usbHoldPos (map + [(first wallPos) (wallY 1) 0] [28 (/ (second usbHoldBaseDims) -2) 0] [0 -2 0]))
(def usbHoldPlaced
  (->>
    usbHoldBase
    (rotate (deg2rad 90) [0 1 0])
    ;this rotation is done in order to adjust the alignment with the wall
    (rotate (deg2rad (* 3 (- (- centercol 0.5) 1))) [0 0 1])
    (translate [(first usbHoldPos) (second usbHoldPos) (/ (first usbHoldBaseDims) 2)])
  )
)
(def usbHoldCutPlaced
  (->>
    usbHoldCut
    (rotate (deg2rad 90) [0 1 0])
    ;this rotation is done in order to adjust the alignment with the wall
    (rotate (deg2rad (* 3 (- (- centercol 0.5) 1))) [0 0 1])
    (translate [(first usbHoldPos) (second usbHoldPos) (/ (first usbHoldBaseDims) 2)])
  )
)


;(print (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0 ]))
;(print (wall-locate2 0 1))
;(print mount-height)
;(print (key-position 1 0 (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0 ])))
(def proMicroBoxDims [17.8 34.5 4])
(def pro-micro-box (apply cube proMicroBoxDims))
(def pro-micro-placed
  (->>
    pro-micro-model
    (rotate (deg2rad 90) [0 0 -1])
    (translate [-79 2 20])
  )
)
(def proMicroHoldClipDims [(- (first proMicroBoxDims) 6)
                           (+ 6 (second proMicroBoxDims))
                           (+ 4 (last proMicroBoxDims))
                           ]
)
(def proMicroHoldClipCutDims [(+ (first proMicroBoxDims) (* 2 tol))
                              ;(second proMicroHoldClipDims)
                              (+ (second proMicroBoxDims) (* 2 tol))
                              (last proMicroHoldClipDims)
                              ]
)
(def proMicroHoldBaseDims [(+ (first proMicroBoxDims) 10)
                           (second proMicroHoldClipDims)
                           (+ (last proMicroBoxDims) 10)
                           ]
)
(def proMicroHoldBase
  (difference 
    (apply cube proMicroHoldBaseDims)
    (->> 
      (apply cube proMicroHoldBaseDims)
      (translate [(- (/ (first proMicroHoldBaseDims) 2) (+ tol (/ (first proMicroBoxDims) 2))) 
                  0 
                  (- (/ (last proMicroHoldBaseDims) 2) (+ tol (/ (last proMicroBoxDims) 2)))])
    )
  )
)
(def proMicroHoldStopDims [(first proMicroBoxDims)
                           2
                           (+ 2 (last proMicroBoxDims))
                           ]
)
(def proMicroHoldStop
  (difference 
    (apply cube proMicroHoldStopDims)
    (->>
      (apply cube proMicroHoldStopDims)
      (translate [(/ (first proMicroHoldStopDims) -2)
                  0 
                  (last proMicroBoxDims)])
    )
  )
)

(def proMicroHold 
  (->>
    (union 
      (difference 
        (->>
          (apply cube proMicroHoldClipDims)
          ;(translate [0 -1 0])
        )
        (->>
          (apply cube proMicroHoldClipCutDims)
          (translate [0 
                      (- (/ (- (second proMicroHoldClipCutDims) (second proMicroBoxDims)) 2) tol)
                      (- tol (/ (- (last proMicroHoldClipCutDims) (last proMicroBoxDims)) 2))
                      ])
        )
        (->>
          (apply cube proMicroHoldClipCutDims)
          (translate [0 
                      (second proMicroBoxDims) 
                      (- tol (+ (/ (- (last proMicroHoldClipCutDims) (last proMicroBoxDims)) 2) (last proMicroBoxDims)))
                      ])
        )
    )
    proMicroHoldBase)))

;(def proMicroHoldPos (key-position 0 2 (map + (wall-locate2 -1 0) [(- (* -1 (/ (last proMicroHoldBaseDims) 2)) 2.6) 0 0])))
(def proMicroHoldPos (key-position 0 2 (map + (wall-locate2 -1 0) [(- (* -1 (/ (last proMicroHoldBaseDims) 2)) 1.0) 16 0])))

(def proMicroHoldPlaced 
  (->> 
    proMicroHold
    (rotate (deg2rad 90) [0 -1 0])
    (rotate (deg2rad 180) [0 0 1])
    (rotate (deg2rad -3) [0 0 1])
    (translate [(first proMicroHoldPos) (second proMicroHoldPos) (/ (first proMicroHoldBaseDims) 2)])
  )
)


;bru-end

(def teensy-holder
    (->>
        (union
          (->> (cube 3 teensy-holder-length (+ 6 teensy-width))
               (translate [1.5 teensy-holder-offset 0]))
          (->> (cube teensy-pcb-thickness teensy-holder-length 3)
               (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-offset (- -1.5 (/ teensy-width 2))]))
          (->> (cube 4 teensy-holder-length 4)
               (translate [(+ teensy-pcb-thickness 5) teensy-holder-offset (-  -1 (/ teensy-width 2))]))
          (->> (cube teensy-pcb-thickness teensy-holder-top-length 3)
               (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-top-offset (+ 1.5 (/ teensy-width 2))]))
          (->> (cube 4 teensy-holder-top-length 4)
               (translate [(+ teensy-pcb-thickness 5) teensy-holder-top-offset (+ 1 (/ teensy-width 2))])))
        (translate [(- teensy-holder-width) 0 0])
        (translate [-1.4 0 0])
        (translate [(first teensy-top-xy)
                    (- (second teensy-top-xy) 1)
                    (/ (+ 6 teensy-width) 2)])
           ))

(defn screw-insert-shape [bottom-radius top-radius height]
   (union (cylinder [bottom-radius top-radius] height)
          (translate [0 0 (/ height 2)] (sphere top-radius))))

(defn screw-insert [column row bottom-radius top-radius height]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                       (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                        (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                       (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))
        ]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate [(first position) (second position) (/ height 2)])
    )))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 0         bottom-radius top-radius height)
         (screw-insert 0 (- lastrow 0.6)   bottom-radius top-radius height)
         (screw-insert 1.3 (+ lastrow 0.55)  bottom-radius top-radius height)
         (screw-insert 3 0         bottom-radius top-radius height)
         (screw-insert (+ lastcol 0.05) 1   bottom-radius top-radius height)
         ))
(def screw-insert-height 3.8)
(def screw-insert-bottom-radius (/ 5.31 2))
(def screw-insert-top-radius (/ 5.1 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.6) (+ screw-insert-top-radius 1.6) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(def wire-post-height 7)
(def wire-post-overhang 3.5)
(def wire-post-diameter 2.6)
(defn wire-post [direction offset]
   (->> (union (translate [0 (* wire-post-diameter -0.5 direction) 0] (cube wire-post-diameter wire-post-diameter wire-post-height))
               (translate [0 (* wire-post-overhang -0.5 direction) (/ wire-post-height -2)] (cube wire-post-diameter wire-post-overhang wire-post-diameter)))
        (translate [0 (- offset) (+ (/ wire-post-height -2) 3) ])
        (rotate (/ α -2) [1 0 0])
        (translate [3 (/ mount-height -2) 0])))

(def wire-posts
  (union
     (thumb-ml-place (translate [-5 0 -2] (wire-post  1 0)))
     (thumb-ml-place (translate [ 0 0 -2.5] (wire-post -1 6)))
     (thumb-ml-place (translate [ 5 0 -2] (wire-post  1 0)))
     (for [column (range 0 lastcol)
           row (range 0 cornerrow)]
       (union
        (key-place column row (translate [-5 0 0] (wire-post 1 0)))
        (key-place column row (translate [0 0 0] (wire-post -1 6)))
        (key-place column row (translate [5 0 0] (wire-post  1 0)))))))


;(def wallStart  (map + [0 0  0] (key-position 0 0 (map + (wall-locate2 0 1) [0 (/ mount-height  2) 0]))))
;(def wallStart  (key-position 0 0 [0 0 0]))
;(def wallStart  (key-position 0 0 (wall-locate3 0 1)))
;(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
;(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))


(println 
      (rotate-around-y tenting-angle [(- (first wallStart) 0) (- (second wallStart) 10) 0]))
(println [(- (first wallStart) 0) (- (second wallStart) 10) 0])

(def testCube
    (->>
      (cube 1 1 1)
      (color [1 0 0])))

(def wallTest 
  (union
   ;(for [x (range 0 1)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   ;(for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
   ;(key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
    (->>
      testCube
      (translate [0 0 0.5])
      (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
    (->>
      testCube
      (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post)
      (translate (wall-locate1 0 1))
      (key-place 0 0))
    (->>
      testCube
      (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post)
      (key-place 0 0))
    (->>
      testCube
      (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post)
      (translate (wall-locate2 0 1))
      (key-place 0 0))
    (->>
      (cube 1 1 1)
      (color [0 0 1])
      (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post)
      (translate (wall-locate3 0 1))
      (key-place 0 0))
    (->>
      (cube 1 1 1)
      (color [0 0 1])
      (translate [0 0 0.5])
      (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0])
      (translate (wall-locate3 0 1))
      (key-place 1 0))
    (->>
      web-post-tr
      (color [0 1 0]))
    (->>
      (cube 1 1 1)
      (color [0 0 0])
      ;(translate [(+ 5 (first test-pos)) (+ 5 (second test-pos)) (- (/ (first trs-holder-dims) 2) (/ (+ (* -1 trshold-thick) trshold-wireOffset) 2))])
      (translate [0 -5 0])
      (translate wallPos)
      )
    (->>
      ;this cube is placed at the x position of the first key, and attempts to be lined up with the wall.
      (cube 1 1 1)
      (color [0 0 0])
      (translate [(first (key-position 0 1 [0 0 0])) (wallY 0) 0.5])
      )
    (->>
      ;this cube gets placed at the center of the top left key
      (cube 1 1 1)
      (color [0 0 0])
      ;(translate [(+ 5 (first test-pos)) (+ 5 (second test-pos)) (- (/ (first trs-holder-dims) 2) (/ (+ (* -1 trshold-thick) trshold-wireOffset) 2))])
      (key-place 0 0)
      )))

(def model-right 
  (difference
                   (union
                    key-holes
                    connectors
                    thumb
                    thumb-connectors
                    (difference (union case-walls
                                       screw-insert-outers
                                       ;trs-placed
                                       usbHoldPlaced
                                       ;teensy-holder
                                       ;usb-holder
                                       proMicroHoldPlaced)
                                 ;rj9-space
                                ;trs-space
                                ;usb-holder-hole
                                ;trs-cuts-placed
                                usbHoldCutPlaced
                                screw-insert-holes)
                     ;rj9-holder
                    ;wire-posts
                    ; thumbcaps
                    ;caps
                    )
                   (translate [0 0 -20] (cube 350 350 40))
                  )
  )

(def leftTrsOffset 7)
(def model-left 
  (difference
    (union
      (->> 
        trs-placed
        (translate [(- (* -2 (first test-pos)) leftTrsOffset) 0 0])
      )
      (->> 
        model-right
        (mirror [-1 0 0])
      )
    )
    (->> 
      trs-cuts-placed
      (translate [(- (* -2 (first test-pos)) leftTrsOffset) 0 0])
    )
  )
)

(def model-right
  (difference 
    (union
      model-right
      trs-placed
      ;wallTest
    )
    trs-cuts-placed
  )
)


(def connTest 
      (intersection 
                (->> 
                  ;(cube 100 100 100)
                  ;(cube 1000 1000 1000)
                  (cube 80 80 40)
                  (translate [-54 30 20])
                )
                model-right))



;(spit "things/right-test.scad"
;      (write-scad
;                   (union
;                    key-holes
;                    connectors
;                    thumb
;                    thumb-connectors
;                    case-walls
;                    thumbcaps
;                    caps
;                    teensy-holder
;                    trs-model
;                    ; rj9-holder
;                    ; usb-holder-hole
;                    ; usb-holder-hole
;                    ; ; teensy-holder-hole
;                    ;             screw-insert-outers
;                    ;             teensy-screw-insert-holes
;                    ;             teensy-screw-insert-outers
;                    ;             usb-cutout
;                    ;             rj9-space
;                                ; wire-posts
;                  )))
(def right-plate
    (cut
        (translate [0 0 -0.1]
          (difference (union case-walls
                            teensy-holder
                            ; rj9-holder
                            screw-insert-outers)
                      (translate [0 0 -10] screw-insert-screw-holes))
                  )))

(def testscad
         ;(difference usb-holder usb-holder-hole))
         ;(union trs-model trs-placed trs-box))
         (union 
           ;proMicroHold
           ;proMicroHoldStop
           ;single-plate
           trs-holder
           ;pro-micro-box
         )
       )

;(spit "things/test.scad" (write-scad testscad))
(spit "things/right.scad" (write-scad model-right))
;(spit "things/left.scad" (write-scad model-left))
(spit "things/connTest.scad" (write-scad connTest))
;(spit "things/cube.scad" (write-scad (cube 10 10 10)))
;(spit "things/right-plate.scad" (write-scad right-plate))

(def geotest
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> (sa-cap (if (= column 5) 1 1))
                (key-place column row)))))

(spit "things/geotest.scad" (write-scad geotest))



(defn -main [dum] 1)  ; dummy to make it easier to batch
