#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require (only-in typed/racket/gui/base put-file get-file))
(require typed/test-engine/racket-tests)


(define-type Player (U 'Black 'White))

(define-struct OccupiedPoint
    ([color : Player]
     [count : Integer]))

(define-type Point (U OccupiedPoint 'EmptyPoint))

(define-struct Board
    ([points : (Listof Point)]
     [black-bar : Integer]
     [white-bar : Integer]
     [black-off : Integer]
     [white-off : Integer]))

(define-struct Style
    ([checker-radius : Integer]
     [spacing : Integer]
     [black-checker : (Integer -> Image)]
     [white-checker : (Integer -> Image)]
     [dark-point : (Integer Boolean -> Image)]
     [light-point : (Integer Boolean -> Image)]
     [background : (Integer Integer -> Image)]
     [label : (String -> Image)]
     [black-die : (Integer Integer -> Image)]
     [white-die : (Integer Integer -> Image)]))

(define-struct Game
    ([board : Board]
     [turn : Player]
     [moves : (Listof Integer)]))

(define-struct PointNum
  ([num : Integer]))

(define-type ClickLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'BlackDice 'WhiteDice 'Nowhere))

(define-type BoardLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'Nowhere))

(define-struct Pair
  ([a : Integer]
   [b : Integer]))

(define-struct World
  ([game : Game]
   [style : Style]
   [highlighted : Integer]
   [rectangles : (Listof Image-Color)]
   [black-num : Pair]
   [white-num : Pair]
   [history : (Listof Game)]))


(: black-checker (Integer -> Image))
;; This function produces the image of a black checker.
;; It consumes an Integer and outputs an Image.
(define (black-checker radius)
  (overlay (circle radius 'outline 'goldenrod)
           (circle radius 'solid "black")))

;; (black-checker 3)
;; (black-checker 30)


(: white-checker (Integer -> Image))
;; This function produces the image of a white checker.
;; It consumes an Integer and outputs an Image.
(define (white-checker radius)
  (overlay (circle radius 'outline 'goldenrod)
           (circle radius 'solid "white")))

;; (white-checker 3)
;; (white-checker 30)


(: general-point (Integer Image-Color -> Image))
;; This function draws an imgae of a point in its general form.
;; It consumes an Integer and outputs an Image.
(define (general-point radius color)
  (triangle/sss (sqrt (+ (expt (* 10 radius) 2) (expt radius 2)))
                (sqrt (+ (expt (* 10 radius) 2) (expt radius 2)))
                (* 2 radius) 'solid color))

;; (general-point 20 'pink)
;; (general-point 35 'yellow)


(: dark-point (Integer Boolean -> Image))
;; This function draws an image of a dark point.
;; It consumes an Integer and a Boolean value and returns an Image.
(define (dark-point radius boolean)
  (if boolean
      (flip-vertical (general-point radius 'brown))
      (general-point radius 'brown)))

;; (dark-point 26 #f)
;; (dark-point 32 #t)


(: light-point (Integer Boolean -> Image))
;; This function draws an image of a light point.
;; It consumes an Integer and a Boolean value and returns an Image.
(define (light-point radius boolean)
  (if boolean
      (flip-vertical (general-point radius 'tan))
      (general-point radius 'tan)))

;; (dark-point 26 #f)
;; (dark-point 32 #t)


(: label (String -> Image))
;; This function creates a label showing the total numebr of checkers on a
;; point. It consumes a String and outputs an Image.
(define (label string)
  (text (string-append "total: " string) 10 'green))

;; (label "hello")
;; (label "cs151")


(: background (Integer Integer -> Image))
;; This function draws the background, whihch is a rectangle of the appropriate
;; width and height to serve as the backdrop for the points, the bar, and the
;; area where you will show the checkers that have been borne out.
;; It consumes two Integers and outputs an Image.
(define (background radius spacing)
  (beside
   (overlay
    (beside
     (rectangle (+ (* 12 radius) (* 5 spacing)) (* 24 radius) 'solid 'wheat)
     (rectangle (* 3 radius) 0 'solid 'white)
     (rectangle (+ (* 12 radius) (* 5 spacing)) (* 24 radius) 'solid 'wheat))
    (rectangle (+ (* 28 radius) (* 10 spacing)) (* 25 radius) 'solid 'peru))
   (rectangle (* 5 radius) (* 25 radius) 'solid 'peru)))

;; (background 20 20)
;; (background 32 34)


;; Below are some test-lists defined for "eye-ball" tests.
(define list1 (list (OccupiedPoint 'White 2)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'Black 5)
               'EmptyPoint
               (OccupiedPoint 'Black 3)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'White 5)
               (OccupiedPoint 'Black 5)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'White 3)
               'EmptyPoint
               (OccupiedPoint 'White 5)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'Black 2)))

(define list2 (list 'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'White 3)
               (OccupiedPoint 'White 1)
               'EmptyPoint
               (OccupiedPoint 'Black 4)
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'White 3)
               (OccupiedPoint 'Black 2)
               (OccupiedPoint 'Black 4)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'Black 2)
               (OccupiedPoint 'White 1)
               (OccupiedPoint 'Black 4)
               (OccupiedPoint 'White 1)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'Black 1)))

(define list3 (list (OccupiedPoint 'White 6)
               (OccupiedPoint 'Black 6)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint))

(define list4 (list (OccupiedPoint 'Black 2)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'White 5)
               'EmptyPoint
               (OccupiedPoint 'White 3)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'Black 5)
               (OccupiedPoint 'White 5)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'Black 3)
               'EmptyPoint
               (OccupiedPoint 'Black 5)
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               'EmptyPoint
               (OccupiedPoint 'White 2)))

(define list5 (list (color 200 0 0 )
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 200 0)
                    (color 0 0 200)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 0 0 0 0)
                    (color 200 0 0)
                    (color 100 100 0)
                    (color 200 200 0)))

(define list6 (make-list 26 (color 0 0 0 0)))


(: is-number : Integer -> Integer)
;; This function guarantees that a number is a number.
;; it consumes an Integer and ouputs an Integer.
(define (is-number n)
  n)

(check-expect (is-number 22) 22)
(check-expect (is-number 10) 10)



(: generate-random-nums : -> Pair)
;; This function generates a pair of random numbers
;; This function takes no parameter and outputs a Pair.
(define (generate-random-nums)
  (Pair (is-number (random 1 7)) (is-number (random 1 7))))
          

(: die (Integer Integer Image-Color Image-Color -> Image))
;; This function draws a picture of a dice which displays the randomly generated
;; number and which look like a real dice.
;; This functions takes in the checker radius, the random number, the color of
;; the dots and the color of the background, and outputs an Image.
(define (die radius num dot-color bg-color)
  (if (= num 0)
      empty-image
      (local
        {(define two-dots
           (beside (circle (* 0.08 radius) "solid" dot-color)
                   (circle (* 0.08 radius) "solid" bg-color)
                   (circle (* 0.08 radius) "solid" dot-color)))}
        (overlay
         (match num
           [1 (circle (* 0.2 radius) "solid" dot-color)]
           [2 (rotate 45 two-dots)]
           [3 (rotate 45
                      (beside (circle (* 0.08 radius) "solid" dot-color)
                              (circle (* 0.08 radius) "solid" bg-color)
                              (circle (* 0.08 radius) "solid" dot-color)
                              (circle (* 0.08 radius) "solid" bg-color)
                              (circle (* 0.08 radius) "solid" dot-color)))]
           [4 (above two-dots
                     (circle (* 0.08 radius) "solid" bg-color)
                     two-dots)]
           [5 (above two-dots
                     (circle (* 0.08 radius) "solid" dot-color)
                     two-dots)]
           [6 (above two-dots
                     (circle (* 0.08 radius) "solid" bg-color)
                     two-dots
                     (circle (* 0.08 radius) "solid" bg-color)
                     two-dots)]) 
         (overlay (square radius "outline" "purple")
                  (square radius "solid" bg-color))))))

;; (die 30 3 "black" "white")
;; (die 35 6 "yellow" "red")
;; (die 45 6 "blue" "green")


(: black-die (Integer Integer -> Image))
;; This function draws the image of a black die.
;; This function consumes the checker radius, the random number,
;; and outputs an Image of black-die.
(define (black-die radius num)
  (die radius num "white" "black"))

;; (black-die 20 3)
;; (black-die 30 5)
;; (black-die 40 6)


(: white-die (Integer Integer -> Image))
;; This function draws the image of a white die.
;; This function consumes the checker radius, the random number,
;; and outputs an Image of white-die.
(define (white-die radius num)
  (die radius num "black" "white"))

;; (white-die 20 2)
;; (white-die 30 4)
;; (white-die 30 6)


;; This is the test-style defined for testing purposes.
(define test-style
  (Style 20 10 black-checker white-checker
         dark-point light-point background label black-die white-die))


;; Below are some test-boards written for "eye-ball tests."
(define test-board1
  (Board list1 0 0 0 0))
                     
(define test-board2
  (Board list2 0 1 0 5 ))

(define test-board3
  (Board list3 6 3 3 6))

(define test-board4
  (Board list4 0 0 0 0))

(define test-board5
  (Board list4 0 0 15 0))


(: draw-board : Style Board -> Image)
;; This function draws a complete picture of the board with all its dimensions
;; and states specified by the Board and Style.
;; It consumes a Board and a Style and output an Image.
(define (draw-board style board)
  (match* (style board)
    [((Style radius spacing b-checker w-checker
             d-point l-point bg l black-die white-die)
      (Board points black-bar white-bar black-off white-off))
     (local
       {(: stack-checkers (Player Integer Integer -> Image))
        ;; This function draws an Image showing a stack of checkers.
        ;; It consumes a Player, two Integers, and outputs an Image.
        (define (stack-checkers color count radius)
          (cond
            [(zero? count) empty-image]
            [else
             (match color
               ['Black
                (above
                 (b-checker radius)
                 (stack-checkers color (- count 1) radius))]
               ['White
                (above
                 (w-checker radius)
                 (stack-checkers color (- count 1) radius))])]))

        (: point-image (Point Integer Boolean (Integer Boolean -> Image)
                              -> Image))
        ;; This function draws an Image of checkers on point. If there are  5
        ;; or fewer.
        ;; than 5 checkers, all checkers are shown. If there are more than 5
        ;; checkers,
        ;; then only 5 are shown with a label displaying the total number of
        ;; checkers.
        ;; The function consumes a Point, an Integer, a Boolean, and a function
        ;; that draws point, and outputs an Image.
        (define
          (point-image point radius boolean draw-point)
          (match point
            ['EmptyPoint (draw-point radius boolean)]
            [(OccupiedPoint color count)
             (cond
               [(<= count 5)
                (if boolean
                    (overlay/align "middle" "bottom"
                                   (stack-checkers color count radius)
                                   (draw-point radius boolean))
                    (overlay/align "middle" "top"
                                   (stack-checkers color count radius)
                                   (draw-point radius boolean)))]
               [else (overlay (l (number->string count))
                              (stack-checkers color 5 radius)
                              (draw-point radius boolean))])]))

        (: lower-points ((Listof Point) Integer Integer Integer Integer
                                        -> Image))
        ;; This function draws the lower part of the 24 points, with the
        ;; approariate number and color of checkers on each point as specified.
        ;; It consumes a List of Point, 3 Integers, and outputs an Image.
        ;; For right-lower-points, index starts at 1 and base-number is 6.
        ;; For left-lower-points, index starts at 7 and base-number is 12.
        (define (lower-points points radius spacing base-num index)
          (cond
            [(<= index (- base-num 1))
             (if (odd? index)
                 (beside (lower-points points radius spacing base-num
                                       (+ index 1))
                         (rectangle spacing 0 'solid 'white)
                         (point-image (list-ref points (- index 1))
                                      radius #t d-point))
                 (beside (lower-points points radius spacing base-num
                                       (+ index 1))
                         (rectangle spacing 0 'solid 'white)
                         (point-image (list-ref points (- index 1))
                                      radius #t l-point)))]
            [(= index base-num)
             (beside
              (point-image (list-ref points (- index 1))
                           radius #t l-point)
              (lower-points points radius spacing base-num (+ index 1)))]
            [else empty-image]))

        (: upper-points ((Listof Point) Integer Integer Integer Integer
                                        -> Image))
        ;; This function draws the upper part of the 24 points, with the
        ;; approariate number and color of checkers on each point as specified.
        ;; It consumes a List of Point, 3 Integers, and outputs an Image.
        ;; For left-upper-points, index starts at 13 and base-number is 18.
        ;; For right-upper-points, index starts at 19 and base-number is 24.
        (define (upper-points points radius spacing base-num index)
          (cond
            [(<= index (- base-num 1))
             (if (odd? index)
                 (beside (point-image (list-ref points (- index 1))
                                      radius #f d-point)
                         (rectangle spacing 0 'solid 'white)
                         (upper-points points radius spacing base-num
                                       (+ index 1)))
                 (beside (point-image (list-ref points (- index 1))
                                      radius #f l-point)
                         (rectangle spacing 0 'solid 'white)
                         (upper-points points radius spacing base-num
                                       (+ index 1))))]
            [(= index base-num)
             (beside
              (upper-points points radius spacing base-num (+ index 1))
              (point-image (list-ref points (- index 1))
                           radius #f l-point))]
            [else empty-image]))

        (: bar (Integer Integer Integer -> Image))
        ;; This function draws the bar in the middle with specified number of
        ;; white checker and black checkers. If the number of checkers is less
        ;; than 5, all checkers are shown. Otherwise, only 5 checkers are shown
        ;; and a label indicates the total number of checkers.
        ;; This function consumes 3 Integers and returns an image.
        (define (bar radius black-bar white-bar)
          (overlay/align
           "middle" "top"
           (cond
             [(<= black-bar 5)
              (stack-checkers 'Black black-bar radius)]
             [else (overlay (l (number->string black-bar))
                            (stack-checkers 'Black 5 radius))])
           (overlay/align
            "middle" "bottom"
            (cond
              [(<= white-bar 5)
               (stack-checkers 'White white-bar radius)]
              [else (overlay (l (number->string white-bar))
                             (stack-checkers 'White 5 radius))])
            (overlay
             (rectangle (* 0.3 radius) (* 25 radius) 'solid 'black)
             (rectangle (* 3 radius) (* 25 radius) 'solid 'peru)))))

        (: borne-off-area (Integer Integer Integer -> Image))
        ;; This function draws the borne-off area with specified number of
        ;; white checker and black checkers. If the number of checkers is
        ;; less than 5, all checkers are shown. Otherwise, only 5 checkers are
        ;; shown and a label indicates the total number of checkers.
        ;; This function consumes 3 Integers and returns an image.
        (define (borne-off-area radius black-off white-off)
          (overlay
           (overlay/align
            "middle" "top"
            (cond
              [(<= black-off 5)
               (stack-checkers 'Black black-off radius)]
              [else (overlay (l (number->string black-off))
                             (stack-checkers 'Black 5 radius))])
            (overlay/align
             "middle" "bottom"
             (cond
               [(<= white-off 5)
                (stack-checkers 'White white-off radius)]
               [else (overlay (l (number->string white-off))
                              (stack-checkers 'White 5 radius))])
             (overlay
              (rectangle (* 4 radius) (* 4 radius) 'solid 'black)
              (rectangle (* 4 radius) (* 24 radius) 'solid 'wheat))))
           (rectangle (* 5 radius) (* 25 radius) 'solid 'peru)))

        (: left-board-points (Integer Integer (Listof Point) -> Image))
        ;; This function draws all the points on the left side of the board.
        ;; It consumes two Integers and a List of Point and outputs an Image.
        (define (left-board-points radius spacing points)
          (above
           (upper-points points radius spacing 18 13)
           (rectangle 0 (* 4 radius) 'solid 'white)
           (lower-points points radius spacing 12 7)))

        (: right-board-points (Integer Integer (Listof Point) -> Image))
        ;; This function draws all the points on the right side of the board.
        ;; It consumes two Integers and a List of Point and outputs an Image.
        (define (right-board-points radius spacing points)
          (above
           (upper-points points radius spacing 24 19)
           (rectangle 0 (* 4 radius) 'solid 'white)
           (lower-points points radius spacing 6 1)))}

       (overlay
        (beside
         (left-board-points radius spacing points)
         (bar radius black-bar white-bar)
         (right-board-points radius spacing points)
         (borne-off-area radius black-off white-off))
        (bg radius spacing)))]))

;; (draw-board test-style test-board1)
;; (draw-board test-style test-board2)
;; (draw-board test-style test-board3)


(: draw-board-updated : Style Board Pair Pair -> Image)
;; This function is the updated version of the draw-board function,
;; now including the dice.
;; This function consumes a style, a board, and two pairs and
;; outputs an Image.
(define (draw-board-updated style board p1 p2)
  (match* (style board)
    [((Style r s black-checker white-checker
             dark-point light-point background label b-die w-die)
      (Board points black-bar white-bar black-off white-off))
     (match* (p1 p2)
       [((Pair b1 b2) (Pair w1 w2))
        (overlay/xy
         (beside
          (b-die r b1)
            (rectangle s 0 "solid" "white")
            (b-die r b2))
        (* -1 (+ (* 20.5 r) (* 7 s))) (* -1 (* 12 r))
         (overlay/xy
          (beside
            (w-die r w1)
            (rectangle s 0 "solid" "white")
            (w-die r w2))
          (* -1 (+ (* 5.5 r) (* 2 s))) (* -1 (* 12 r))
          (draw-board style board)))])]))
          
;; (draw-board-updated test-style test-board1 (Pair 1 1) (Pair 3 4))
;; (draw-board-updated test-style test-board2 (Pair 1 6) (Pair 2 4))
;; (draw-board-updated test-style test-board3 (Pair 1 5) (Pair 4 4))


(: point-location : Integer Integer Integer Integer Integer ->
   (U PointNum 'Nowhere))
;; This function computes where the user clicked based on the x coordinate
;; and outputs the number of the point that is clicked on.
;; It takes in the x coordiate of the origin, the x coordinate of the click
;; location, the base number, the radius and the spacing, and outputs the
;; ordering of the point.
;; The base number is 6 for the lower-right points, 12 for the lower-left
;; points, 18 for the upper-left points, and 24 for the upper- right points
(define (point-location origin-x x base-num radius spacing)
  (local
    {(define diff (abs (- x origin-x)))}
    (if (= 0 (quotient diff (* 2 radius)))
        (PointNum base-num)
        (if (> (quotient
                (- diff (* spacing (quotient diff (+ (* 2 radius) spacing))))
                (* 2 radius))
               (quotient diff (+ (* 2 radius) spacing)))
            'Nowhere
            (PointNum
             (- base-num (quotient diff (+ (* 2 radius) spacing))))))))

(check-expect (point-location 1 26 12 2 2) (PointNum 8))
(check-expect (point-location 35 2 18 2 2) (PointNum 13))
(check-expect (point-location 35 12 18 2 2) 'Nowhere)
(check-expect (point-location 41 42 6 2 2) (PointNum 6))
(check-expect (point-location 75 10 24 2 2) 'Nowhere)
(check-expect (point-location 75 42 24 2 2) (PointNum 19))


(: click-where : Style Integer Integer -> ClickLoc)
;; This function computes where on the board has the mouse clicked.
;; It takes in a style and the x/y coordinates of the click location
;; and outputs a ClickLoc.
;; Note: in style, r means radius and s means spacing.
(define (click-where style x y)
  (match style
    [(Style r s black-checker white-checker
            dark-point light-point background label black-die white-die)
     (cond
       [(< (+ (* 12.5 r) (* 5 s)) x (+ (* 15.5 r) (* 5 s)))
        (cond
          [(< 0 y (* 10 r)) 'BlackBar]
          [(< (* 15 r) y (* 25 r)) 'WhiteBar]
          [else 'Nowhere])]
       [(< (+ (* 28 r) (* 10 s)) x (+ (* 32 r) (* 10 s)))
        (cond
          [(< (* 0.5 r) y (* 10.5 r)) 'BlackOff]
          [(< (* 14.5 r) y (* 24.5 r)) 'WhiteOff]
          [else 'Nowhere])]
       [(< (* 12 r) y (* 13 r))
        (cond
          [(< (+ (* 5.5 r) (* 2 s)) x (+ (* 7.5 r) (* 3 s))) 'WhiteDice]
          [(< (+ (* 20.5 r) (* 7 s)) x (+ (* 22.5 r) (* 8 s))) 'BlackDice]
          [else 'Nowhere])]
       [(and (< (* 14.5 r) y (* 24.5 r))
             (< (+ (* 15.5 r) (* 5 s)) x (+ (* 27.5 r) (* 10 s))))
        (point-location (exact-round (+ (* 15.5 r) (* 5 s))) x 6 r s)]
       [(and (< (* 14.5 r) y (* 24.5 r))
             (< (* 0.5 r) x (+ (* 12.5 r) (* 15 s))))
        (point-location (exact-round (* 0.5 r)) x 12 r s)]
       [(and (< (* 0.5 r) y (* 10.5 r))
             (< (+ (* 15.5 r) (* 5 s)) x (+ (* 27.5 r) (* 10 s))))
        (point-location (exact-round (+ (* 27.5 r) (* 10 s))) x 24 r s)]
       [(and (< (* 0.5 r) y (* 10.5 r))
             (< (* 0.5 r) x (+ (* 12.5 r) (* 15 s))))
        (point-location (exact-round (+ (* 12.5 r) (* 5 s))) x 18 r s)]
       [else 'Nowhere])]))

(check-expect (click-where test-style 130 20) (PointNum 15))
(check-expect (click-where test-style 130 295) (PointNum 10))
(check-expect (click-where test-style 365 295) (PointNum 6))
(check-expect (click-where test-style 305 20) 'BlackBar)
(check-expect (click-where test-style 305 330) 'WhiteBar)
(check-expect (click-where test-style 670 20) 'BlackOff)
(check-expect (click-where test-style 670 295) 'WhiteOff)
(check-expect (click-where test-style 133 243) 'WhiteDice)
(check-expect (click-where test-style 487 243) 'BlackDice)
(check-expect (click-where test-style 3 3) 'Nowhere)
(check-expect (click-where test-style 30 220) 'Nowhere)
           

;; Documentation:
;; The function "replace-at" is directly borroowed from my own lab4 assignment.

(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; replace the item at the given position
;; position counting starts at 0
;; ex: (replace-at 0 'Z '(a b c)) -> '(Z b c)
;; ex: (replace-at 1 'Z '(a b c)) -> '(a Z c)
(define (replace-at i x xs)
  (if (<= i (- (length xs) 1))
      (match i
        [0 (cons x (rest xs))]
        [_ (cons (first xs) (replace-at (- i 1) x (rest xs)))])
      (error "replace-at: the given position cannot be found in the list")))

(check-expect (replace-at 0 'EmptyPoint (list (OccupiedPoint 'White 3)
                                              'EmptyPoint
                                              'EmptyPoint))
              (list 'EmptyPoint 'EmptyPoint 'EmptyPoint))
(check-expect (replace-at 2 (OccupiedPoint 'Black 8)
                          (list (OccupiedPoint 'White 3)
                                'EmptyPoint
                                'EmptyPoint))
              (list (OccupiedPoint 'White 3)
                    'EmptyPoint (OccupiedPoint 'Black 8)))
(check-expect (replace-at 1 'Z '(a b c)) '(a Z c))
(check-expect (replace-at 3 'ds '(a b c d e)) '(a b c ds e))
(check-error (replace-at 3 's '(a bc cd))
             "replace-at: the given position cannot be found in the list")

             
(: previous-apply-move : Board BoardLoc BoardLoc -> Board)
;; This function computes what move the player makes according to the original
;; and final click location. It raises an error if the move is illegitimate。
;; It consumes a Board, 2 BoardLocs and outputs a Board.
(define (previous-apply-move board loc1 loc2)
  (match board
    [(Board points b-b w-b b-o w-o)
     (local
       {(: new-point : Point -> Point)
        ;; This function computes the new Point when a checker is removed from
        ;; original Point.
        ;; It consumes a Point and returns a Point.
        (define (new-point old-point)
          (match old-point
            [(OccupiedPoint player count)
             (if (= 1 count) 'EmptyPoint
                 (OccupiedPoint player (sub1 count)))]))

        (: point->off : Board BoardLoc (U 'WhiteOff 'BlackOff) -> Board)
        ;; This function computes the move as the checker is transferred from
        ;; a Point to the corresponding bear-off area.
        ;; It consumes a Board, a Location, and a bear-off area, and it outputs
        ;; a Board.
        (define (point->off board location color)
          (match location
            [(PointNum num)
             (match* ((list-ref points (sub1 num)) color)
               [((OccupiedPoint 'Black count) 'BlackOff)
                (Board (replace-at (sub1 num)
                                   (new-point (list-ref points (sub1 num)))
                                   points) b-b w-b (add1 b-o) w-o)]
               [((OccupiedPoint 'White count) 'WhiteOff)
                (Board (replace-at (sub1 num)
                                   (new-point (list-ref points (sub1 num)))
                                   points) b-b w-b b-o (add1 w-o))])]
            [_ (error "point->off: BoardLoc out of range")]))

        (: point->point : Board BoardLoc BoardLoc -> Board)
        ;; This function computes the move as the checker is transferred from
        ;; a Point to another Point.
        ;; It consumes a Board, two Locations, and it outputs a Board.
        (define (point->point board location1 location2)
          (match* (location1 location2)
            [((PointNum num1) (PointNum num2))
             (local
               {(define new-points
                  (replace-at (sub1 num1)
                              (new-point
                               (list-ref points (sub1 num1))) points))}
               (if (= num1 num2) (error "point->point: same point")
                   (match* ((list-ref points (sub1 num1))
                            (list-ref points (sub1 num2)))
                     [((OccupiedPoint player _) 'EmptyPoint)
                      (Board
                       (replace-at
                        (sub1 num2) (OccupiedPoint player 1) new-points)
                       b-b w-b b-o w-o)]
                     [((OccupiedPoint player1 count1)
                       (OccupiedPoint player2 count2))
                      (if (symbol=? player1 player2)
                          (Board (replace-at
                                  (sub1 num2)
                                  (OccupiedPoint player2 (add1 count2))
                                  new-points) b-b w-b b-o w-o)
                          (if (= 1 count2)
                              (match player2
                                ['Black (Board
                                         (replace-at
                                          (sub1 num2) (OccupiedPoint 'White 1)
                                          new-points) (add1 b-b) w-b b-o w-o)]
                                ['White (Board
                                         (replace-at
                                          (sub1 num2) (OccupiedPoint 'Black 1)
                                          new-points)
                                         b-b (add1 w-b) b-o w-o)])
                              (error "point->point: invalid move")))])))]))

        (: bar->point : Board (U 'BlackBar 'WhiteBar) BoardLoc -> Board)
        ;; This function computes the move as the checker is transferred from
        ;; the balck bar or the white bar to a Point.
        ;; It consumes a Board, a location, and a bar, and it outputs a Board.
        (define (bar->point board bar location)
          (match location
            [(PointNum num)
             (local
               {(: newpoint : Symbol Point -> Point)
                ;; This function computes the new point when a checker is added
                ;; to that point.
                ;; It consumes a symbol (whether it is the black bar or the
                ;; white bar) and a Point, and it outputs a Point.
                (define (newpoint color point)
                  (match point
                    ['EmptyPoint (OccupiedPoint
                                  (match color
                                    ['WhiteBar 'White]
                                    ['BlackBar 'Black]) 1)]
                    [(OccupiedPoint player count)
                     (OccupiedPoint player (add1 count))]))
                (: new-points : Symbol -> (Listof Point))
                ;; This function computes the new list of Points that is used
                ;; to replace the original list when a checker is added to
                ;; one of the Points from the bar.
                ;; It takes in a symbol and outputs a List of Points.
                (define (new-points bar-color)
                  (replace-at
                   (sub1 num) (newpoint bar-color
                                        (list-ref points (sub1 num))) points))}
               (match bar
                 ['WhiteBar
                  (match (list-ref points (sub1 num))
                    [(or 'EmptyPoint (OccupiedPoint 'White _))
                     (Board (new-points 'WhiteBar) b-b (sub1 w-b) b-o w-o)]
                    [(OccupiedPoint 'Black count2)
                     (if (= 1 count2)
                         (Board (replace-at
                                 (sub1 num) (OccupiedPoint 'White 1) points)
                                (add1 b-b) (sub1 w-b) b-o w-o)
                         (error "bar->point: invalid move"))])]
                 ['BlackBar
                  (match (list-ref points (sub1 num))
                    [(or 'EmptyPoint (OccupiedPoint 'Black _))
                     (Board (new-points 'BlackBar) (sub1 b-b) w-b b-o w-o)]
                    [(OccupiedPoint 'White count2)
                     (if (= 1 count2)
                         (Board (replace-at
                                 (sub1 num) (OccupiedPoint 'Black 1) points)
                                (sub1 b-b) (add1 w-b) b-o w-o)
                         (error "bar->point: invalid move"))])]))]))}

       (match* (loc1 loc2)
         [((PointNum num) (or 'WhiteOff 'BlackOff))
          (point->off board loc1 loc2)]
         [((PointNum num1) (PointNum num2))
          (point->point board loc1 loc2)]
         [((or 'WhiteBar 'BlackBar) (PointNum num))
          (bar->point board loc1 loc2)]
         [(_ _) (error "apply-move: invalid move!")]))]))

(check-expect (previous-apply-move test-board1 (PointNum 1) 'WhiteOff)
              (Board (replace-at 0 (OccupiedPoint 'White 1)
                                 (Board-points test-board1)) 0 0 0 1))
(check-error (previous-apply-move test-board1 'WhiteBar 'WhiteOff)
             "apply-move: invalid move!")
(check-expect (previous-apply-move test-board2 'WhiteBar (PointNum 24))
              (Board (replace-at 23 (OccupiedPoint 'White 1)
                                 (Board-points test-board2)) 1 0 0 5))
(check-expect (previous-apply-move test-board2 'WhiteBar (PointNum 3))
              (Board (replace-at 2 (OccupiedPoint 'White 4)
                                 (Board-points test-board2)) 0 0 0 5))
(check-error (previous-apply-move test-board2 'WhiteBar (PointNum 6))
             "bar->point: invalid move")
(check-expect (previous-apply-move test-board2 'WhiteBar (PointNum 7))
              (Board
               (list
                'EmptyPoint
                'EmptyPoint
                (OccupiedPoint 'White 3)
                (OccupiedPoint 'White 1)
                'EmptyPoint
                (OccupiedPoint 'Black 4)
                (OccupiedPoint 'White 1)
                'EmptyPoint
                (OccupiedPoint 'White 3)
                (OccupiedPoint 'Black 2)
                (OccupiedPoint 'Black 4)
                'EmptyPoint
                'EmptyPoint
                'EmptyPoint
                'EmptyPoint
                'EmptyPoint
                (OccupiedPoint 'Black 2)
                (OccupiedPoint 'White 1)
                (OccupiedPoint 'Black 4)
                (OccupiedPoint 'White 1)
                'EmptyPoint
                'EmptyPoint
                'EmptyPoint
                (OccupiedPoint 'Black 1)) 0 0 0 5))


(: highlight-image : Integer Integer Integer -> Image)
;; This function highlight individual points and the black/white bars
;; by putting a red rectangle on the respcetive area. Note that all 26
;; rectangles are present at all times. However, all but one is transparent.
;; This function consumes radius, spacing, and the highlighted number,
;; and outputs an Image.
(define (highlight-image radius spacing highlighted)
  (local
    {(: rectangle-image : Image-Color Integer -> Image)
     ;; This functions draws a rectangle image that is used to highlight
     ;; points and bars.
     ;; This function takes in a color and a radius and outputs an Image.
     (define (rectangle-image color radius)
       (rectangle (* 2 radius) (* 10 radius) "solid" color))

     (: lower-rectangles ((Listof Image-Color)
                          Integer Integer Integer Integer -> Image))
     ;; This function takes in a list of colors, radius, spacing, a base number
     ;; and an index and outputs the lower part of the complete
     ;; highlight cover
     ;; For right-lower-rect, index starts at 1 and base-number is 6.
     ;; For left-lower-rects, index starts at 7 and base-number is 12.
     (define (lower-rectangles rects radius spacing base-num index)
       (cond
         [(<= index (- base-num 1))
          (beside (lower-rectangles rects radius spacing base-num (+ index 1))
                  (rectangle spacing 0 'solid 'white)
                  (rectangle-image (list-ref rects (- index 1))
                                   radius))]
         [(= index base-num)
          (beside
           (rectangle-image (list-ref rects (- index 1))
                            radius)
           (lower-rectangles rects radius spacing base-num (+ index 1)))]
         [else empty-image]))

     (: upper-rectangles ((Listof Image-Color)
                          Integer Integer Integer Integer -> Image))
     ;; This function takes in a list of colors, radius, spacing, a base number
     ;; and an index and outputs the upper part of the complete
     ;; highlight cover.
     ;; For left-upper-points, index starts at 13 and base-number is 18.
     ;; For right-upper-points, index starts at 19 and base-number is 24.
     (define (upper-rectangles rects radius spacing base-num index)
       (cond
         [(<= index (- base-num 1))
          (beside (rectangle-image (list-ref rects (- index 1))
                                   radius)
                  (rectangle spacing 0 'solid 'white)
                  (upper-rectangles
                   rects radius spacing base-num (+ index 1)))]
         [(= index base-num)
          (beside
           (upper-rectangles rects radius spacing base-num (+ index 1))
           (rectangle-image (list-ref rects (- index 1))
                            radius))]
         [else empty-image]))

     (: highlight-image-helper : Integer Integer (Listof Image-Color) -> Image)
     ;;  This function draws a crude picture of the complete highlight cover.
     ;; Note that the output of this helper function can have multiple colored
     ;; rectangles at the same time.
     ;; It takes in two Integers and a list of colors, and outputs an image.
     (define (highlight-image-helper radius spacing rects)
       (beside
        (above (upper-rectangles rects radius spacing 18 13)
               (rectangle 0 (* 4 radius) 'solid 'white)
               (lower-rectangles rects radius spacing 12 7))
        (above
         (rectangle (* 3 radius) (* 10 radius)
                    "solid" (list-ref rects 24))
         (rectangle (* 3 radius) (* 5 radius)
                    "outline" (color 0 0 0 0))
         (rectangle (* 3 radius) (* 10 radius)
                    "solid" (list-ref rects 25)))
        (above (upper-rectangles rects radius spacing 24 19)
               (rectangle 0 (* 4 radius) 'solid 'white)
               (lower-rectangles rects radius spacing 6 1))))}
    
    (local
      {(define list (make-list 26 (color 0 0 0 0)))}
      (cond
        [(= 0 highlighted) (highlight-image-helper radius spacing list)]
        [(<= 1 highlighted 26)
         (highlight-image-helper
          radius spacing (replace-at
                          (sub1 highlighted) (color 255 0 0 100) list))]
        [else (error "highlight-image: not a valid point to highlight")]))))

(check-error (highlight-image 20 10 33)
             "highlight-image: not a valid point to highlight")
;; (highlight-image 20 10 0)
;; (highlight-image 20 10 13)
;; (highlight-image 20 10 25)
;; (highlight-image 20 10 26)


;; The Worlds below are defined for test purposes
(define test-world1 (World (Game test-board3 'White (list 2)) test-style
                           25 list6 (Pair 1 3) (Pair 2 5) '()))
(define test-world2
  (World (Game (Board list2 0 0 15 0) 'White (list 3))
         test-style 0 list6 (Pair 1 3) (Pair 2 5) '()))
;; (define test-world3 (World (Game test-board3 'White (list 4)) test-style
;;                            0 list6 (Pair 1 3) (Pair 2 5)))


(: draw-world-old : World -> Image)
;; This function produces a visualization of the given world.
;; It consumes a World and outputs an Image.
(define (draw-world-old world)
  (match world
    [(World (Game board player moves) style 
            highlighted rects p1 p2 history)
     (match style
       [(Style rad sp black-checker white-checker
               dark-point light-point background label black-die white-die)
        (overlay/xy
         (highlight-image rad sp highlighted)
         (* -1 (* 0.5 rad)) 0
         (draw-board-updated style board p1 p2))])]))

;; (draw-world test-world1)
;; (draw-world test-world2)
;; (draw-world test-world3)



                          
;; ============================================= Phase 3

                     
(: distance : BoardLoc BoardLoc -> Integer)
;; This function computes the distance of the move specified by the two
;; given Boardloc.
;; It consumes an origin and a destination, and returns the "distance" of
;; that move.
(define (distance loc1 loc2)
  (match* (loc1 loc2)
    [((PointNum num1) (PointNum num2))
     (- num2 num1)]
    [('BlackBar (PointNum num)) num]
    [('WhiteBar (PointNum num))
     (* -1 (- 25 num))]
    [((PointNum num) 'BlackOff)
     (- 25 num)]
    [((PointNum num) 'WhiteOff)
     (* -1 num)]
    [(_ _) (error "distance: invalid move")]))

(check-expect (distance (PointNum 3) (PointNum 5)) 2)
(check-expect (distance (PointNum 3) 'WhiteOff) -3)
(check-expect (distance 'WhiteBar (PointNum 22)) -3)
(check-error (distance 'Nowhere 'BlackOff) "distance: invalid move")
(check-expect (distance 'BlackBar (PointNum 5)) 5)
(check-expect (distance 'WhiteBar (PointNum 23)) -2)
(check-expect (distance (PointNum 7) (PointNum 9)) 2)

      
(: present? : Integer (Listof Integer) -> Boolean)
;; This function computes whether the given integer is in the list.
;; It takes in an integer and a list of integer and outputs a Boolean.
(define (present? n nums)
  (match nums
    ['() #f]
    [(cons f r) (if (= n f) #t (present? n r))]))

(check-expect (present? 3 '(3 4 5)) #t)
(check-expect (present? 2 '(3 4 5)) #f)


(: bigger-present? : Integer (Listof Integer) -> Boolean)
;; This function computes whether there are any elements in the list
;; that is bigger than the given integer.
;; It consumes an integer and a list of integers and outputs a Boolean.
(define (bigger-present? n nums)
  (match nums
    ['() #f]
    [(cons f r) (if (> f n) #t (bigger-present? n r))]))

(check-expect (bigger-present? 2 '( 4 5 6 9)) #t)
(check-expect (bigger-present? 10 '( 4 5 6 9)) #f)


(: smaller-present? : Integer (Listof Integer) -> Boolean)
;; This function computes whether there are any elements in the list
;; that is smaller than the given integer.
;; It consumes an integer and a list of integers and outputs a Boolean.
(define (smaller-present? n nums)
  (match nums
    ['() #f]
    [(cons f r) (if (< f n) #t (smaller-present? n r))]))

(check-expect (smaller-present? 2 '( 4 5 6 9)) #f)
(check-expect (smaller-present? 10 '( 4 12 6 9)) #t)


(: point->point? : Game BoardLoc BoardLoc -> Boolean)
;; This function computes whether a move from a point to another point is valid.
;; It takes in a game and two board locations and outputs a Boolean.
;; It is intended as a helper function for legal-move?
(define (point->point? game loc1 loc2)
  (match game
    [(Game (Board points b-b w-b b-o w-o) turn moves)
     (local
       {(: point->point?-helper : Player Player -> Boolean)
        (define (point->point?-helper player1 player2)
          (and (or
                (and (symbol=? 'Black player1)
                     (> (distance loc1 loc2) 0))
                (and (symbol=? 'White player1)
                     (< (distance loc1 loc2) 0)))
               (if (present? (abs (distance loc1 loc2)) moves)
                   (match* (loc1 loc2)
                     [((PointNum num1) (PointNum num2))
                      (match* ((list-ref points (sub1 num1))
                               (list-ref points (sub1 num2)))
                        [((OccupiedPoint player1 _) 'EmptyPoint) #t]
                        [((OccupiedPoint player1 _) (OccupiedPoint player1 _))
                         #t]
                        [((OccupiedPoint player1 _) (OccupiedPoint player2 1))
                         #t]
                        [(_ _) #f])])
                   #f)))}
       (match turn
         ['Black
          (point->point?-helper 'Black 'White)]
         ['White
          (point->point?-helper 'White 'Black)]))]))

(check-expect (point->point? (Game test-board4 'Black '(1 2 3 5))
                             (PointNum 1) (PointNum 3)) #t)
(check-expect (point->point? (Game test-board4 'Black '(1 2 3 5))
                             (PointNum 1) (PointNum 5)) #f)
(check-expect (point->point? (Game test-board4 'Black '(1 2 3 5))
                             (PointNum 12) (PointNum 11)) #f)
(check-expect (point->point? (Game test-board4 'White '(1 2 3 5))
                             (PointNum 24) (PointNum 19)) #f)
(check-expect (point->point? (Game test-board4 'White '(1 2 3 5))
                             (PointNum 24) (PointNum 23)) #t)
 

(: bar->point? : Game BoardLoc BoardLoc -> Boolean)
;; This function computes whether a move from a bar to a point is valid.
;; It takes in a game and two board locations and outputs a Boolean.
;; It is intended as a helper function for legal-move?
(define (bar->point? game loc1 loc2)
  (match game
    [(Game (Board points b-b w-b b-o w-o) turn moves)
     (match turn
       ['Black
        (if (present? (distance loc1 loc2) moves)
            (match* (loc1 loc2)
              [('BlackBar (PointNum num))
               (match (list-ref points (sub1 num))
                 ['EmptyPoint #t]
                 [(OccupiedPoint 'Black _) #t]
                 [(OccupiedPoint 'White 1) #t]
                 [_ #f])])
            #f)]
       ['White
        (if (present? (* -1 (distance loc1 loc2)) moves)
            (match* (loc1 loc2)
              [('WhiteBar (PointNum num))
               (match (list-ref points (sub1 num))
                 ['EmptyPoint #t]
                 [(OccupiedPoint 'White _) #t]
                 [(OccupiedPoint 'Black 1) #t]
                 [_ #f])])
            #f)])]))

(check-expect (bar->point? (Game test-board4 'Black '(1 2 3 5)) 'BlackBar
                           (PointNum 1)) #t)
(check-expect (bar->point? (Game test-board4 'White '(1 2 3 5)) 'WhiteBar
                           (PointNum 23)) #t)
(check-expect (bar->point? (Game test-board4 'White '(1 2 3 5)) 'WhiteBar
                           (PointNum 24)) #t)
(check-expect (bar->point? (Game test-board4 'White '(1 2 3 5)) 'WhiteBar
                           (PointNum 19)) #f)
(check-expect (bar->point? (Game test-board4 'White '(1 5)) 'WhiteBar
                           (PointNum 22)) #f)


(: point->off? : Game BoardLoc BoardLoc -> Boolean)
;; This function computes whether a move from a point to the bear-off area
;; is valid.
;; It takes in a game and two board locations and outputs a Boolean.
;; It is intended as a helper function for legal-move?
(define (point->off? game loc1 loc2)
  (match game
    [(Game board turn moves)
     (match board
       [(Board points b-b w-b b-o w-o)
        (local
          {(: black-bearoff-legal? : Board PointNum -> Boolean)
           ;; This function computes whether it is legal to bear off a checker
           ;; when the checker is on a point lower than the number on the dice.
           ;; It takes in a board and a pointnum and outputs a Boolean.
           ;; It is made sure elsewhere that the input PointNum is bigger than
           ;; 19.
           (define (black-bearoff-legal? board pt)
             (match board
               [(Board points b-b w-b b-o w-o)
                (match pt
                  [(PointNum num)
                   (cond
                     [(= num 20)
                      (match (list-ref points 18)
                        ['EmptyPoint #t]
                        [(OccupiedPoint 'White count) #t]
                        [_ #f])]
                     [else
                      (and (match (list-ref points (- num 2))
                             ['EmptyPoint #t]
                             [(OccupiedPoint 'White count) #t]
                             [_ #f])
                           (black-bearoff-legal? board
                                                 (PointNum (sub1 num))))])])]))

           (: white-bearoff-legal? : Board PointNum -> Boolean)
           ;; This function computes whether it is legal to bear off a checker
           ;; when the checker is on a point lower than the number on the dice.
           ;; It takes in a board and a pointnum and outputs a Boolean.
           ;; It is made sure elsewhere that the input PointNum is smaller than
           ;; 6.
           (define (white-bearoff-legal? board pt)
             (match board
               [(Board points b-b w-b b-o w-o)
                (match pt
                  [(PointNum num)
                   (cond
                     [(= num 5)
                      (match (list-ref points 5)
                        ['EmptyPoint #t]
                        [(OccupiedPoint 'Black count) #t]
                        [_ #f])]
                     [else
                      (and (match (list-ref points num)
                             ['EmptyPoint #t]
                             [(OccupiedPoint 'Black count) #t]
                             [_ #f])
                           (white-bearoff-legal? board
                                                 (PointNum (add1 num))))])])]))}
           
          (match turn
            ['Black
             (if (present? (distance loc1 loc2) moves)
                 (match loc1
                   [(PointNum num)
                    (match (list-ref points (sub1 num))
                      [(OccupiedPoint 'Black _) #t]
                      [_ #f])])
                 (if (bigger-present? (distance loc1 loc2) moves)
                     (match loc1
                       [(PointNum num)
                        (match (list-ref points (sub1 num))
                          [(OccupiedPoint 'Black _)
                           (black-bearoff-legal? board loc1)]
                          [_ #f])])
                     #f))]
            ['White
             (if (present? (* -1 (distance loc1 loc2)) moves)
                 (match loc1
                   [(PointNum num)
                    (match (list-ref points (sub1 num))
                      [(OccupiedPoint 'White _) #t]
                      [_ #f])])
                 (if (bigger-present? (* -1 (distance loc1 loc2)) moves)
                     (match loc1
                       [(PointNum num)
                        (match (list-ref points (sub1 num))
                          [(OccupiedPoint 'White _)
                           (white-bearoff-legal? board loc1)]
                          [_ #f])])
                     #f))]))])]))

(check-expect (point->off? (Game test-board4 'Black '(1 2 3 5)) (PointNum 23)
                           'BlackOff) #f)
(check-expect (point->off? (Game test-board4 'Black '(1 2 3 5)) (PointNum 3)
                           'BlackOff) #f)
(check-expect (point->off? (Game test-board4 'White '(1 2 3 6)) (PointNum 6)
                           'WhiteOff) #t)
(check-expect (point->off? (Game test-board2 'White '(6 5)) (PointNum 4)
                           'WhiteOff) #t)
                


(: legal-move? : Game BoardLoc BoardLoc -> Boolean)
;; This function computes whether a move is legal. This function
;; takes into consideration all the relevant rules and is the foundation
;; for the implementation of many other functions.
;; It takes in a game and two board locations and outputs a Boolean.
(define (legal-move? game loc1 loc2)
  (match game
    [(Game (Board points b-b w-b b-o w-o) turn moves)
     (local
       {(: bearoff-ready? : Game -> Boolean)
        ;; This funtion determines whether, given a game, the player whose
        ;; turn it is is ready to start bearing off checkers.
        ;; It takes in a game and outputs a Boolean.
        (define (bearoff-ready? game)
          (match game
            [(Game board turn moves)
             (local
               {(: w-bearoff-ready? : Board Integer -> Boolean)
                ;; This function determines whether the White player is ready
                ;; to start bearing off checkers.
                ;; The function takes in a board and an index and outputs
                ;; a Boolean.
                ;; n starts at 24
                (define (w-bearoff-ready? board n)
                  (match board
                    [(Board points b-b w-b b-o w-o)
                     (cond
                       [(= n 7)
                        (match (list-ref points 6)
                          [(OccupiedPoint 'White _) #f]
                          [_ #t])]
                       [else
                        (and (match (list-ref points (sub1 n))
                               [(OccupiedPoint 'White _) #f]
                               [_ #t])
                             (w-bearoff-ready? board (sub1 n)))])]))

                (: b-bearoff-ready? : Board Integer -> Boolean)
                ;; This function determines whether the Black player is ready
                ;; to start bearing off checkers.
                ;; The function takes in a board and an index and outputs
                ;; a Boolean.
                ;; n starts at 1
                (define (b-bearoff-ready? board n)
                  (match board
                    [(Board points b-b w-b b-o w-o)
                     (cond
                       [(= n 18)
                        (match (list-ref points 17)
                          [(OccupiedPoint 'Black _) #f]
                          [_ #t])]
                       [else
                        (and (match (list-ref points (sub1 n))
                               [(OccupiedPoint 'Black _) #f]
                               [_ #t])
                             (b-bearoff-ready? board (add1 n)))])]))}
               (match turn
                 ['Black (b-bearoff-ready? board 1)]
                 ['White (w-bearoff-ready? board 24)]))]))}

       (match* (loc1 loc2)
         [((PointNum num1) (PointNum num2))
          (cond
            [(and (symbol=? turn 'Black) (not (= 0 b-b))) #f]
            [(and (symbol=? turn 'White) (not (= 0 w-b))) #f]
            [else (point->point? game loc1 loc2)])]
         [('BlackBar (PointNum num))
          ( bar->point? game loc1 loc2)]
         [('WhiteBar (PointNum num))
          ( bar->point? game loc1 loc2)]
         [((PointNum num) 'BlackOff)
          (cond
            [(and (symbol=? turn 'Black) (not (= 0 b-b))) #f]
            [(and (symbol=? turn 'White) (not (= 0 w-b))) #f]
            [else (if (bearoff-ready? game)
                      (point->off? game loc1 loc2)
                      #f)])]
         [((PointNum num) 'WhiteOff)
          (cond
            [(and (symbol=? turn 'Black) (not (= 0 b-b))) #f]
            [(and (symbol=? turn 'White) (not (= 0 w-b))) #f]
            [else (if (bearoff-ready? game)
                      (point->off? game loc1 loc2)
                      #f)])]))]))


(check-expect (legal-move? (Game test-board4 'Black (list 1 3 4 6))
                           (PointNum 1) (PointNum 5)) #t)
(check-expect (legal-move? (Game test-board4 'White (list 1 3 2 6))
                           (PointNum 1) (PointNum 5)) #f)
(check-expect (legal-move? (Game test-board4 'Black (list 1 3 2 6))
                           (PointNum 1) (PointNum 5)) #f)
(check-expect (legal-move? (Game test-board4 'White (list 1 3 2 6))
                           (PointNum 1) (PointNum 5)) #f)
(check-expect (legal-move? (Game test-board2 'White (list 1 3 2 6))
                           (PointNum 4) (PointNum 5)) #f)
(check-expect (legal-move? (Game test-board2 'Black (list 1 3 2 6))
                           (PointNum 24) 'BlackOff) #f)
(check-expect (legal-move? (Game test-board2 'White (list 1 3 2 6))
                           'WhiteBar (PointNum 1)) #f)
                    
       
(: remove-integer : Integer (Listof Integer) -> (Listof Integer))
;; This function removes a given integer from a list of integers.
;; If the given integer appears twice in the list, only one
;; integer is removed.
;; It consumes an integer and a list of integer and returns a
;; list of integer.
(define (remove-integer n xs)
  (match xs
    ['() '()]
    [(cons f r) (if (= n f) r (cons f (remove-integer n r)))]))

(check-expect (remove-integer 5 '(2 3 4 5)) '(2 3 4))
(check-expect (remove-integer 43 '(2 3 5 6 6)) '(2 3 5 6 6))
(check-expect (remove-integer 5 '()) '())
(check-expect (remove-integer 5 '(5 5 4 3 5)) '(5 4 3 5))
   

(: apply-move : Game BoardLoc BoardLoc -> Game)
;; This function is a modification of the previous-apply-move function.
;; It applies the moves selected by the player and returns errors when
;; the move is invalid.
;; This function takes in a game and two board locations and returns
;; a game.
(define (apply-move game loc1 loc2)
  (local
    {(define d (distance loc1 loc2))}
    (if (legal-move? game loc1 loc2)
        (match game
          [(Game board turn moves)
           (local
             {(define result-board (previous-apply-move board loc1 loc2))}
             (Game result-board turn
                   (match turn
                     ['Black
                      (match loc2
                        ['BlackOff
                         (if (present? d moves)
                             (remove-integer d moves)
                             (remove-integer (foldr max 0 moves) moves))]
                        [_ (remove-integer (distance loc1 loc2) moves)])]
                     ['White
                      (match loc2
                        ['WhiteOff
                         (if (present? d moves)
                             (remove-integer d moves)
                             (remove-integer (foldr max 0 moves) moves))]
                        [_ (remove-integer (* -1 (distance loc1 loc2))
                                           moves)])])))])
        (error "apply-move: invalid move!"))))

(check-expect (apply-move (Game test-board4 'Black '(1 2 4 5))
                          (PointNum 1) (PointNum 2))
              (Game
               (Board
                (list
                 (OccupiedPoint 'Black 1)
                 (OccupiedPoint 'Black 1)
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'White 5)
                 'EmptyPoint
                 (OccupiedPoint 'White 3)
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'Black 5)
                 (OccupiedPoint 'White 5)
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'Black 3)
                 'EmptyPoint
                 (OccupiedPoint 'Black 5)
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'White 2))
                0
                0
                0
                0)
               'Black
               '(2 4 5)))
(check-expect (apply-move (Game test-board2 'White '(1 2 4 5))
                          'WhiteBar (PointNum 24))
              (Game
               (Board
                (list
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'White 3)
                 (OccupiedPoint 'White 1)
                 'EmptyPoint
                 (OccupiedPoint 'Black 4)
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'White 3)
                 (OccupiedPoint 'Black 2)
                 (OccupiedPoint 'Black 4)
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'Black 2)
                 (OccupiedPoint 'White 1)
                 (OccupiedPoint 'Black 4)
                 (OccupiedPoint 'White 1)
                 'EmptyPoint
                 'EmptyPoint
                 'EmptyPoint
                 (OccupiedPoint 'White 1))
                1
                0
                0
                5)
               'White
               '(2 4 5)))
(check-error (apply-move (Game test-board2 'White '(1 2 4 6))
                         'WhiteBar (PointNum 19))
             "apply-move: invalid move!")


(: list-of-int? : (Listof Integer) -> (Listof Integer))
;; This function guarantees that a list of integers is indeed a list
;; of integers. It is written to solve the error in the ormap function.
;; It takes in a list of integer and returns a list of integer.
(define (list-of-int? xs)
  (filter integer? xs))

(check-expect (list-of-int? '(3 5 2 3)) '(3 5 2 3))
(check-expect (list-of-int? '(2 3 4 56 6)) '(2 3 4 56 6))


(: available-moves? : Game -> Boolean)
;; This function determine if the player whose turn it is has any remaining
;; moves they can make, with the available dice rolls.
;; This function consumes a game and returns a Boolean.
(define (available-moves? game)
  (match game
    [(Game (Board points b-b w-b b-o w-o) turn moves)
     (local {(define l (map add1 (list-of-int? (build-list 24 +))))}
       (cond
         [(and (symbol=? turn 'Black) (not (= 0 b-b)))
          (ormap (lambda ([m : Integer])
                   (legal-move? game 'BlackBar (PointNum m))) l)]
         [(and (symbol=? turn 'White) (not( = 0 w-b)))
          (ormap (lambda ([m : Integer])
                   (legal-move? game 'WhiteBar (PointNum m))) l)]
         [else
          (local
            {(: p->p-helper : Integer Game -> Boolean)
             ;; This function checks whether a checker on a particular has
             ;; any legal move if it is to move to any of the 24 points.
             ;; For example, (p->p-helper 2 game) determines whether the
             ;; checker on Point 2 has any legal moves to other Point.
             ;; The function consumes an integer and a game and returns
             ;; a Boolean.
             (define
               (p->p-helper n game)
               (ormap (lambda ([m : Integer])
                        (legal-move? game (PointNum n) (PointNum m)))
                      l))

             (: p->p-helper2 : Game -> Boolean)
             ;; This function checks whether there are any legal moves between
             ;; any two of the 24 points.
             ;; It consumes a game and returns a Boolean.
             (define (p->p-helper2 game)
               (ormap (lambda ([y : Integer]) (p->p-helper y game))
                      l))}
            (or
             (p->p-helper2 game)
             (ormap (lambda ([m : Integer])
                      (legal-move? game (PointNum m) 'BlackOff))
                    l)
             (ormap (lambda ([m : Integer])
                      (legal-move? game (PointNum m) 'WhiteOff))
                    l)))]))]))


(: pair->list : Pair -> (Listof Integer))
;; This function convert a pair into a list of integers.
;; It takes in a Pair and returns a list of integers.
(define (pair->list pair1)
  (match pair1
    [(Pair a b)
     (if (= a b) (list a a a a) (list a b))]))

(check-expect (pair->list (Pair 3 2)) (list 3 2))
(check-expect (pair->list (Pair 1 1)) (list 1 1 1 1))
(check-expect (pair->list (Pair 4 5)) (list 4 5))

   
(: react-to-mouse-crude : World Integer Integer Mouse-Event -> World)
;; This function computes the reaction of the interactive program in response
;; to mouse activities.
;; It consumes a World, the x and y locations of the mouse, mouse event, and
;; outputs a World.
(define
  (react-to-mouse-crude world x y event)
  (match world
    [(World game style 
            highlighted rects p1 p2 history)
     (match game
       [(Game board player moves)
        (match board
          [(Board points b-b w-b b-o w-o)
           (match style
             [(Style rad sp black-checker white-checker dark-point
                     light-point background label black-die white-die)
              (local
                {(define c-loc (click-where style x y))
                 
                 (: manage-turn : Game -> Game)
                 ;; This function conducts turn management. It makes sure that
                 ;; a turn automatically becomes the opponent's when the
                 ;; current player has no more available moves.
                 ;; This function consumes a game and returns a game.
                 (define (manage-turn game)
                   (match game
                     [(Game board turn moves)
                      (cond
                        [(available-moves? game) game]
                        [else (Game board
                                    (match turn
                                      ['White 'Black]
                                      ['Black 'White])
                                    (list -1000))])]))}
                
                (match event
                  ["button-down"
                   (cond
                     [(= 0 highlighted)
                      (match c-loc
                        [(or 'Nowhere 'BlackOff 'WhiteOff) world]
                        ['BlackDice
                         (if (symbol=? player 'Black)
                             (if (available-moves? game)
                                 world
                                 (local
                                   {(define new-pair
                                      (generate-random-nums))}
                                   (World (manage-turn
                                           (Game board 'Black
                                                 (pair->list new-pair)))
                                          style 0 rects
                                          new-pair p2
                                          (cons game history))))
                             world)]
                        ['WhiteDice
                         (if (symbol=? player 'White)
                             (if (available-moves? game)
                                 world
                                 (local
                                   {(define new-pair
                                      (generate-random-nums))}
                                   (World (manage-turn
                                           (Game board 'White
                                                 (pair->list new-pair)))
                                          style 0 rects
                                          p1 new-pair
                                          (cons game history))))
                             world)]
                        ['BlackBar
                         (if (symbol=? player 'Black)
                             (if (>= b-b 1)
                                 (World (Game board player moves) style 25 rects
                                        p1 p2 history)
                                 world) world)]
                        ['WhiteBar
                         (if (symbol=? player 'White)
                             (if (>= w-b 1)
                                 (World (Game board player moves) style 26 rects
                                        p1 p2 history)
                                 world) world)]
                        [(PointNum num)
                         (match (list-ref points (sub1 num))
                           ['EmptyPoint world]
                           [(OccupiedPoint color count)
                            (if (symbol=? color player)
                                (World (Game board player moves) style 
                                       num rects p1 p2 history) world)])])]
                     [(or (= 25 highlighted) (= 26 highlighted))
                      (match c-loc
                        [(or 'Nowhere 'BlackDice 'WhiteDice 'WhiteOff 'BlackOff)
                         world]
                        ['BlackBar (if
                                    (= 25 highlighted)
                                    (World (Game board player moves) style
                                           0 rects p1 p2 history)
                                    world)]
                        ['WhiteBar (if
                                    (= 26 highlighted)
                                    (World (Game board player moves) style
                                           0 rects p1 p2 history)
                                    world)]
                        [(PointNum num)
                         (if (present? -1000 moves)
                             world
                             (if (or (legal-move? game 'BlackBar c-loc)
                                     (legal-move? game 'WhiteBar c-loc))
                                 (World
                                  (manage-turn
                                   (apply-move game (if (= 25 highlighted)
                                                        'BlackBar 'WhiteBar)
                                               c-loc))
                                  style 0 rects p1 p2
                                  (cons game history))
                                 world))])]
                     [(<= 1 highlighted 24)
                      (match c-loc
                        [(or 'Nowhere 'BlackDice
                             'WhiteDice 'BlackBar 'WhiteBar)
                         world]
                        ['WhiteOff
                         (if (present? -1000 moves)
                             world
                             (if (legal-move? game (PointNum highlighted)
                                              'WhiteOff)
                                 (World
                                  (manage-turn
                                   (apply-move game (PointNum highlighted)
                                               'WhiteOff))
                                  style 0 rects p1 p2
                                  (cons game history))
                                 world))]
                        ['BlackOff
                         (if (present? -1000 moves)
                             world
                             (if (legal-move? game (PointNum highlighted)
                                              'BlackOff)
                                 (World
                                  (manage-turn
                                   (apply-move game (PointNum highlighted)
                                               'BlackOff))
                                  style 0 rects p1 p2
                                  (cons game history))
                                 world))]
                        [(PointNum num)
                         (if (= num highlighted)
                             (World (Game board player moves) style 
                                    0 rects p1 p2 history)
                             (if (present? -1000 moves)
                                 world
                                 (if (legal-move? game (PointNum highlighted)
                                                  (PointNum num))
                                     (World
                                      (manage-turn
                                       (apply-move game (PointNum highlighted)
                                                   (PointNum num)))
                                      style 0 rects p1 p2
                                      (cons game history))
                                     world)))])]
                     [else
                      (error "react-to-mouse : cannot react properly.")])]
                  [_ world]))])])])]))


(: react-to-mouse : World Integer Integer Mouse-Event -> World)
;; This is the updated version of the react-to-mouse-crude function.
;; It takes into consideration the requirement that if a game is
;; determined to be over, then clicking anywhere will have no effects.
;; This function takes ina  world, two integers, and a mouse event
;; and returns a world.
(define (react-to-mouse world x y event)
  (match world
    [(World game style 
            highlighted rects p1 p2 history)
     (if (game-over? game)
         world
         (react-to-mouse-crude world x y event))]))


(: initial-world : Style -> World)
;; This function generates the initial world and determines whether it is
;; the black player's turn or the white player's turn.
;; If the number on the dice are the same, then the function is recursively
;; run until the numbers on the dice become different.
;; The function takes in a style and returns a world.
(define (initial-world style)
  (local
    {(define b (is-number (random 1 7)))
     (define w (is-number (random 1 7)))}
    (cond
      [(> b w)
       (World (Game test-board4 'Black (list b w)) style 0 list6
                    (Pair 0 b) (Pair 0 w) '())]
      [(< b w)
       (World (Game test-board4 'White (list b w)) style 0 list6
                    (Pair 0 b) (Pair 0 w) '())]
      [else (initial-world style)])))
;; Since this function has radom number as its component, it is not
;; possible to write check-expects for it.
       

(: game-over? : Game -> Boolean)
;; This function determine if the game is over and is used as a helper
;; function for other functions.
;; It takes in a game and returns a Boolean.
(define (game-over? game)
  (match game
    [(Game (Board points b-b w-b b-o w-o) turn moves)
     (or (= b-o 15) (= w-o 15))]))


(: winner : Game -> Player)
;; This function determines the winner of a completed game.
;; It takes in a game and returns a player.
(define (winner game)
  (if (game-over? game)
      (match game
        [(Game (Board points b-b w-b b-o w-o) turn moves)
         (if (= 15 b-o) 'Black 'White)])
      (error "winner: The game is not yet over!")))
         

(: draw-world : World -> Image)
;; This function produces a visualization of the given world.
;; It consumes a World and outputs an Image.
(define (draw-world world)
  (match world
    [(World game style 
            highlighted rects p1 p2 history)
     (match game
       [(Game board turn moves)
        (match style
          [(Style rad sp black-checker white-checker
                  dark-point light-point background label black-die white-die)
           (overlay
            (cond
              [(game-over? game)
               (overlay
                (above
                 (text "Game Over!" 20 'red)
                 (text (string-append "Winner : "
                                      (symbol->string (winner game)))
                       20 'black))
                (rectangle (* 25 rad) (* 18 rad) 'solid 'yellow))]
              [else empty-image])
            (overlay/xy
             (highlight-image rad sp highlighted)
             (* -1 (* 0.5 rad)) 0
             (draw-board-updated style board p1 p2)))])])]))

;;eye-ball tests:
(draw-world test-world1)
(draw-world test-world2)


;; Note: The five functions below takes in list of games as
;; arguments. The complexity of the game struct makes it extremly
;; difficult to write any meaningful check-expects for these
;; functions.

(: first-time : (Listof Game) Integer -> Integer)
;; Given a list of games and an index, this function returns the index
;; of the position where -1000 appears for the first time in the moves.
;; It takes in a list of game and an integer and returns an integer.
;; The index is always 0 for our purposes.
(define (first-time games i)
  (if (present? -1000 (Game-moves (list-ref games i)))
      i
      (first-time games (add1 i))))


(: find-index : (Listof Game) Integer -> Integer)
;; Given a list of games and an index, this function returns the index
;; of the position where -1000 appears for the second time in the moves.
;; It takes in a list of game and an integer and returns an integer.
;; The index is always 1 for our purposes.
(define (find-index games index)
  (local
    {(define m (first-time games 0))}
    (if (present? -1000 (Game-moves (list-ref games (+ index m))))
        (+ index m)
        (find-index games (add1 index)))))


(: find-moves2 : (Listof Game) -> (Listof Integer))
;; Given a list of games, this function returns the moves which
;; can be used to recover the numbers on the dice.
;; It takes in a list of games and returns a list of integers.
(define (find-moves2 games)
  (local
    {(define y (find-index (rest games) 1))}
    (Game-moves (list-ref games y))))


(: find-moves1 : (Listof Game) -> (Listof Integer))
;; Given a list of games, this function returns the moves which
;; can be used to recover the numbers on the dice.
;; It takes in a list of games and returns a list of integers.
(define (find-moves1 games)
  (local
    {(define y (first-time (rest games) 1))}
    (Game-moves (list-ref games y))))


(: num-negative-moves : (Listof Game) -> Integer)
;; This function computes how many times the number -1000
;; appears in the moves in the given list of games.
;; It consumes a list of games and returns an integer.
(define (num-negative-moves games)
  (match games
    ['() 0]
    [(cons (Game board turn moves) rest)
     (if (present? -1000 moves)
         (+ 1 (num-negative-moves rest))
         (num-negative-moves rest))]))
     

(: react-to-key : World String -> World)
;; This function is the on-key function in the universe.
;; If "u" is pressed, the previous move or dice roll can be undone.
;; If "s" is pressed, the game is saved.
;; If "l" is pressed, the saved game can be loaded.
;; The function takes in a world and a string and returns a world.
(define (react-to-key world str)
  (match world
    [(World game style 
            highlighted rects p1 p2 history)
     (match game
       [(Game board turn moves)
        (match str
          ["u"
           (if (or (empty? history) (not (= 0 highlighted)))
               world
               (local
                 {(define turn1 (Game-turn (first history)))
                  (define moves1 (Game-moves (first history)))
                  (define 1-b (first (Game-moves (last history))))
                  (define 1-w (second (Game-moves (last history))))}
                 (cond
                   [(symbol=? turn turn1)
                    (cond
                      [(and (present? -1000 moves1)
                            (or
                             (= 0 (num-negative-moves (rest history)))
                             (= 1 (num-negative-moves (rest history)))))
                       (World (first history) style highlighted rects
                              (match turn1
                                ['Black
                                 (Pair 0 1-b)]
                                ['White p1])
                              (match turn1
                                ['Black p2]
                                ['White
                                 (Pair 0 1-w)])
                              (rest history))]
                      [(present? -1000 moves1)
                       (World (first history) style 
                              highlighted rects
                              (match turn1
                                ['Black (Pair
                                         (list-ref (find-moves2
                                                    history)
                                                   0)
                                         (list-ref (find-moves2
                                                    history)
                                                   1))]
                                ['White p1])
                              (match turn1
                                ['Black p2]
                                ['White (Pair
                                         (list-ref (find-moves2
                                                    history)
                                                   0)
                                         (list-ref (find-moves2
                                                    history)
                                                   1))])
                              (rest history))]
                      [else (World (first history) style 
                                   highlighted rects p1 p2
                                   (rest history))])]
                   [else (World (first history) style 
                                highlighted rects p1 p2
                                (rest history))])))]
          ["s" (begin (save-game! world) world)]
          ["l" (load-game style)]
          [_ world])])]))


(: run : Style -> World)
;; This function starts the interactive program.
;; It takes in a style and outputs a World
(define (run style)
  (big-bang (initial-world style) : World
    [to-draw draw-world]
    [on-mouse react-to-mouse]
    [on-key react-to-key]))


(: string->integer : String -> Integer)
;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

(check-expect (string->integer "3") 3)
(check-error (string->integer "B") "string->integer: invalid integer")

    
(: point->string : Point -> String)
;; This function converts a point into a string.
;; It consumes a point and returns a string.
(define (point->string pt)
  (match pt
    ['EmptyPoint "_"]
    [(OccupiedPoint player count)
     (if (symbol=? 'Black player)
         (string-append "B" (number->string count))
         (string-append "W" (number->string count)))]))

(check-expect (point->string 'EmptyPoint) "_")
(check-expect (point->string (OccupiedPoint 'White 3)) "W3")
(check-expect (point->string (OccupiedPoint 'Black 7)) "B7")


(: string->point : String -> Point)
;; This function converts a string into a point.
;; It consumes a string and returns a point.
(define (string->point str)
  (match str
    ["_" 'EmptyPoint]
    [_
     (local
       {(define color (match (first (string->list str))
                        [#\B 'Black]
                        [#\W 'White]
                        [_ (error "string->point: current player not valid!")]))
        (define count
          (string->integer (substring str 1)))}
       (if (<= count 0)
           (error
            "string->point: count of an OccupiedPoint cannot be nonpositive!")
           (OccupiedPoint color count)))]))

(check-expect (string->point "_") 'EmptyPoint)
(check-expect (string->point "B4") (OccupiedPoint 'Black 4))
(check-expect (string->point "W3") (OccupiedPoint 'White 3))
(check-error (string->point "E3") "string->point: current player not valid!")
(check-error
 (string->point "B-4")
 "string->point: count of an OccupiedPoint cannot be nonpositive!")
 

(: points->string : (Listof Point) -> String)
;; This function converts a list of points into a string.
;; It consumes a list of points and returns a string.
(define (points->string pts)
  (match pts
    ['() ""]
    [(cons f '())
     (point->string f)]
    [(cons f r)
     (string-append (point->string f) " " (points->string r))]))

(check-expect (points->string (list 'EmptyPoint 'EmptyPoint))
              "_ _")
(check-expect (points->string (list (OccupiedPoint 'White 3)
                                    (OccupiedPoint 'Black 4) 'EmptyPoint))
              "W3 B4 _")
(check-expect (points->string (list (OccupiedPoint 'Black 4)
                                    (OccupiedPoint 'Black 5) 'EmptyPoint))
              "B4 B5 _")


(: string->points : String -> (Listof Point))
;; This function converts a string into a list of points.
;; It consumes a string and returns a list of points.
(define (string->points str)
  (match (string-split str " ")
    [x (map string->point x)]))

(check-expect
 (string->points "_ _") (list 'EmptyPoint 'EmptyPoint))
(check-expect
 (string->points "W3 B4 _") (list (OccupiedPoint 'White 3)
                                  (OccupiedPoint 'Black 4) 'EmptyPoint))
(check-expect
 (string->points "B4 B5 _") (list (OccupiedPoint 'Black 4)
                                  (OccupiedPoint 'Black 5) 'EmptyPoint))
(check-expect
 (string->points "B4 B5 _") (list (OccupiedPoint 'Black 4)
                                  (OccupiedPoint 'Black 5) 'EmptyPoint))


(: board->string : Board -> String)
;; This function converts a board into a string.
;; It consumes a board and returns a string.
(define (board->string board)
  (match board
    [(Board points black-bar white-bar black-off white-off)
     (string-append (points->string points) "|"
                    (number->string black-bar) "|"
                    (number->string white-bar) "|"
                    (number->string black-off) "|"
                    (number->string white-off))]))

(check-expect
 (board->string test-board1)
 "W2 _ _ _ _ B5 _ B3 _ _ _ W5 B5 _ _ _ W3 _ W5 _ _ _ _ B2|0|0|0|0")
(check-expect
 (board->string test-board2)
 "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|0|5")


(: string->board : String -> Board)
;; This function converts a string into a board.
;; It consumes a string and returns a board.
(define (string->board str)
  (match (string-split str "|")
    [l
     (if (not (= 5 (length l)))
         (error "string->board: board not format properly!")
         (if (= 24 (length (string-split (first l) " ")))
             (Board (string->points (first l))
                    (if (>= (string->integer(second l)) 0)
                        (string->integer(second l))
                        (error
                         (string-append "string->board: "
                                        "The number of points on the bar"
                                        " cannot be negative")))
                    (if (>= (string->integer(third l)) 0)
                        (string->integer(third l))
                        (error
                         (string-append "string->board: "
                                        "The number of points on the bar "
                                        "cannot be negative")))
                    (if (>= (string->integer (fourth l)) 0)
                        (string->integer (fourth l))
                        (error
                         (string-append "string->integer: "
                                        "The number of points in the bear-off "
                                        "area cannot be negative")))
                    (if (>= (string->integer (fifth l)) 0)
                        (string->integer (fifth l))
                        (error
                         (string-append "string->integer: "
                                        "The number of points in the bear-off "
                                        "area cannot be negative"))))
             (error "string->board: The number of points is not exactly 24!")
             ))]))

(check-expect
 (string->board
  "W2 _ _ _ _ B5 _ B3 _ _ _ W5 B5 _ _ _ W3 _ W5 _ _ _ _ B2|0|0|0|0")
 test-board1)
(check-expect
 (string->board
  "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|0|5")
 test-board2)
(check-error
 (string->board
  "_ _ W3 W1 _ B4 _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|0|5")
 "string->board: The number of points is not exactly 24!")
(check-error
 (string->board
  "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|-5|1|0|5")
 "string->board: The number of points on the bar cannot be negative")
(check-error
 (string->board
  "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|-4|5")
 (string-append "string->integer: The number of points in the bear-off " 
"area cannot be negative"))
(check-error
 (string->board
  "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|-4|5|3")
  "string->board: board not format properly!")


(: game->string : Game -> String)
;; This function converts a game into a string.
;; It takes in a game and returns a string.
(define (game->string game)
  (local
    {(: moves->string : (Listof Integer) -> String)
     ;; This function converts moves into a string.
     ;; It consumes a list of integers and returns a string.
     (define (moves->string m)
       (match m
         ['() ""]
         [(cons f '()) (number->string f)]
         [(cons f r)
          (string-append (number->string f) " " (moves->string r))]))}
    (match game
      [(Game board turn moves)
       (string-append (board->string board)
                      "@"
                      (match turn
                        ['Black "B"]
                        ['White "W"])
                      "@"
                      (moves->string moves))])))

(check-expect
 (game->string (Game test-board1 'Black '(2 3)))
 "W2 _ _ _ _ B5 _ B3 _ _ _ W5 B5 _ _ _ W3 _ W5 _ _ _ _ B2|0|0|0|0@B@2 3")
(check-expect
 (game->string (Game test-board2 'Black '()))
 "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|0|5@B@")


(: string->game : String -> Game)
;; This function converts a string into a game.
;; It takes in a string and returns a game.
(define (string->game str)
  (local
    {(: string->moves : (Listof String) -> (Listof Integer))
     ;; This function converts a string into moves.
     ;; It takes in a list of string and returns a list of integers.
     (define (string->moves s)
       (if (> (length s) 4)
           (error "string->moves: More than 4 moves in moves!")
           (match s
             ['() '()]
             [(cons f r)
              (cons (string->integer f) (string->moves r))])))
     (define l (string-split str "@"))
     (define len (length l))}
    (cond
      [(= 2 len)
       (Game (string->board (first l))
             (match (second l)
               ["B" 'Black]
               ["W" 'White]
               [_ (error "string->game: Current turn not formatted properly")])
             '())]
      [(= 3 len)
       (Game (string->board (first l))
             (match (second l)
               ["B" 'Black]
               ["W" 'White]
               [_ (error "string->game: Current turn not formatted properly")])
             (string->moves (string-split (third l) " ")))]
      [else (error "string->game: Game not formatted properly!")])))

(check-expect
 (string->game
  "W2 _ _ _ _ B5 _ B3 _ _ _ W5 B5 _ _ _ W3 _ W5 _ _ _ _ B2|0|0|0|0@B@2 3")
 (Game test-board1 'Black '(2 3)))
(check-expect
 (string->game
  "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|0|5@B@")
 (Game test-board2 'Black '()))
(check-error
 (string->game
  "_ _ W3 W1 _ B4 _ _ W3 B2 B4 _ _ _ _ _ B2 W1 B4 W1 _ _ _ B1|0|1|0|5@T@")
 "string->game: Current turn not formatted properly")
(check-error
 (string->game
  "W2 _ _ _ _ B5 _ B3 _ _ _ W5 B5 _ _ _ W3 _ W5 _ _ _ _ B2|0|0|0|0@B@2 3 4 5 5")
 "string->moves: More than 4 moves in moves!")
(check-error
 (string->game
  "W2 _ _ _ _ B5 _ B3 _ _ _ W5 B5 _ _ _ W3 _ W5 _ _ _ _ B2|0|0|0|0@B@2 3 4 5@3")
 "string->game: Game not formatted properly!")
 
             
(: history->string : (Listof Game) -> String)
;; This function converts history into string.
;; It consumes a list of games and returns a string.
(define (history->string games)
  (match games
    ['() ""]
    [(cons f '()) (game->string f)]
    [(cons f r)
     (string-append (game->string f) "!"
                    (history->string r))]))

(check-expect (history->string (list
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 2)
                                   (OccupiedPoint 'Black 1)
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  1
                                  0
                                  0
                                  0)
                                 'White
                                 '(3))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 2)
                                   (OccupiedPoint 'Black 1)
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '(3 4))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 2)
                                   (OccupiedPoint 'Black 1)
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '(-1000))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'Black
                                 '(1))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'Black
                                 '(3 1))))
              (string-append "B1 _ _ W1 _ W5 _ W2 _ _ _ B5 W5 _ _ _ B2 B1 "
                             "B5 _ _ _ _ W2|1|0|0|0@W@3!B1 _ _ B1 _ W5 _ W3"
                             " _ _ _ B5 W5 _ _ _ B2 B1 B5 _ _ _ _ W2|0|0|0|0@"
                             "W@3 4!B1 _ _ B1 _ W5 _ W3 _ _ _ B5 W5 _ _ _ B2 "
                             "B1 B5 _ _ _ _ W2|0|0|0|0@W@-1000!B1 _ _ B1 _ W5 "
                             "_ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0"
                             "@B@1!B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ "
                             "B5 _ _ _ _ W2|0|0|0|0@B@3 1"))


(: string->history : String -> (Listof Game))
;; This function converts string into history.
;; It takes in a string and returns a list of game.
(define (string->history str)
  (match (string-split str "!")
    [l (map string->game l)]))

(check-expect
 (string->history
  (string-append "B1 _ _ W1 _ W5 _ W2 _ _ _ B5 W5 _ _ _ B2 B1 "
                 "B5 _ _ _ _ W2|1|0|0|0@W@3!B1 _ _ B1 _ W5 _ W3"
                 " _ _ _ B5 W5 _ _ _ B2 B1 B5 _ _ _ _ W2|0|0|0|0@"
                 "W@3 4!B1 _ _ B1 _ W5 _ W3 _ _ _ B5 W5 _ _ _ B2 "
                 "B1 B5 _ _ _ _ W2|0|0|0|0@W@-1000!B1 _ _ B1 _ W5 "
                 "_ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0"
                 "@B@1!B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ "
                 "B5 _ _ _ _ W2|0|0|0|0@B@3 1"))
 (list
  (Game
   (Board
    (list
     (OccupiedPoint 'Black 1)
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'White 1)
     'EmptyPoint
     (OccupiedPoint 'White 5)
     'EmptyPoint
     (OccupiedPoint 'White 2)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 5)
     (OccupiedPoint 'White 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 2)
     (OccupiedPoint 'Black 1)
     (OccupiedPoint 'Black 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'White 2))
    1
    0
    0
    0)
   'White
   '(3))
  (Game
   (Board
    (list
     (OccupiedPoint 'Black 1)
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 1)
     'EmptyPoint
     (OccupiedPoint 'White 5)
     'EmptyPoint
     (OccupiedPoint 'White 3)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 5)
     (OccupiedPoint 'White 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 2)
     (OccupiedPoint 'Black 1)
     (OccupiedPoint 'Black 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'White 2))
    0
    0
    0
    0)
   'White
   '(3 4))
  (Game
   (Board
    (list
     (OccupiedPoint 'Black 1)
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 1)
     'EmptyPoint
     (OccupiedPoint 'White 5)
     'EmptyPoint
     (OccupiedPoint 'White 3)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 5)
     (OccupiedPoint 'White 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 2)
     (OccupiedPoint 'Black 1)
     (OccupiedPoint 'Black 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'White 2))
    0
    0
    0
    0)
   'White
   '(-1000))
  (Game
   (Board
    (list
     (OccupiedPoint 'Black 1)
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 1)
     'EmptyPoint
     (OccupiedPoint 'White 5)
     'EmptyPoint
     (OccupiedPoint 'White 3)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 5)
     (OccupiedPoint 'White 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 3)
     'EmptyPoint
     (OccupiedPoint 'Black 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'White 2))
    0
    0
    0
    0)
   'Black
   '(1))
  (Game
   (Board
    (list
     (OccupiedPoint 'Black 2)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'White 5)
     'EmptyPoint
     (OccupiedPoint 'White 3)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 5)
     (OccupiedPoint 'White 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'Black 3)
     'EmptyPoint
     (OccupiedPoint 'Black 5)
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     'EmptyPoint
     (OccupiedPoint 'White 2))
    0
    0
    0
    0)
   'Black
   '(3 1))))
 
                    
(: save-game! : World -> Void)
;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
      (local
        {(: file : Output-Port)
         (define file (open-output-file path))}
           (begin
             (write-string (world->string w) file)
             (close-output-port file)))
      (void))))


(: world->string : World -> String)
;; This function converts a world into a string.
;; It takes in a world and returns a string.
(define (world->string world)
  (string-append (game->string (World-game world))
                 "!"
                 (history->string (World-history world))))


(: load-game : Style -> World)
;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided Style to make a new World
;; raise an error if the user cancels or if something goes wrong
(define (load-game s)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
      (string->world s (port->string (open-input-file path)))
      (error "load-game: user cancelled"))))


(: string->world : Style String -> World)
;; This function converts a world into a string, recovering
;; everything in the previous world.
;; It takes in a string and a style and returns a world.
(define (string->world style str)
  (local
    {(define l (map string->game (string-split str "!")))
     (define curr-game (first l))
     (define history (rest l))
     (define 1-b (first (Game-moves (last history))))
     (define 1-w (second (Game-moves (last history))))
     (define 1-player (Game-turn (last l)))}
    (match curr-game
      [(Game board turn moves)
       (cond 
         [(= 0 (num-negative-moves history))
          (World curr-game style 0 list6
                 (Pair 0 1-b)
                 (Pair 0 1-w)
                 history)]
         [(= 1 (num-negative-moves history))
          (local
            {(define moves-helper (Game-moves
                                   (list-ref l (first-time history 1))))}
            (if (symbol=? 1-player 'Black)
                (World curr-game style 0 list6
                       (Pair 0 1-b)
                       (Pair
                        (list-ref moves-helper 0)
                        (list-ref moves-helper 1))
                       history)
                (World curr-game style 0 list6
                       (Pair
                        (list-ref moves-helper 0)
                        (list-ref moves-helper 1))
                       (Pair 0 1-w)
                       history)))]
         [(present? -1000 moves)
          (local
            {(define y (first-time history 0))
             (define find-moves1-updated (Game-moves (list-ref l y)))
             (define z (find-index history 1))
             (define find-moves2-updated
               (Game-moves (list-ref l z)))}
            (if (symbol=? turn 'Black)
                (World curr-game style 0 list6
                       (Pair
                        (list-ref find-moves2-updated 0)
                        (list-ref find-moves2-updated 1))
                       (Pair
                        (list-ref find-moves1-updated 0)
                        (list-ref find-moves1-updated 1))
                       history)
                (World curr-game style 0 list6
                       (Pair
                        (list-ref find-moves1-updated 0)
                        (list-ref find-moves1-updated 1))
                       (Pair
                        (list-ref find-moves2-updated 0)
                        (list-ref find-moves2-updated 1))
                       history)))]
         [else
          (local
            {(define y (first-time history 0))
             (define find-moves1-updated (Game-moves (list-ref l y)))
             (define z (find-index history 1))
             (define find-moves2-updated
               (Game-moves (list-ref l z)))}
            (if (symbol=? turn 'Black)
                (World curr-game style 0 list6
                       (Pair
                        (list-ref find-moves1-updated 0)
                        (list-ref find-moves1-updated 1))
                       (Pair
                        (list-ref find-moves2-updated 0)
                        (list-ref find-moves2-updated 1))
                       history)
                (World curr-game style 0 list6
                       (Pair
                        (list-ref find-moves2-updated 0)
                        (list-ref find-moves2-updated 1))
                       (Pair
                        (list-ref find-moves1-updated 0)
                        (list-ref find-moves1-updated 1))
                       history)))])])))
                            
 
(test)
         