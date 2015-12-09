;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ResultsScreen) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (mouseHandler4 w mx my event)
    (cond
    [(and (inRegion? (make-posn mx my) (button-region backButton));Cursor in button?
          (string=? event "button-up");Is click?
     (button-world backButton))];Go back to starting screen
    [else w];Nope not a click
    )
  )

(define (keyHandler4 w k)
  w;im a doge
  )

(define (drawHandler4 w)
    (place-images
   (list (text "-Result-" 50 "white")
         (text (string-append "Your score was "
                              (number->string (world-score w))) 30 "white")
         cursor
         (rectangle (* .3 cvLength) (* .175 cvHeight) "solid" "white"))
   (list (make-posn (/ cvLength 2) (/ cvHeight 2))
         (make-posn (/ cvLength 2) (+ 100 (/ cvHeight 2)))
         (world-posn w)
         (region->posn (button-region backButton)))
   SCENE))