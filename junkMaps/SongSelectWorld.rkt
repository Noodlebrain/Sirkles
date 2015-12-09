;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname SongSelectWorld) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Region contains two sets of coordinates where
;x1, y1, x2, y2
;x1 <= x2 && y1 <= y2
(define-struct region (x1 y1 x2 y2))
;Could also use posns I guess...

;Button contains
;A region
;A world to return
(define-struct button (region world))

;A listOfButtons is
;'(), or
;(cons button listOfButtons)

;buttonsInitializer
;Takes a filepath
;Returns a listOfButtons where:
;First button is the back button
;Rest of the buttons go to a playField as their world
(define (buttonsInitializer filepath)
  (cons
   (make-button (make-region 0 (* (- 1 .175) height) (* .35 width) height)
                (make-menuWorld ...));Creating the back button
   
   )
  )

;World contains
;Signature saying type of world DON'T NEED - CHECK STRUCTURE TYPE INSTEAD
;mouseX and mouseY in a posn
;list of buttons (1 for back, 4 for beatmaps for now)
(define-struct selectWorld (posn listOfButtons))

;MouseHandler
;Movement -> world with updated mouse position
;Click -> check inButton?
(define (mouseHandler w mx my event)
  (cond
    [() (make-selectWorld (make-posn mx my) (selectWorld-listOfButtons w))];event is movement, return world with updated mouseX and mouseY
    [else (inButton? (make-posn mx my) (selectworld-listOfButtons w))];event is click, check inButton?
    )
  )

;inButton?
;Takes a posn and a listOfButtons
;Returns a world from a button if a posn is in a button
;Else returns a selectWorld from the posn and listOfButtons
(define (inButton? posn listOfButtons)
  (cond
    [];Empty, return selectWorld
    [];posn is in the region, return button's world
    [];posn not in the region, recursive call on -> (rest list)
    )
  )

;DrawHandler
;Takes a listOfButtons
;Draws the back button
;Draws beatmap buttons
(define (drawHandler listOfButtons)
  (place-images
   (append (beatmapNames listOfButtons) (buttonRectangles listOfButtons))
   (buttonPosns)
   )
  )

;buttonRectangles
;Takes a listOfButtons
;Returns a list of rectangles depending on
;the regions of the buttons
(define (buttonRectangles listOfButtons)
  
  )

;buttonPosns
;Takes a listOfButtons
;Returns a list of posns based on
;the regions of the buttons
(define (buttonPosns listOfButtons)
  
  )

;BEATMAP READER - > METADATA SECTION NEEDED
;beatmapNames
;Takes a filepath
;Returns a list of strings with the beatmap names
(define (beatmapNames filepath)
  
 )









