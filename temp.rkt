;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname temp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Song select is working except for:
;Sometimes calls the wrong handlers?
;Weird stuff happens the second time around
;Buttons don’t appear in the proper place
(require rsound)
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require racket/string)

;Framerate
(define fr 44100)

;Takes milliseconds
;returns them as frames
(define (ms->frames ms)
  (* (* ms .001) fr)
  )

(define (both a b)
  b)

;Seconds conversion
(define (s n)
(* n fr))

;Canvas x and y
(define cvLength 1024)
(define cvHeight 768)

;Ratio for Osu beatmaps to our beatmaps
(define xRatio 1.75)
(define yRatio 1.75)

;Define the Canvas
(define SCENE (empty-scene cvLength cvHeight "black"))

;timingLenience (milliseconds -> frames)
(define timingLenience 5512.5)
;1653.75

;Score is a number
(define initialScore 0)

;The amount of score added per circle hit
(define scoreInc 150)

;Circle radius
(define cRad 75)

;The key being tapped to hit circles
(define key1 "z")
(define key2 "x")

;Hitsound
(define hitSound (rs-scale .5 (rs-read "effectSounds/hitSound.wav")))
(define missSound (rs-scale .5 (rs-read "effectSounds/comboBreak.wav")))

;Song
(define song1 (rs-read "beatmaps/map1.wav"))
(define song2 (rs-read "beatmaps/map2.wav"))
(define song3 (rs-read "beatmaps/map3.wav"))
(define song4 (rs-read "beatmaps/map4.wav"))

;Cursor
(define cursor (bitmap/file "graphics/cursor.png"))
                       
;Cursor x and y coords
(define (cursorX w)
  (- (posn-x (world-posn w)) (/ (image-width cursor) 2)))
(define (cursorY w)
  (- (posn-y (world-posn w)) (/ (image-height cursor) 2)))

;scaleCircle ;You guys are going to kill me for putting this up here
;Takes a hitcircle image graphic ;the radius is half the width
;scales it to the circle radius cRad
(define (scaleCircle img)
  (scale
   (/ cRad (/ (image-width img) 2))
   img)
)

;Rate of approach; the parameter is in ms
(define approachRate (s (* .001 450)))

;Hitcircle graphic
(define hitcircle (scaleCircle (bitmap/file "graphics/hitcircle.png")))

;Pen for approach circle
(define pen1 (make-pen (color 75 75 210 204) 5 "solid" "round" "round"))

;Background
(define background (bitmap/file "graphics/background.png"))

;img x and y coords
(define (imgX img)
  (/ (image-width img) 2))
(define (imgY img)
  (/ (image-height img) 2))

;Region contains two sets of coordinates where
;x1, y1, x2, y2
;x1 <= x2 && y1 <= y2
(define-struct region (x1 y1 x2 y2))
;Could also use posns I guess...

;Region->Posn
(define (region->posn r)
  (make-posn (/ (+ (region-x2 r) (region-x1 r)) 2) (/ (+ (region-y2 r) (region-y1 r)) 2))
  )

;inRegion?
;Takes a posn and a region -> boolean
(define (inRegion? p r)
  (cond
    [(and
      (and (<= (posn-x p) (region-x2 r)) (>= (posn-x p) (region-x1 r)))
      (and (<= (posn-y p) (region-y2 r)) (>= (posn-y p) (region-y1 r))))
     true];In region, true
    [else false];Not in region, false
    )
  )

;Button contains
;A region
;A world to return
;A rsound (a song)
(define-struct button (region world song))

;A listOfButtons is
;'(), or
;(cons button listOfButtons)

;Notes are structures with Time and Location
;Time is a number in frames
;Location is a posn
(define-struct note [time location])

;Beatmaps are lists of note structures
;(define beatmap (list note note note ... '()))
(define untrimmedBeatmap (read-words/line "beatmaps/map1.txt"));Import beatmap text

;Takes a list of lists of strings
;Returns a new list of lists of strings that has everything after "[HitObjects]"
;Should never reach the end of a beatmap and see an empty there
(define (trimMap map)
  (cond
    [(empty? (first map)) (trimMap (rest map))];Take out any empties that come our way
    [(string=? (first (first map)) "[HitObjects]") (rest map)];If it's [HitObjects], we r home free and return the rest of the list. no more trim
    [else (trimMap (rest map))];If it's not [HitObjects], return the rest of the list into the function and keep trimming
    )
)

(define trimmedBeatmap (trimMap untrimmedBeatmap));Trim the beatmap

(define splicedBeatmap (map (lambda (str) (string-split (first str) #px",")) trimmedBeatmap)) ;Splice into sections by commas

;Takes a list of 3 numbers
;Puts them into a note structure
(define (noteMapper list)
  (make-note (ms->frames (string->number (third list))) (make-posn (string->number (first list)) (string->number (second list))))
  )

;Takes a note and creates a new note after changing the location based on the ratios
;note->note
(define (changeRatio n)
  (make-note (note-time n) (make-posn (* xRatio (posn-x (note-location n))) (* yRatio (posn-y (note-location n))))))

;Takes a filepath
;Returns a beatmap
(define (makeMap filepath)
  (map changeRatio
       (map noteMapper
            (map (lambda (str) (string-split (first str) #px","))
                 (trimMap
                     (read-words/line filepath))))))


;Creating the beatmap for the world
(define currentBeatmap (addOffset (makeMap "beatmaps/map1.txt") 1279));Map to a list of notes

;Initializing beatmaps
(define beatmap1 (makeMap "beatmaps/map1.txt"))
(define beatmap2 (makeMap "beatmaps/map2.txt"))
(define beatmap3 (makeMap "beatmaps/map3.txt"))
(define beatmap4 (makeMap "beatmaps/map4.txt"))

;World holds:
;state (menu or game)
;(Current) MouseX, MouseY
;Total score
;Dynamic beatmap, altered by on-tick
(define-struct world (state posn score beatmap))
(define w1 (make-world 0 (make-posn 0 0) 0 '() )) ;initialize world

;Initialize buttons
(define backButton ;backButton
  (make-button (make-region 0 (* .825 cvHeight) (* .3 cvLength) cvHeight)
                 (make-world 0 (make-posn 0 0) 0 '() )
                 ding))

(define button1
  (make-button (make-region (* .8 cvLength) 0 cvLength (* .25 cvHeight))
                 (make-world 3 (make-posn 1 0) 0 beatmap1)
                 song1))

(define button2
  (make-button (make-region (* .8 cvLength) (* .25 cvHeight) cvLength (* .5 cvHeight))
                 (make-world 3 (make-posn 2 0) 0 beatmap2)
                 song2))

(define button3
  (make-button (make-region (* .8 cvLength) (* .5 cvHeight) cvLength (* .75 cvHeight))
                 (make-world 3 (make-posn 3 0) 0 beatmap3)
                 song3))

(define button4
  (make-button (make-region (* .8 cvLength) (* .75 cvHeight) cvLength cvHeight)
                 (make-world 3 (make-posn 4 0) 0 beatmap4)
                 song4))

(define buttons (list backButton button1 button2 button3 button4));;THE list of buttons

;Takes a wordl, a list of buttons and a posn
;Checks to see if a button is pressed
;If true for a button, return the appropriate button
(define (whichButton w buttonList posn)
  (cond
    [(empty? buttonList) w];None of them were clicked..
    [(inRegion? posn (button-region (first buttonList)))
             (first buttonList)];Clicked!
    [else (whichButton w (rest buttonList) posn)];Check the next one
    )
  )

;Takes a world, a list of buttons and a posn
;Checks to see if a button is pressed
;If true for a button, return true, else false
(define (withinButton w buttonList posn)
  (cond
    [(empty? buttonList) false];None of them were clicked..
    [(inRegion? posn (button-region (first buttonList)))
             true];Clicked!
    [else (withinButton w (rest buttonList) posn)];Check the next one
    )
  )
; DRAWHANDLER
(define (drawHandler w)
  (cond
    [(= (world-state w) 0) (drawhandler0 w)]
    [(= (world-state w) 1) (drawhandler1 w)]
    [(= (world-state w) 2) (drawhandler2 w)]
    [(= (world-state w) 3) (drawhandler3 w)]
    [(= (world-state w) 4) (drawhandler4 w)]
    ))

(define (drawhandler0 w)
  (place-images
     (list (text "SiRkLeS" 100 "white") (text "Press Z or X to start" 60 "white"))
     (list (make-posn (/ cvLength 2) (/ cvHeight 2)) (make-posn (/ cvLength 2) (+ 150 (/ cvHeight 2))))
     SCENE))

(define (drawhandler2 w)
  (place-images
   (list cursor
         (rectangle (* .3 cvLength) (* .175 cvHeight) "solid" "white")
         (rectangle (* .2 cvLength) (* .25 cvHeight) "solid" "blue")
         (rectangle (* .2 cvLength) (* .25 cvHeight) "solid" "red")
         (rectangle (* .2 cvLength) (* .25 cvHeight) "solid" "green")
         (rectangle (* .2 cvLength) (* .25 cvHeight) "solid" "yellow"))
   (list (world-posn w)
         (region->posn (button-region backButton))
         (region->posn (button-region button1))
         (region->posn (button-region button2))
         (region->posn (button-region button3))
         (region->posn (button-region button4)))
   SCENE))

(define (drawhandler1 w)
  (place-images
     (list (text "Instructions:" 70 "white")
           (text "Press z or x when your mouse is over" 40 "white")
           (text "a circle to gain points. You must " 40 "white")
           (text "press the key at the appropriate time" 40 "white")
           (text "to gain points. You can also click the" 40 "white")
           (text "circles to get points as well. Good Luck" 40 "white")
           (text "Press Z or X to start" 55 "white"))
     (list (make-posn (/ cvLength 2) 100)
           (make-posn (/ cvLength 2) 180)
           (make-posn (/ cvLength 2) 240)
           (make-posn (/ cvLength 2) 300)
           (make-posn (/ cvLength 2) 360)
           (make-posn (/ cvLength 2) 420)
           (make-posn (/ cvLength 2) 490))
     SCENE))

(define (drawhandler3 w)
  (place-images
   (list cursor (text (number->string (world-score w)) 20 "white"))
   (list (world-posn w) (make-posn (- cvLength 25) 25))
   (overlay (render (world-beatmap w)) background)))    

(define (drawhandler4 w)
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

;SEARCHER
;Check values of list starting from index 0
;Takes a list and a starting index
;If value is “close enough”
    ;Add to new list
;else don’t add to new list
        ;efficiency: If last item was added, and the next item is not to be, end function
(define (searcher beatmap)
  (cond
        [(empty? beatmap) '()];Ayyyy fixed the crashes. Still stops at the end, but not a crash
        [(not (< (- (note-time (first beatmap)) (pstream-current-frame ps)) approachRate)) '()]
        [else (cons (first beatmap) (searcher (rest beatmap)))]))

;noOfCircles
; Number -> List
; takes length of a list, and makes a number of identical circles equal to the length of the list
(define (noOfCircles n)
  (cond [(= 0 n) '()]
        [else (cons hitcircle (noOfCircles (sub1 n)))]))

;approachCircles
;List -> List
;takes list of notes and creates a list approach circles for notes
(define (approachCircles beatmap)
  (cond
    [(empty? beatmap) '()]
    [else (cons (circle
                 (abs (+ cRad (* cRad (/ (+ (- (note-time (first beatmap)) (pstream-current-frame ps))) approachRate))))
                 "outline" pen1) (approachCircles (rest beatmap)))]))


;RENDER HELPER
;List -> image  
;Takes a list of things to render  
;Renders them on a scene
; renders note circle and approach ring: radius of ring decreases based on time before note
; should be hit
(define (render beatmap)
  (local [(define x (searcher beatmap)) (define y (map note-location x))]
    (place-images (append (noOfCircles (length x)) (approachCircles x))
                (append y y)
                background)))

;MOUSE HANDLER ; General
(define (mouseHandler w mx my event)
  (cond
    [(= (world-state w) 0) (mouseHandler0 w mx my event)]
    [(= (world-state w) 1) (mouseHandler1 w mx my event)]
    [(= (world-state w) 2) (mouseHandler2 w mx my event)]
    [(= (world-state w) 3) (mouseHandler3 w mx my event)]
    [(= (world-state w) 4) (mouseHandler4 w mx my event)]
    ))

;MOUSE HANDLER;Does nothing
(define (mouseHandler0 w mx my event) w)

;MOUSE HANDLER ; SongSelect
(define (mouseHandler2 w mx my event)
  (cond
    [(and (string=? event "button-up") (withinButton w buttons (make-posn mx my))) ;Is click!
     (both (pstream-queue ps (button-song (whichButton w buttons (make-posn mx my))) (pstream-current-frame ps))
           (local [(define w1 (button-world (whichButton w buttons (make-posn mx my))))] (make-world (world-state w1) (world-posn w1) (world-score w1) (addDelay (world-beatmap w1)))))]      
    [else  (make-world (world-state w) (make-posn mx my) (world-score w) (world-beatmap w))];Nope not a click
    )
  )

;MOUSE HANDLER Instructions
;Returns the world with updated x and y coordinates
(define (mouseHandler1 w mx my event)
  w)

;MOUSE HANDLER ; Playfield
;Returns the world with updated x and y coordinates
(define (mouseHandler3 w mx my event) ;event is essentially ignored since we’re looking at keys
(cond
  [(empty? (world-beatmap w)) (make-world 4 (make-posn 0 0) (world-score w) '() )]
  [(and (string=? "button-down" event) (inLeniency? (first (world-beatmap w)))) ;Is the nearest note within leniency? 
        (inCircle? w (first (world-beatmap w)))] 
  [else (make-world (world-state w) (make-posn mx my) (world-score w) (world-beatmap w))]))

(define (mouseHandler4 w mx my event)
    (cond
    [(and (inRegion? (make-posn mx my) (button-region backButton));Cursor in button?
          (string=? event "button-up"));Is click?
     (button-world backButton)];Go back to starting screen
    [else (make-world (world-state w) (make-posn mx my) (world-score w) (world-beatmap w))];Nope not a click
    )
  )

;KEYHANDLER
(define (keyHandler w k)
  (cond
    [(= (world-state w) 0) (keyHandler0 w k)]
    [(= (world-state w) 1) (keyHandler1 w k)]
    [(= (world-state w) 2) (keyHandler2 w k)]
    [(= (world-state w) 3) (keyHandler3 w k)]
    [(= (world-state w) 4) (keyHandler4 w k)]
    ))

;KeyHandler Menu
(define (keyHandler0 w k)
  (cond
    [(or (string=? k "z") (string=? k "x"))
     (make-world 1 (make-posn 0 0) 0 currentBeatmap)]
    [else w]
    )
  )

;KeyHandler SongSelect
(define (keyHandler2 w k)
  w
  )

;KeyHandler Instructions
(define (keyHandler1 w k)
  (cond
  [(and (correctKey? w k) (= (world-state w) 1))
     (make-world 2 (world-posn w) (world-score w) (world-beatmap w))]))

;KeyHandler Playfield
;Checks for correct key, if the circle is in leniency, and if the cursor is in the circle
(define (keyHandler3 w k) ;ADD CHECK FOR CORRECT KEY; ADD CHECKS TO PREVENT HOLDING DOWN KEYS; ADD EMPTY MAP CASE
    (cond
    [(empty? (world-beatmap w)) (make-world 4 (make-posn 0 0) (world-score w) '() )]
    [(and (correctKey? w k) (inLeniency? (first (world-beatmap w)))) ;Is the nearest note within leniency? 
        (inCircle? w (first (world-beatmap w)))] ;Is the cursor in the circle? (+pts if yes)
    [else w]
    )
)

(define (keyHandler4 w k)
  w;im a doge
  )

;Queues the song and alters the beatmap to reflect time spent in the menu
(define (addDelay beatmap)
  (cond
    [(empty? beatmap) '()]
    [else (cons
           (make-note
            (+ (pstream-current-frame ps) (note-time (first beatmap)))
            (note-location (first beatmap)))
           (addDelay (rest beatmap)))]))
;Takes a beatmap and adds an offset to it

;Offset currently must be looked at manually
(define (addOffset beatmap offset)
    (cond
    [(empty? beatmap) '()]
    [else (cons
           (make-note
            (+ offset (note-time (first beatmap)))
            (note-location (first beatmap)))
           (addDelay (rest beatmap)))]))

;Get the distance of a note to a current distance
(define (getNoteDistance note)
(abs (- (pstream-current-frame ps) (note-time note)))
)

;Check if a note is within the current range
(define (inLeniency? note)
    (<= (getNoteDistance note) timingLenience)    
)

;Check if the correct key was pressed
(define (correctKey? w k)
  (cond
    [(or (string=? key1 k) (string=? key2 k)) true]
    [else false]
   )
)

;andQueue instead of andplay
(define (andQueue rs w)
  (both (pstream-queue ps1 rs (+ 1000 (pstream-current-frame ps1))) w))
;KEY HELPER - Check if mouse is in circle
;Takes a world, note
;Returns a world with added score if the mouse is in the circle
(define (inCircle? w note)
   (cond 
     [(<= (distance (world-posn w) (note-location note)) cRad)
      (andQueue hitSound (make-world (world-state w) (world-posn w) (+ (world-score w) scoreInc) (rest (world-beatmap w))))] ;Yep, it’s inside. Give points!; remove the circle hit
     [else (andQueue missSound (make-world (world-state w) (world-posn w) (world-score w) (rest (world-beatmap w))))] ;Nope, the player sucks. No points!
))

;Computes a distance between two positions
(define (distance posn1 posn2)
    (sqrt (+ (sqr (- (posn-x posn2) (posn-x posn1))) (sqr (- (posn-y posn2) (posn-y posn1)))))
)

;TICK HELPER
;Checks if given note has been past already
;returns true if the note has past
;note->boolean
(define (hasPast n)
  (> (pstream-current-frame ps) (+ (note-time n) timingLenience)))
;Tick HANDLER
;Takes in the world and updates the beatmap by removing notes that have already played
;world->world
(define (tickHandler w)
  (cond
    [(empty? (world-beatmap w)) w]
    [(hasPast (first (world-beatmap w)))
     (make-world (world-state w) (world-posn w) (world-score w) (rest (world-beatmap w)))]
    [else w]))

(define ps (make-pstream))
(define ps1 (make-pstream))
(big-bang w1
[on-tick tickHandler] ;Truncates the beatmap
[on-mouse mouseHandler] ;Gives the world the new mouse coordinates
[on-key keyHandler] ;Either adds score or does nothing
[to-draw drawHandler] ;Draws circles and score
)



