;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Sirkles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;ADDED BEATMAP READER
;SET TO ADVANCED STUDENT OR IT WON’T RUN.
;Eric working on circle fade in
;Keyhandler needs spam control
;http://i.imgur.com/Bxbhj20.png
; https://www.mediafire.com/?c2uwsdb35bxc30m
;^Contains the racket file as well as the images in the correct directory for the code
;Sometimes crashes with too many keypresses; the miss sound sometimes causes crashes

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
(define length1 1024)
(define height1 768)

;Ratio for Osu beatmaps to our beatmaps
(define xRatio 1.75)
(define yRatio 1.75)

;Define the Canvas
(define SCENE (empty-scene length1 height1 (color 0 0 0 0)))

;timingLenience (milliseconds -> frames)
(define timingLenience 11025)
;1653.75

;Score is a number
(define initialScore 0)

;The amount of score added per circle hit
(define scoreInc 100)

;Circle radius
(define cRad 75)

;The key being tapped to hit circles
(define key1 "d")
(define key2 "f")

;Hitsound
(define hitSound (rs-scale .5 (rs-read "effectSounds/hitSound.wav")))
(define missSound (rs-scale .5 (rs-read "effectSounds/comboBreak.wav")))
;Song
(define song (rs-read "beatmaps/map2.wav"))

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
(define approachRate (s (* .001 600)))

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

;Notes are structures with Time and Location
;Time is a number in frames
;Location is a posn
(define-struct note [time location])

;Beatmaps are lists of note structures
;;NEW: Contains a song
;(define beatmap (list note note note ... '()))
(define untrimmedBeatmap (read-words/line "beatmaps/map2.txt"));Import beatmap text

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

;Creating the beatmap for the world
(define currentBeatmap (map changeRatio (map noteMapper splicedBeatmap)));Map to a list of notes

;Image transparency
;Takes an image and and alpha value and returns a transparent image
(define (imgTrans image alpha)
     (color-list->bitmap
      (map (lambda (color) (clrTrans color alpha))
        (image->color-list image))
      (image-width image)
      (image-height image)
     )
  )

;Color transparency
;Takes a color and an alpha value and returns a transparent
(define (clrTrans color alpha)
  (make-color (color-red color) (color-green color) (color-blue color) (floor (abs alpha)))
  )

;0 to 255 scalar
;Takes a number
;Returns a value from 0 to 255
(define (alphaScale n)
  (cond
    [(<= n 0) 0]
    [(>= n 255) 255]
    [else n]
    )
  )

;Maybe have multiple types of worlds
;Then there would be general key/mouse/draw handlers
;These would then call a specific handler depending on the type of world
;Can use this to have things like difficulty selects or beatmap editors

;World holds:
;(Current) MouseX, MouseY
;Total score
;Dynamic beatmap, altered by on-tick
;;NEW
;cRad
;Type of world/State
(define-struct world (posn score beatmap))
(define w1 (make-world (make-posn 0 0) 0 currentBeatmap)) ;initialize world

; DRAWHANDLER

(define (drawHandler w)
  (place-images
   (list cursor (text (number->string (world-score w)) 20 "white"))
   (list (world-posn w) (make-posn (- length1 25) 25))
   (underlay background
        	(cond
          	[(empty? (world-beatmap w)) SCENE]
          	[else
           	(render (world-beatmap w))]))))    

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
(define (noOfCircles n beatmap)
  (cond [(= 0 n) '()]
        [else (cons hitcircle
                    (noOfCircles (sub1 n) beatmap))]))

;approachCircles
;List -> List
;takes list of notes and creates a list approach circles for notes
(define (approachCircles beatmap)
  (cond
    [(empty? beatmap) '()]
    [else (cons (circle
                 (+ cRad (* cRad (/ (+ (- (note-time (first beatmap)) (pstream-current-frame ps))) approachRate)))
                 "outline" pen1) (approachCircles (rest beatmap)))]))


;RENDER HELPER
;List -> image  
;Takes a list of things to render  
;Renders them on a scene
; renders note circle and approach ring: radius of ring decreases based on time before note
; should be hit
(define (render beatmap)
  (local [(define x (searcher beatmap)) (define y (map note-location x))]
    (place-images (append (noOfCircles (length x) beatmap) (approachCircles x))
                (append y y)
                SCENE)))



;MOUSE HANDLER
;Returns the world with updated x and y coordinates
(define (mouseHandler w mx my event) ;event is essentially ignored since we’re looking at keys
(cond
  [(and (string=? "button-down" event) (inLeniency? (first (world-beatmap w)))) ;Is the nearest note within leniency? 
        (inCircle? w (first (world-beatmap w)))] 
  [else (make-world (make-posn mx my) (world-score w) (world-beatmap w))]
)
)
;andplay for hitsounds
;KEYHANDLER(v1)
;Takes a world and a key
;Returns a key
;If the key is the “hitkey” as specified by ___
    ;Return the world with “the key is hit”
;Else just return the world}
;KEYHANDLER Complete?:[✓] Verified by someone else?: [X]
;Takes a world and a key
;Returns a world with either added score, or the same as before
;If world says a key is pressed
;IS IT THE RIGHT KEY?
    ;Check the beatmap for nearby timing points [HELPER]
;(Amount for nearby is a constant);timingLenience
        ;Nothing nearby, return the world as it is
        ;Something nearby -> Give the following helper the note nearby
            ;Check to see if the mouse is within the circle [HELPER]
                ;In the circle, return the world with added score        
;Not in circle, return the world as it is    
;Else return the world as is
(define (keyHandler w k) ;ADD CHECK FOR CORRECT KEY; ADD CHECKS TO PREVENT HOLDING DOWN KEYS; ADD EMPTY MAP CASE
    (cond
    [(and (correctKey? w k) (inLeniency? (first (world-beatmap w)))) ;Is the nearest note within leniency? 
        (inCircle? w (first (world-beatmap w)))] ;Is the cursor in the circle? (+pts if yes)
    [else w]
    )
)

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
;KEY HELPER - Check if mouse is in circle Complete?:[✓] Verified by someone else?: [X]
;Takes a world, note(MIGHT BE A BOOLEAN)
;Returns a world with added score if the mouse is in the circle
;Distance formula between circleXcircleY and mxmy
;If: Distance <= given radius || return true
;else || return false
(define (inCircle? w note)
   (cond 
     [(<= (distance (world-posn w) (note-location note)) cRad)
      (andQueue hitSound (make-world (world-posn w) (+ (world-score w) scoreInc) (rest (world-beatmap w))))] ;Yep, it’s inside. Give points!; remove the circle hit
     [else (andQueue missSound (make-world (world-posn w) (world-score w) (rest (world-beatmap w))))] ;Nope, the player sucks. No points!
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
    [(hasPast (first (world-beatmap w)))
     (make-world (world-posn w) (world-score w) (rest (world-beatmap w)))]
    [else w]))


;returns true if the current world's beatmap is empty
(define (beatMapEmpty? w)
  (empty? (world-beatmap w)))

(define ps (make-pstream))
(define ps1 (make-pstream))
(pstream-queue ps song 0)
(big-bang w1
[stop-when beatMapEmpty?]
[on-tick tickHandler] ;Truncates the beatmap
[on-mouse mouseHandler] ;Gives the world the new mouse coordinates
[on-key keyHandler] ;Either adds score or does nothing
[to-draw drawHandler] ;Draws circles and score
)

;->Initialize as menu

;Menu has slider for cRad
;Menu has button to go to [song select]
;Drawhandler, mouse handler, tick handler to deal altering the song timing and note timings

;Song select has 4 beatmaps/songs for now that are clickable
	;Click these to start and go to [playfield]
;Song select has a back button to go to [menu]
;Draw handler->show maps and button, mouse handler -> select and start a map or go back
;Use a list of beatmaps to show available songs?

;Playfield plays a map
;End of playfield, go to [menu]

