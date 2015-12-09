;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ReaderTester) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require rsound) ;NOT NEW
(require 2htdp/image) ;NOT NEW
(require 2htdp/universe) ;NOT NEW

(require 2htdp/batch-io)
(require racket/string)

(define FR 44100);NOT NEW

;Notes are structures with Time and Location ;NOT NEW
;Time is a number in frames ;NOT NEW
;Location is a posn ;NOT NEW
(define-struct note [time location]) ;NOT NEW

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

;Takes milliseconds
;returns them as frames
(define (ms->frames ms)
  (* (* ms .001) FR)
  )

;Takes a list of 3 numbers
;Puts them into a note structure
(define (noteMapper list)
  (make-note (ms->frames (string->number (third list))) (make-posn (string->number (first list)) (string->number (second list))))
  )

(define currentBeatmap (map noteMapper splicedBeatmap));Map to a list of notes