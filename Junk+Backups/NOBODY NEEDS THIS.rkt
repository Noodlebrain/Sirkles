;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |NOBODY NEEDS THIS|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Takes a list of 1strings, a string, and a natural number n where n<size of list of 1strings
;;;;;WE WILL USE n=3 FOR OUR PROGRAM;;;;;
;Returns a list of ints of size n
;Items in the list are ordered as separated by the splicer
(define (spliceToInts s splicer n)
  (cond
    [(= n 0) '()];n is 0, we're done
    [(string=? (first s) ",") (spliceToInts (rest s) splicer (- n 1))];it's a comma, don't add this one, increment n, keep going
    [else (cons (string->number (1strsUntilSplicer s splicer))
                (spliceToInts (restNtimes s (length (1strsUntilSplicer s splicer))) splicer n))];it's not a comma, append it, keep going
    )
  )

;Takes a list of 1strings and a splicer
;Returns a string with all the 1strings concatenated until the splicer
(define (1strsUntilSplicer s splicer)
  )

;Takes a list of 1strings and a natural number
;Returns a list equal to the list of 1strings except with some elements at the start removed
;The number of elements removed is equal to the natural number
(define (restNtimes s n)
  )