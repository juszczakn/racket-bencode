#lang racket

(provide bencode-decode-file)

(define (read-in-int starter end-token)
  (define (loop total s)
    (cond [(equal? end-token s) (string->number total)]
          [else
           (loop (string-append total s) (read-string 1))]))
  (lambda (s) (loop starter s)))

(define (bencode-int)
  (let ((loop (read-in-int "" "e")))
    (loop (read-string 1))))

(define (bencode-string starter)
  (let* ((loop (read-in-int starter ":"))
         (len (loop (read-string 1))))
    (read-string len)))

(define (bencode-dict)
  (let ((h (make-hash)))
    (define (loop)
      (let ((x (bencode-switch)))
        (if (not x)
            h
            (let ((y (bencode-switch)))
              (hash-set! h x y)
              (loop)))))
    (loop)))

(define (bencode-list [l (list)])
  (let ((item (bencode-switch)))
    (if (not item)
        (flatten l)
        (bencode-list (cons item l)))))

(define (bencode-switch [s (read-string 1)])
  (cond [(equal? "i" s) (bencode-int)]
        [(equal? "d" s) (bencode-dict)]
        [(equal? "l" s) (bencode-list)]
        [(eof-object? s) #f]
        [(string->number s) (bencode-string s)]
        [else #f]))

(define (bencode-decode-file f)
  (with-input-from-file f
    (lambda ()
      (define (loop data)
        (let ((d (bencode-switch)))
          (if (not d)
              (flatten data)
              (loop (cons data d)))))
      (loop '()))))
