#lang racket

(require
  net/url-connect
  net/url
  "goodscrape.rkt")

(define insecure (make-parameter #f))

(define book-link
  (command-line
   #:program "Goodreads reviewer extractor"
   #:once-each
   [("--insecure") "Do not validate TLS certificate" (insecure #t)]
   #:args (link) (string->url link)))

(when (not (insecure))
    (current-https-protocol 'secure))

(define goodreads-base-url
  (struct-copy url book-link [path null] [query null] [fragment #f]))

(define goodreads-book-path
  (url->string
   ;;        Sc Us Ho Po Ab
   (make-url #f #f #f #f #f 
             (url-path book-link)
             (url-query book-link)
             (url-fragment book-link))))

(stream-for-each
 (lambda (rating-rater)
   (define rating (car rating-rater))
   (define rater (cdr rating-rater))
   (define rater-url (combine-url/relative goodreads-base-url (url->string rater)))
   (printf "~a,~a\n" rating (url->string rater)))
 (goodreads-reviews-by-stars-stream goodreads-base-url goodreads-book-path))