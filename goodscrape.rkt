#lang racket

(require
  net/url
  net/uri-codec
  net/cookies
  net/head
  (only-in srfi/13 string-filter)
  srfi/14
  html-parsing
  sxml
  (prefix-in js: javascript))

;;
;; Types
;;

(struct goodreads-book
  (title author pages isbn isbn13 link)
  #:transparent
  #:extra-constructor-name make-goodreads-book)

;;
;; Parameters
;;

(define goodreads-current-authenticity-token (make-parameter null))
(define goodreads-current-nonce (make-parameter null))

;;
;; XPath Selectors
;;

(define goodreads-authenticity-token
  (sxpath "string(//input[@name='authenticity_token']/@value)"))

(define goodreads-nonce
  (sxpath "string(//input[@name='n']/@value)"))

(define goodreads-user-id-script
  (sxpath "string(//script[contains(., 'uid')])"))

(define goodreads-user-shelf
  (sxpath "//div[contains(@class, 'userShelf')]//a/@href"))

(define goodreads-books-review
  (sxpath "//table//tbody//tr[contains(@class, 'review')]"))

(define goodreads-review-title
  (sxpath "normalize-space(string(//td[contains(@class, 'title')]//a/text()[2]))"))

(define goodreads-review-link
  (sxpath "string(//td[contains(@class, 'title')]//a/@href)"))

(define goodreads-review-author
  (sxpath "normalize-space(string(//td[contains(@class, 'author')]//a/text()))"))

(define goodreads-review-pages
  (sxpath "//td[contains(@class, 'num_pages')]//*[contains(@class, 'value')]//nobr/text()"))

(define goodreads-review-isbn
  (sxpath "normalize-space(string(//td[contains(@class, 'isbn')]//*[contains(@class, 'value')]/text()))"))

(define goodreads-review-isbn13
  (sxpath "normalize-space(string(//td[contains(@class, 'isbn13')]//*[contains(@class, 'value')]/text()))"))

(define goodreads-review-rating
  (sxpath "count(//td[contains(@class, 'rating')]//*[contains(@class, 'p10')])"))

(define goodreads-review-filters-token
  (sxpath #<<EOF
     substring-before(
       substring-after(
         normalize-space(
           string(//div[@id = 'reviews']//*[contains(a/@id, 'more_filters')]/script)
         ),
         '//<![CDATA['),
       '//]]'
     )
EOF
          ))
(define goodreads-link-onclick
  (sxpath "//a/@onclick/text()"))

(define goodreads-link-next-page
  (sxpath "string(//a[@class='next_page']/@onclick)"))

(define goodreads-starred-reviewers-links
  (sxpath #<<EOF
    //*[@itemtype='http://schema.org/Review']
    //*[@itemtype='http://schema.org/Person' and
        count(../*[contains(@class, 'staticStars')]) = 1]
    /a[@itemprop='url']/@href/text()
EOF
          ))

;; Utilities

;; Interpolate HTML escape codes
(define interpolate-escape-codes
  (compose
   (lambda (script) (string-replace script "&amp;" "&"))
   (lambda (script)
     (regexp-replace*
      #rx"&#([0-9]+);"
      script
      (lambda (match code)
        (string (integer->char (string->number code))))))))

;; Interpolate HTML escape codes recursively
(define (interpolate-escape-codes* script)
  (define interpolated (interpolate-escape-codes script))
  (if (equal? script interpolated)
      script
      (interpolate-escape-codes* interpolated)))

(define (goodreads-begin! base)
  (define url (combine-url/relative base "/user/sign_in"))
  (define-values (port headers) (get-pure-port/headers url))
  (define xexp (dynamic-wind void
                             (lambda () (html->xexp port))
                             (lambda () (close-input-port port))))
  (extract-and-save-cookies!
   (extract-all-fields (string->bytes/utf-8 headers))
   base)
  ;; These are CSRF-like tokens (and a nonce) for login
  (goodreads-current-authenticity-token (goodreads-authenticity-token xexp))
  (goodreads-current-nonce (goodreads-nonce xexp)))

(define (goodreads-login! base email password)
  (define url (combine-url/relative base "/user/sign_in"))
  ;; Construct the POST payload
  (define nonce-field (cons 'n (goodreads-current-nonce)))
  (define token-field
    (cons 'authenticity_token (goodreads-current-authenticity-token)))
  (define email-field (cons (string->symbol "user[email]") email))
  (define password-field (cons (string->symbol "user[password]") password))
  (define rme-field (cons 'remember_me "on"))
  (define next-field (cons 'next "Sign+in"))
  (define form (list nonce-field
                     token-field
                     email-field
                     password-field
                     rme-field
                     next-field))
  ;; Serialize form and headers
  (define form-data (string->bytes/utf-8 (alist->form-urlencoded form)))
  (define base-cookie (bytes->string/utf-8 (cookie-header base)))
  (define base-cookie-header (string-join (list "Cookie:" base-cookie)))
  (define content-length-header
    (string-join
     (list "Content-Length:"
           (number->string (bytes-length form-data)))))
  (define send-header (list base-cookie-header
                            content-length-header
                            "Content-Type: application/x-www-form-urlencoded"))
  (define port (post-impure-port url form-data send-header))
  (define recv-header (dynamic-wind void
                                    (lambda () (purify-port port))
                                    (lambda () (close-input-port port))))
  ;; This will save the session cookie
  (extract-and-save-cookies!
   (extract-all-fields (string->bytes/utf-8 recv-header)) base))

(define (goodreads-current-user-id base)
  (define url (combine-url/relative base "/recommendations/to_me"))
  (define base-cookie (bytes->string/utf-8 (cookie-header base)))
  (define base-cookie-header (string-join (list "Cookie:" base-cookie)))
  (define headers (list base-cookie-header))
  (define-values (port ignore)
    (get-pure-port/headers url headers))
  (define xexp (dynamic-wind void
                             (lambda () (html->xexp port))
                             (lambda () (close-input-port port))))
  ;; Match the following AST
  ;; | ...
  ;; | var <varname> = { ..., uid: value }
  (match (js:parse-program-unit (goodreads-user-id-script xexp))
    [(list _ ...
           (js:VariableDeclaration
            _
            (list _ ...
                  (js:VariableInitializer
                   _ _ (js:ObjectLiteral
                        _
                        (list _ ...
                              (cons (js:Identifier _ 'uid)
                                    (js:StringLiteral _ value)))))))) value]))

(define (goodreads-shelves base id)
  (define url (combine-url/relative base (string-append "/review/list/" id)))
  (define-values (port ignore)
    (get-pure-port/headers url))
  (define xexp (dynamic-wind void
                             (lambda () (html->xexp port))
                             (lambda () (close-input-port port))))
  (map (curry apply (lambda (tag link)
                      (cdr (assq 'shelf (url-query (string->url link))))))
       (goodreads-user-shelf xexp)))

(define (goodreads-review-xexp->book xexp)
  (let ([title (goodreads-review-title xexp)]
        [author (goodreads-review-author xexp)]
        ;; Page count is localized and may contain
        ;; separator characters which must be stripped
        [pages (string-filter
                char-set:digit
                (string-join (goodreads-review-pages xexp)))]
        [isbn (goodreads-review-isbn xexp)]
        [isbn13 (goodreads-review-isbn13 xexp)]
        [rating (goodreads-review-rating xexp)]
        [link (goodreads-review-link xexp)])
    (cons (goodreads-book title author (string->number pages) isbn isbn13 link)
          rating)))

;; Returns a single page of shelved books for a user id
(define (goodreads-books
         base id
         #:shelf [shelf #f]
         #:sort [key #f]
         #:asc [asc #f]
         #:limit [limit #f]
         #:page [page #f])
  (define req-url
    (struct-copy url
                 (combine-url/relative base (string-append "/review/list/" id))
                 [query (append
                         (list (cons 'print "true"))
                         (if page
                             (list (cons 'page (number->string page)))
                             null)
                         (if key
                             (list (cons 'sort (symbol->string key)))
                             null)
                         (if limit
                             (list (cons 'per_page (number->string limit)))
                             null)
                         (if shelf
                             (list (cons 'shelf shelf))
                             null)
                         (if asc
                             (list (cons 'order "a"))
                             null))]))
  (define headers (let ([cookie (cookie-header base)])
                    (if cookie
                        (list
                         (string-join (list "Cookie:"
                                            (bytes->string/utf-8 cookie))))
                        null)))
  (define-values (port ignore)
    (get-pure-port/headers req-url headers))
  (define xexp (dynamic-wind void
                             (lambda () (html->xexp port))
                             (lambda () (close-input-port port))))
  (map goodreads-review-xexp->book (goodreads-books-review xexp)))

(define (goodreads-books-stream
         base id
         [start 1]
         #:shelf [shelf #f]
         #:sort [key #f]
         #:asc [asc #f]
         #:limit [limit #f])
  (stream-lazy
   (letrec ([page (goodreads-books base id
                                   #:shelf shelf
                                   #:sort key #:asc asc
                                   #:limit limit #:page start)]
            ;; Recursively decompose the page into individual items
            ;; and request the next page once the page is fully consumed
            [dematerialize (lambda (head tail)
                             (stream-cons
                              head
                              (if (null? tail)
                                  (goodreads-books-stream base id
                                                          (+ start 1)
                                                          #:shelf shelf
                                                          #:sort key #:asc asc
                                                          #:limit limit)
                                  (dematerialize (car tail) (cdr tail)))))])
     (if (null? page)
         empty-stream
         (dematerialize (car page) (cdr page))))))

(define (script->goodreads-rating-link script)
  (match (js:parse-expression script)
    ;; Match the AST
    ;; new <constructor>(link,  { ..., parameters: <operand> + encodeURIComponent(token) })
    [(js:NewExpression
      _ _
      (list (js:StringLiteral _ link)
            (js:ObjectLiteral
             _
             (list _ ...
                   (cons (js:Identifier _ 'parameters)
                         (js:InfixExpression
                          _ _ '+
                          (js:CallExpression
                           _ (js:VarReference _ (js:Identifier _ 'encodeURIComponent))
                           (list (js:StringLiteral _ token)))))))))
     (letrec ([lnk (string->url link)]
              [qry (url-query lnk)]
              [rtg (string->number (cdr (assq 'rating qry)))]
              [tkn (cons 'authenticity_token token)])
       (cons rtg (struct-copy url lnk [query (append qry (list tkn))])))]))

(define (goodreads-review-links-by-stars base link)
  (define grurl (combine-url/relative base link))
  (define headers (let ([cookie (cookie-header base)])
                    (if cookie
                        (list
                         (string-join (list "Cookie:"
                                            (bytes->string/utf-8 cookie))))
                        null)))
  (define-values (port ignore)
    (get-pure-port/headers grurl headers))
  (define xexp (dynamic-wind void
                             (lambda () (html->xexp port))
                             (lambda () (close-input-port port))))
  ;; Match the AST
  ;; ...
  ;; var newTip = new Tip(<expression>, html, <expression>);
  ;; ...
  (define fxexp (match (js:parse-program-unit (goodreads-review-filters-token xexp))
                  [(list (js:VariableDeclaration
                          _ (list (js:VariableInitializer
                                   _ (js:Identifier _ 'newTip)
                                   (js:NewExpression
                                    _ (js:VarReference _ (js:Identifier _ 'Tip))
                                    (list _ (js:StringLiteral _ html) _)))))
                         _ ...) (html->xexp html)]))
  (map (compose
        (lambda (pair)
          (define rating (car pair))
          (define datum (cdr pair))
          (define path (url-path datum))
          (define query (url-query datum))
          (define fragment (url-fragment datum))
          (cons rating
                (struct-copy url base [path path] [query query] [fragment fragment])))
        script->goodreads-rating-link
        interpolate-escape-codes*)
       (goodreads-link-onclick fxexp)))

;; Parses a text/javascript payload for a review listing in a tolerant
;; way. Received payloads sometimes cause issues with the parser
(define (goodreads-linked-review-lex lexer)
  (define datum
    (and
     (match (send lexer read-token) [(js:token 'ID 'Element _) #t] [_ #f])
     (send lexer match '|.|)
     (match (send lexer read-token) [(js:token 'ID 'update _) #t] [_ #f])
     (send lexer match '|(|)
     (match (send lexer read-token) [(js:token 'STRING "reviews" _) #t] [_ #f])
     (send lexer match '|,|)
     (send lexer match 'STRING)))
  (js:token-contents datum))

;; Parses a single page of dynamic/embedded reviews
(define (goodreads-linked-reviews link)
  (define headers
    (cons "Accept: text/javascript"
          (let ([cookie (cookie-header link)])
            (if cookie
                (list
                 (string-join (list "Cookie:"
                                    (bytes->string/utf-8 cookie))))
                null))))
  (define-values (port ignore)
    (get-pure-port/headers link headers))
  (define xexp
    (html->xexp
     (dynamic-wind
      void
      (lambda () (goodreads-linked-review-lex (new js:lexer% [port port])))
      (lambda () (close-input-port port)))))
  (define reviewers
    (map (compose (lambda (reviewer)
                    (struct-copy url link
                                 [path (url-path reviewer)]
                                 [query (url-query reviewer)]
                                 [fragment (url-fragment reviewer)]))
                  string->url)
         (goodreads-starred-reviewers-links xexp)))
  (define script (goodreads-link-next-page xexp))
  (define next-link
    (and (non-empty-string? script)
         ;; Match the AST
         ;; new <constructor>(prefix, { ..., parameters: <operand> + encodeURIComponent(token) })
         (match (js:parse-expression (interpolate-escape-codes* script))
           [(js:NewExpression
             _ _
             (list
              (js:StringLiteral _ prefix)
              (js:ObjectLiteral
               _ (list
                  _ ...
                  (cons (js:Identifier _ 'parameters)
                        (js:InfixExpression
                         _ _ '+
                         (js:CallExpression
                          _ (js:VarReference _ (js:Identifier _ 'encodeURIComponent))
                          (list (js:StringLiteral _ token)))))))))
            (let ([prefix-url (string->url prefix)]
                  [token-param (cons 'authenticity_token token)])
              (struct-copy url link
                           [path (url-path prefix-url)]
                           [query (append (url-query prefix-url) (list token-param))]
                           [fragment #f]))])))
  (values reviewers next-link))

(define (goodreads-linked-reviews-stream link)
  (stream-lazy
   (letrec-values ([(page next) (goodreads-linked-reviews link)]
                   [(dematerialize) (lambda (head tail)
                                      (stream-cons
                                       head
                                       (if (null? tail)
                                           (if next (goodreads-linked-reviews-stream next) empty-stream)
                                           (dematerialize (car tail) (cdr tail)))))])
     (if (null? page)
         empty-stream
         (dematerialize (car page) (cdr page))))))

(define goodreads-reviews-by-stars-stream
  (compose
   (curry apply stream-append)
   (curry
    map
    (lambda (rating-link)
      (stream-map (curry cons (car rating-link))
                  (goodreads-linked-reviews-stream (cdr rating-link)))))
   goodreads-review-links-by-stars))
  

(provide
 goodreads-book
 make-goodreads-book
 goodreads-current-authenticity-token goodreads-current-nonce
 goodreads-begin! goodreads-login!
 goodreads-current-user-id
 goodreads-shelves
 goodreads-books-stream
 goodreads-review-links-by-stars
 goodreads-linked-reviews-stream
 goodreads-reviews-by-stars-stream)