#lang racket/gui
(require (planet jaymccarthy/dijkstra:1:1))
(define-struct edge (end weight))
(define-struct graph (adj))
(define (create-graph)
  (make-graph (make-hasheq)))
(define (graph-add! g start end [weight 0])
  (hash-update! (graph-adj g) start
                (lambda (old)
                  (cons (make-edge end weight) old))
                '()))
(define (graph-shortest-path g src [stop? (lambda (n) #f)])
  (shortest-path (lambda (n) (hash-ref (graph-adj g) n '()))
                 (lambda (e) (edge-weight e))
                 (lambda (e) (edge-end e))
                 src
                 stop?))
; List of all stations
(define station-list (list "Kennington" "Elephant & Castle" "Borough" "London Bridge"
                           "Bank" "Moorgate" "Old Street" "Angel" "King's Cross St. Pancras"
                           "Euston" "Camden Town" "Kentish Town" "Tufnell Park" "Archway"
                           "Highgate" "East Finchley" "Finchley Central" "West Finchley"
                           "Woodside Park" "Totteridge & Whetstone" "High Barnet" "Chalk Farm"
                           "Belsize Park" "Hampstead" "Golders Green" "Brent Cross" "Hendon Central"
                           "Colindale" "Burnt Oak" "Edgware"))
; Create the graph and add edges
(define g (create-graph))
(for-each (lambda (i) (graph-add! g (list-ref station-list i) (list-ref station-list (+ i 1)) 1)) (range (- (length station-list) 1)))
; Function to calculate the path
(define (calculate-path from to)
  (define-values (distances prev) (graph-shortest-path g from (lambda (n) (equal? n to))))
  (define path (shortest-path-to prev to))
  (if path
      (string-join path " -> ")
      "No path found"))
; Function to be called when the "Find Route" button is clicked
(define (find-route)
  (let* ([from (send from-choice get-string-selection)]
         [to (send to-choice get-string-selection)])
    (when (and from to (not (equal? from to)))
      (let ([path (calculate-path from to)])
        (send route-text erase)
        (send route-text insert (string-append "From " from " to " to ": " path))))))
; GUI Definitions
(define myframe (new frame% [label "Transport Finder"] [width 400] [height 300]))
(define outer-panel (new vertical-panel% [parent myframe] [alignment '(center center)] [stretchable-width #f] [stretchable-height #t]))
(define bus-icon (read-bitmap "underground.png"))
(define bus-icon-label (new message% [parent myframe] [label bus-icon]))
(define inner-panel (new horizontal-panel% [parent outer-panel] [alignment '(center center)] [stretchable-width #f] [stretchable-height #f]))
; Dropdown for 'From' location
(define from-choice (new choice% [label (string-append "From")]
                                 [parent inner-panel]
                                 [choices station-list]))
; Dropdown for 'To' location
(define to-choice (new choice% [label (string-append "To")]
                               [parent inner-panel]
                               [choices station-list]))
; Horizontal panel for 'Transport Type' and 'Route Type'
(define type-panel (new horizontal-panel% [parent outer-panel] [alignment '(center center)] [stretchable-width #f] [stretchable-height #f]))
; Choice for transport type
(define transport-choice (new choice% [label (string-append "Transport Type")] [parent type-panel] [choices (list "Bus" "Train" "Tram")]))
; Choice for route type
(define route-type-choice (new choice% [label (string-append "Route Type")] [parent type-panel] [choices (list "Fastest" "Cheapest")]))
; Button to find the route
(define route-btn (new button% [parent outer-panel] [label "Find Route"]
                               [callback (lambda (o e) (find-route))]))
; Checkbox for nearby stops
(define stop-check-box (new check-box% [label "Nearby Stops"] [parent outer-panel] [value #t]))
; Message to display the route
(define route-text (new text%))
(define route-message (new editor-canvas% [parent outer-panel] [editor route-text] [min-width 400] [min-height 200]))
(send myframe show #t)