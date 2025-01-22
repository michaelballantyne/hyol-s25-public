#lang racket

(require "impl/tables.rkt" "impl/core-streams.rkt")

;; Columns: airline,airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "openflights/airlines.csv"))

;; Columns: airport-id,name,city,country,iata,icao,latitude,longitude,altitude,timezone,dst,tz-database-timezone,type,source
(define airports (load-table "openflights/airports.csv"))

(printf "~a routes, ~a airlines, ~a airports\n"
        (length routes) (length airlines) (length airports))

(printf "~a codeshare routes\n"
        (length
         (query/rows
          (from routes)
          (where (lambda (row)
                   (equal? (hash-ref row 'codeshare)
                           "Y"))))))

(time
  (query/rows
    (from routes)
    (where (lambda (row)
             (equal? (hash-ref row 'codeshare)
                     "Y")))
    (join (query
            (from airlines #:qualify 'airline)
            (where (lambda (row)
                     (equal? (hash-ref row 'airline.name)
                             "American Airlines"))))
          'airline-id 'airline.airline-id)
    (select 'source-airport 'destination-airport)
    (limit 3))
)


