#lang racket

(require "impl/tables.rkt" "impl/core-streams.rkt")

;; Columns: airline,airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "openflights/airlines.csv"))

;; Columns: airport-id,name,city,country,iata,icao,latitude,longitude,altitude,timezone,dst,tz-database-timezone,type,source
(define airports (load-table "openflights/airports.csv"))



(query/rows
 (from routes)
 (where (lambda (row)
          (equal? (hash-ref row 'codeshare)
                  "Y")))
 (join (from airlines #:qualify 'airline)
       'airline-id 'airline.airline-id)
 (where (lambda (row)
          (equal? (hash-ref row 'airline.name)
                  "American Airlines")))
    
 (select 'source-airport 'destination-airport)
 (limit 3))








