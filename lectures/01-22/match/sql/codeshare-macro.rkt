#lang racket

(require "impl/tables.rkt" "impl/syntax-spec.rkt")

;; Columns: airline,airline-id,source-airport,source-airport-id,destination-airport,destination-airport-id,codeshare,stops,equipment
(define routes (load-table "openflights/routes.csv"))

;; Columns: airline-id,name,alias,iata,icao,callsign,country,active
(define airlines (load-table "openflights/airlines.csv"))

;; Columns: airport-id,name,city,country,iata,icao,latitude,longitude,altitude,timezone,dst,tz-database-timezone,type,source
(define airports (load-table "openflights/airports.csv"))




(query/rows
 (from routes (airline-id
               codeshare
               source-airport
               destination-airport))
 (where (equal? codeshare "Y"))
 (join airlines (name [airline.id airline-id])
       airline-id airline.id)
 (where (equal? name "American Airlines"))
 (select source-airport destination-airport)
 (limit 3))








