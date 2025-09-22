This is an example of a LATERAL for a correlated top-k query.

Find the top 2 temperature observations for each country.

It is related to the ONCE algorithm in
["deep injection" discussion document](https://github.com/w3c/sparql-query/blob/main/discussion/Defining_the_DEEP_INJECTION_approach_for_EXISTS.md)
because it operates on each outer partial result.

```
PREFIX : <http://example/>

:A :location "Country A" ;
   :temperature 23 ;
   :temperature 25 ;
   :temperature 30 ;
   :temperature 18 ;
   .

:B :location "Country B" ;
   :temperature 13 ;
   :temperature 15 ;
   .

:C :location "Country C" ;
   :temperature 5 ;
   .
  
:D :location "Country D" ;
   .
```

Query:

```
PREFIX : <http://example/>

## Two highest temperatures for each location
SELECT * {
    ?x :location ?label .
    LATERAL {
       SELECT * {
         ?x :temperature ?temp
       }
       ORDER BY DESC(?temp)
       LIMIT 2
   } 
} GROUP BY ?x
```

Results:

```
---------------------------
| x  | label       | temp |
===========================
| :A | "Country A" | 30   |
| :A | "Country A" | 25   |
| :B | "Country B" | 15   |
| :B | "Country B" | 13   |
| :C | "Country C" | 5    |
---------------------------
```
