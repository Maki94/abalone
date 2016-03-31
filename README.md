# Abelone
- Generic algorithms
    - ```print-list```  
    - ```sortiraj```
    - ```dodaj```
    - ```pridruzi```
    - ```dodaj-potomke```
    - ```novi-cvorovi```
    - ```set-union2```           pronalazi uniju 2 skupa
    - ```set-difference2```      pronalazi razliku 2 skupa
    - ```difference-union```     "l1 \ (l2 union l3)"
    - ```without-last```         vraca listu bez poslednjeg elementa
- External functions
    - `(defun print-row (row)`
    - `(defun print-field (field)`

***
## Homework 1
  -  Problem formulation
     > Ovo ces ti da sredis
  - Interface
     - ```(defun get-empty (white black rows columns column state)```
        Vraća listu praznih polja u formatu ((čvor (lista potega)) (čvor (lista
potega)) ...)

            ((A (7)) (B NIL) (C (3 6 9)) (D (2 3 4 5 6 7 8 9)) (E (1 2 3 4 5 6 7 8 9))
            (F (1 2 3 4 5 6 7 8)) (G (1 4 7)) (H NIL) (I (3)))

     - ```(defun get-table (white black empty rows)```

           ((A ((1 U) (2 U) (3 U) (4 U) (5 W) (6 W) (7 E) (8 B) (9 B)))
           (B ((1 U) (2 U) (3 U) (4 W) (5 W) (6 W) (7 B) (8 B) (9 B)))
             -------------------------------------------------------
           (H ((1 B) (2 B) (3 B) (4 W) (5 W) (6 W) (7 U) (8 U) (9 U)))
           (I ((1 B) (2 B) (3 E) (4 W) (5 W) (6 U) (7 U) (8 U) (9 U))))
          `U` - Undefine, `B` - Black, `W` - White
    - ``` (defun get-fields (white-row black-row empty-row col)```
        Pomoćna funkcija za `get-table`, vraća u prvim redu

            (1 U) (2 U) (3 U) (4 U) (5 W) (6 W) (7 E) (8 B) (9 B)
    - ``` (defun print-table (table)```
        Štampa tabelu, pomoćne funkcije `(defun print-row (row)` i `(defun print-field (field)`


***
## Homework 2
