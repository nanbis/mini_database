#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (cons table
          (map list
               columns-name))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (map car
         (cdr table))))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (car (filter (λ (table)
                   (equal? (car table)
                           table-name))
                 db))))

(define add-table
  (λ (db table)
    (cons table
          db)))

(define remove-table
  (λ (db table-name)
    (filter (λ(table)
              (not (equal? (car table)
                           table-name)))
            db)))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db '(("Studenți"
              ("Număr matricol" 123 124 125 126)
              ("Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
              ("Prenume" "Gigel" "Maria" "Ionel" "Ioana")
              ("Grupă" "321CA" "321CB" "321CC" "321CD")
              ("Medie" 9.82 9.91 9.99 9.87))
             ("Cursuri"
              ("Anul" "I" "II" "III" "IV" "I" "III")
              ("Semestru" "I" "II" "I" "I" "II" "II")
              ("Disciplină" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date")
              ("Număr credite" 5 6 5 6 5 5)
              ("Număr teme" 2 3 3 3 3 0))))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================
(define insert
  (λ (db table-name record)
    (map (λ(table)
           (if (equal? table-name
                       (car table))
               (cons (car table)
                     (map (λ(column)
                            (append column
                                    (letrec ([to_add (map (λ(record_pair)
                                                            (cdr record_pair))
                                                          (filter (λ(record_pair)
                                                                    (equal? (car record_pair)
                                                                            (car column)))
                                                                  record))])
                                      (if (null? to_add)
                                          '(null)
                                          to_add))
                                    ))
                          (cdr table)))
               table))
         db)))
;Iteram prin db cu un map. Daca NU aveam de a face cu tabelul cerut, il lasam in pace, altfel
;iteram prin fiecare coloana cu un map. Pentru fiecare coloana, selectam din lista record, doar
;entryurile care corespund coloanei respective, si le appenduiam la sfarsitul coloanei. Daca
;niciun entry nu corsespundea, appenduiam '(null).


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define simple-select
  (λ (db table-name columns)
    (filter (λ(col)
              (not (null? col)))
            (letrec ([tab (get-table db
                                     table-name)])
              (map (λ(column)
                     (letrec ([result(cdr (get-column-by-name tab
                                                              column))])
                       result))
                   columns)))))

;Printr-un map, selectam din db doar tabelul corect. Folosind un alt map,
;selectam din acel tabel doar coloanele cerute.

(define get-column-by-name
  (λ(table col-name)
    (car (filter (λ(column)
                   (equal? (car column) col-name))
                 (cdr table)))))

;Functie ajutatoare folosita pentru a extrage doar
;o coloana anume dintr-un tabel

;====================================
; Functii ajutatoare pentru select, =
; update si delete                  =
;====================================

(define create_aux_table
  (λ(table)
    (cons (car table)
          (map (λ(table)
                 (let loop ([current_el 0]
                            [current_table table])
                   (if (null? current_table)
                       '()
                       (cons (cons current_el
                                   (car current_table))
                             (loop (+ 1 current_el)
                                   (cdr current_table))))))
               (cdr table)))))

;Pentru fiecare coloana dintr-un tabel dat, inlocuiam
;fiecare entry cu o pereche de forma (index_entry . entry).
;Numele coloanei avea index-ul 0.


(define obtain_original_table
  (λ(table)
    (cons (car table)
          (map (λ(column)
                 (map cdr
                      column))
               (cdr table)))))

;Operatia inversa lui create_aux_table

(define extract_by_singe_cond
  (λ(table cond)
    (cons 0
          (map car
               (filter (λ(entry)
                         (if (equal? (cdr entry)
                                     'null)
                             false
                             ((car cond) (cdr entry)
                                         (caddr cond))))
                       (cdr (car (filter (λ(col)
                                           (equal? (cadr cond)
                                                   (cdr (car col))))
                                         (cdr table)))))))))

;extract_by_singe_cond primeste ca parametru o singura conditie si
;un tabel trecut prin create_aux_table. Aceasta functie, se uita in
;coloana corespunzatoare conditie, si pastreaza indexii entryurilor
;care respecta conditia. La acea lista de indexi, pune si 0,
;numele coloanei. 'null nu verifica nicio conditie.

(define extract_by_conditions
  (λ(table conditions)
    (if (null? conditions)
        (map car
             (car (cdr table)))
        (letrec ([results (map (λ(condition)
                                 (extract_by_singe_cond table
                                                        condition))
                               conditions)])
          (foldr intersect
                 (car results)
                 (cdr results))))))

;Apeleaza extract_by_singe_cond pentru fiecare element din conidions.
;Fiecare apel al lui extract_by_singe_cond va returna o lista de indexi.
;Pentru a obtine doar indexii care respecta toate conditiile, folosesc
;functia intersect, care face intersectia a 2 liste de indexi

(define intersect
  (λ(a b)
    (let loop ([first a]
               [acc '()])
      (if (null? first)
          acc
          (if (member (car first)
                      b)
              (loop (cdr first)
                    (cons (car first)
                          acc))
              (loop (cdr first)
                    acc))))))

;O functie inspirata de pe stackoverflow care
;face intersectia a 2 liste.


(define (remove-duplicates-left-iter L res)
  (if (null? L)
      (reverse res)
      (if (member (car L)
                  res)
          (remove-duplicates-left-iter (cdr L)
                                       res)
          (remove-duplicates-left-iter (cdr L)
                                       (cons (car L)
                                             res)))))

;Functie facuta pentru laboratorul 2 care sterge
;duplicatele dintr-o lista.

(define delete_unselected_entries
  (λ(table extracted_index)
    (cons (car table)
          (map (λ(col)
                 (cons (car col)
                       (filter (λ(entry)
                                 (member (car entry)
                                         extracted_index))
                               (cdr col))))
               (cdr table)))))

;Functia primeste ca parametri un tabel si o lista de indexi.
;Aceasta sterge, din fiecare coloana, entry-urile cu indexi
;care nu se afla in lista de indexi.

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================
(define select
  (λ (db table-name columns conditions)
    (filter (λ(col)
              (not (null? col)))
            (letrec ([tab (get-table db
                                     table-name)]
                     [aux_tab (create_aux_table tab)]
                     [extracted_indexes (extract_by_conditions aux_tab
                                                               conditions)]
                     [final_table (obtain_original_table (delete_unselected_entries aux_tab
                                                                                    extracted_indexes))]
                     [simplified_columns (map (λ(col)
                                                (if (pair? col)
                                                    (cdr col)
                                                    col))
                                              columns)]
                     [column_pairs (filter pair?
                                           columns)]
                     [column_pairs_names (map cdr
                                              column_pairs)])
              (map (λ(column)
                     (letrec ([result (cdr (get-column-by-name final_table
                                                               column))])
                       (if (member column
                                   column_pairs_names)
                           (case (car (car (filter (λ(pair)
                                                     (equal? (cdr pair)
                                                             column))
                                                   column_pairs)))
                             ['min (foldr min
                                          (car result)
                                          result)]
                             ['max (foldr max
                                          (car result)
                                          result)]
                             ['count (length (remove-duplicates-left-iter result
                                                                          '()))]
                             ['sum (foldr + 0
                                          result)]
                             ['avg (/ (foldr + 0
                                             result)
                                      (length result))]
                             ['sort-asc (sort result <)]
                             ['sort-desc (sort result >)])
                           result)))
                   simplified_columns)))))

;Functia select pastreaza dintr-un tabel doar entry-urile care respecta toate
;conditiile date. Initial, nu ma leg de perechile de tipul (operatie . 'nume_coloana),
;ci selectez intreaga colona 'nume_coloana. Dupa ce sterg entry-urile care nu respecta
;conditiile, fac un case pentru fiecare operatie si coloana respectiva. Daca este 'min,
;aleg cu un foldr minimul, idem pentru 'max, pentru 'count sterg toate elementele
;duplicate si fac length de lista obtinuta, pentru 'sum fac suma, pentru 'avg fac
;suma pe lungime, iar pentru sortarim apelez sort.

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define update
  (λ (db table-name values conditions)
    (map (λ(table)
           (if (equal? table-name
                       (car table))
               (obtain_original_table (cons table-name
                                            (letrec ([new_table (create_aux_table table)]
                                                     [extracted_index (extract_by_conditions new_table
                                                                                             conditions)])
                                              (map (λ(col)
                                                     (if (member (cdr (car col))
                                                                 (foldr cons
                                                                        '()
                                                                        (map car values)))
                                                         (letrec ([val (cdr (car (filter (λ(value)
                                                                                           (equal? (car value)
                                                                                                   (cdr (car col))))
                                                                                         values)))])
                                                           (cons (car col)
                                                                 (map (λ(entry)
                                                                        (if (member (car entry)
                                                                                    extracted_index)
                                                                            (cons (car entry)
                                                                                  val)
                                                                            entry))
                                                                      (cdr col))))
                                                         col))
                                                   (cdr new_table))))) 
               table))
         db)))

;Folosind functia extract_by_conditions, obtin doar indexii care respecta conditiile. Pentru fiecare coloana,
;verific daca aceasta se afla in values, si, in caz favorabil, ii updatez toate valorile.


;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
(define delete
  (λ (db table-name conditions)
    (map (λ(table)
           (if (equal? table-name
                       (car table))
               (obtain_original_table (cons table-name
                                            (letrec ([new_table (create_aux_table table)]
                                                     [extracted_index (extract_by_conditions new_table
                                                                                             conditions)])
                                              (map (λ(col)
                                                     (cons (car col)
                                                           (filter (λ(entry)
                                                                     (not (member (car entry)
                                                                                  extracted_index)))
                                                                   col)))
                                                   (cdr new_table)))))
               table))
         db)))

;Asemanatoare lui update, pastrez din fiecare coloana doar entry-urile care nu se afla in lista de indexi
;extrafi folosind extract_by_conditions.

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================

;Join-ul l-am facut pe ultima suta de metri si n-am mai apucat sa il comentez. Nu merge la unul dintre teste,
;dar nu stiu de ce.
;Pe scurt, vedeam pe ce coloane se intersectau tabelele, si foloseam extract_by_conditions pentru a gasi
;indexii entry-urilor din tabela 2 care aveau aceleasi valori ca in prima tabela. Pe urma combinam indexii
;din primul tabel cu cei corespunzatori din a doua tabela, si aplicam insert pe ele. Pe rezultat, aplicam
;select cu conditiile date. Am rescris insert-ul deoarece insert-ul original primeste o lista de perechi,
;aici aveam nevoie de o lista de liste. E mult cod duplicat, e o rezolvare foarte ineficienta si gandita
;prost. Dar macar am incercat :D

(define separate_and_number_entries
  (λ(table)
    (let loop([current 1]
              [current_list (apply map list (map (λ(col)
                                                   (map (λ(entry)
                                                          (list (car col) entry)) (cdr col)))
                                                 (cdr table)))])
      (if (null? current_list)
          '()
          (cons (cons current
                      (car current_list))
                (loop (+ current 1)
                      (cdr current_list)))))))

(define combine_entries
  (λ(entries1 entries2 indexes)
    (let loop([first_entries entries1]
              [second_entries indexes])
      (if (or (null? first_entries)(null? second_entries))
          '()
          (if (equal? (length (car second_entries)) 1)
              (loop (cdr first_entries) (cdr second_entries))
              (cons (map (λ(entry)
                           (append (cdr (car first_entries))
                                   (cdr (car (filter (λ(other_entry)
                                                       (equal? entry
                                                               (car other_entry)))
                                                     entries2)))))
                         (cdr (car second_entries)))
                    (loop (cdr first_entries) (cdr second_entries))))))))

(define natural-join
  (λ (db tables columns conditions)
    (letrec ([two_tables (map (λ(table-name)
                                (get-table db
                                           table-name))
                              tables)]
             [first_table (car two_tables)]
             [second_table (car (cdr two_tables))]
             [first_columns (map car (cdr first_table))]
             [second_columns (map car (cdr second_table))]
             [common_columns (intersect first_columns second_columns)]
             [first_table_aux (create_aux_table first_table)]
             [second_table_aux (create_aux_table second_table)]
             [second_table_only_other_col (cons (car second_table) (filter (λ(col)(not (member (car col) common_columns))) (cdr second_table)))]
             [result_table (cons "Placeholder" (map list(remove-duplicates-left-iter (append first_columns second_columns) '())))])
      (select (rec_insert (list result_table) "Placeholder" (foldr append '() (combine_entries (separate_and_number_entries first_table)
                                                                                               (separate_and_number_entries second_table_only_other_col)
                                                                                               (map (λ(cond)
                                                                                                      (extract_by_conditions second_table_aux cond))
                                                                                                    (apply map list (map (λ(column)
                                                                                                                           (map (λ(entry)
                                                                                                                                  (list equal?
                                                                                                                                        (car column)
                                                                                                                                        entry))
                                                                                                                                (filter (λ(entry)
                                                                                                                                          (not(equal?
                                                                                                                                               'null
                                                                                                                                               entry)))
                                                                                                                                        (cdr column))))
                                                                                                                         (filter (λ(col)
                                                                                                                                   (member (car col)
                                                                                                                                           common_columns))
                                                                                                                                 (cdr first_table))))))))
              "Placeholder" columns conditions))))


(define rec_insert
  (λ(db table-name records)
    (let loop ([current records]
               [result db])
      (if (null? current)
          result
          (loop (cdr current) (insert-modified result table-name (car current)))))))

(define insert-modified
  (λ (db table-name record)
    (map (λ(table)
           (if (equal? table-name
                       (car table))
               (cons (car table)
                     (map (λ(column)
                            (append column
                                    (letrec ([to_add (map (λ(record_pair)
                                                            (car (cdr record_pair)))
                                                          (filter (λ(record_pair)
                                                                    (equal? (car record_pair)
                                                                            (car column)))
                                                                  record))])
                                      (if (null? to_add)
                                          '(null)
                                          to_add))
                                    ))
                          (cdr table)))table))
         db)))
