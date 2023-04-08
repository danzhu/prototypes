#!/usr/bin/env racket

#lang racket/base

(require data/heap)
(require db)
(require racket/dict)
(require racket/format)
(require racket/match)
(require racket/set)

(define conn (sqlite3-connect #:database "edb.db"))
(query-exec conn "PRAGMA foreign_keys = on")

(define (build id)
  (eprintf "building ~a {~n" id)
  (define expr (read (open-input-string (query-value conn "SELECT expr FROM target WHERE id = ?" id))))
  (define deps (mutable-set))
  (define (deref! ref)
    (set-add! deps ref)
    (deref ref))
  (define res
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-set-variable-value! 'want want)
      (namespace-set-variable-value! '$ deref!)
      (eval expr)))
  (query-exec conn "UPDATE target SET result = ?, outdated = 0 WHERE id = ?"
              res id)
  (for ([dep deps])
    (query-exec conn "INSERT INTO depend(dependent, dependency) VALUES(?, ?)"
                id dep))
  (eprintf "} built ~a~n" id))

(define (rebuild)
  (define id (query-maybe-value conn "SELECT id FROM target WHERE outdated = 1 LIMIT 1"))
  (when id
    (query-exec conn "DELETE FROM depend WHERE dependent = ?" id)
    (build id)
    (rebuild)))

(define (queue-rebuild ids)
  (for ([id ids])
    (query-exec conn "
WITH RECURSIVE
  queue(id) AS (
    SELECT ?
    UNION
    SELECT dependent
    FROM queue
    JOIN depend ON queue.id = dependency
  )
UPDATE target SET outdated = 1
FROM queue
WHERE target.id = queue.id
" id)))

(define (want expr)
  (eprintf "want ~a~n" expr)
  (define expr-str (~s expr))
  (match (query-maybe-row conn "SELECT id, outdated FROM target WHERE expr = ?" expr-str)
    [#f
     (eprintf "  not found~n")
     (define result
       (query conn "INSERT INTO target(expr) VALUES(?)"
              expr-str))
     (define id (dict-ref (simple-result-info result) 'insert-id))
     ;; TODO: schedule build
     ;; (build id)
     id]
    [(vector id 1)
     (eprintf "  outdated~n")
     ;; TODO: schedule build?
     ;; (build id)
     id]
    [(vector id 0)
     (eprintf "  up to date~n")
     id]))

(define (deref ref)
  (eprintf "deref ~a~n" ref)
  (when (= 1 (query-value conn "SELECT outdated FROM target WHERE id = ?" ref))
    (eprintf "  outdated~n")
    ;; TODO: wait for scheduled build
    (build ref))
  ;; separate query, to wait for build to finish before reading result
  (read (open-input-string (query-value conn "SELECT result FROM target WHERE id = ?" ref))))

(define (print-table result)
  (println (map (lambda (h) (dict-ref h 'name))
                (rows-result-headers result)))
  (for ([row (rows-result-rows result)])
    (println row))
  (newline))

(call-with-transaction
 conn
 (lambda ()
   (define a (want '(current-seconds)))
   (define b (want `(+ 20 ($ ,a))))
   (queue-rebuild (list a))
   (rebuild)
   (printf "~a~n" (deref b))
   (print-table (query conn "SELECT * FROM target"))
   (print-table (query conn "SELECT * FROM depend"))
   ))

(disconnect conn)
