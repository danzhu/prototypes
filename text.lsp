(div
 class:container
 (: style
    background:black
    color:white)
 (p "hello world")
 (p "time: "t" secs"))

(define (twitch/video-comments id)
  '(101 102))
(define (twitch/comment id)
  '((commenter u11)))
(define (twitch/user id))

(define (mpv/filename ipc-path)
  "foobar")
(define (mpv/playback-time ipc-path)
  1.2)

(define follows '(u11))

(define (mpv-chat/chat ipc-path follows)
  (let* ([video-id (mpv/filename ipc-path)]
         [current-time (mpv/playback-time ipc-path)]
         [comments (twitch/video-comments video-id)])
    (define (late id)
      (> (.time (twitch/comment id)) current-time))
    (map (lambda (id) (mpv-chat/comment id follows))
         (take 500 (drop-while late comments)))))
(define (mpv-chat/comment id follows)
  (let* ([comment (twitch/comment id)]
         [user (twitch/user (.commenter comment))])
    `(div
      (p ,(.display-name user))
      (p ,(.message comment)))))

;; client <-> server

;; -> provide fun-id
;; -> unprovide fun-id
;; <- observe sub-id one-shot? fun-id args
;; <- unobserve sub-id
;; -> update sub-id state

;; -> observe sub-id one-shot? expr
;; -> unobserve sub-id
;; <- update sub-id state

(data val
      value:(or string (cons val val))
      deps:(set target))

;; purposes
;; - plugin / code reuse: each data source can be consumed by anyone
;; - reflection: each data source / intermediate result can be inspected
;; - hot reload / offline cache: providers can stop/restart while conusmers continue to use cached data
;; usages
;; - mpv twitch chat
;; - osu! song symlinks
;; ideas
;; - attach "loading"/"error" status (and other metadata e.g. modified time) to state
;; - force refresh ignoring cache of not-always-up-to-date (polling) targets
;; questions
;; - continuous data e.g. time, network: provider polls and pushes, or server pulls on interval
;; - where to cache: hub implicitly handles cache storage, or explicit storage provider
