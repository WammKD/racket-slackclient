#lang racket
(require "client.rkt" net/rfc6455 net/url)

(define slackClient (new SlackClient% [clientToken (string-append
                                                     "xoxb-304531317809-EGl"
                                                     "F0iUdmhkb0FBPuUrA9USg")]))

(define a (send slackClient rtm-connect #:withTeamStat #f))
(displayln (ws-conn? a))
(displayln (ws-conn-closed? a))
(displayln (ws-recv a))
(displayln (ws-conn? a))
(displayln (ws-conn-closed? a))
(sleep 1)
(displayln (ws-recv a))
