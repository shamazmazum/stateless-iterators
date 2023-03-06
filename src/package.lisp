(defpackage stateless-iterators
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum))
  (:export #:iterator
           #:stop
           #:iterate-for-effects
           #:do-iterator
           #:collect

           #:list->iterator
           #:vector->iterator

           #:count-from
           #:range
           #:take
           #:take-while
           #:drop-while
           #:imap
           #:zip
           #:zip*
           #:enumerate
           #:repeat
           #:replicate
           #:foldl
           #:iterate
           #:cycle
           #:concat
           #:product
           #:filter))
