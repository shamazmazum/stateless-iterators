(defpackage stateless-iterators
  (:use #:cl)
  (:shadow #:nth #:every #:some)
  (:local-nicknames (#:sera #:serapeum))
  (:export #:iterator
           #:stop
           #:iterate-for-effects
           #:do-iterator
           #:collect
           #:consume-one
           #:nth

           #:list->iterator
           #:vector->iterator

           #:+empty+
           #:singleton
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
           #:filter
           #:power
           #:indices
           #:every
           #:some))
