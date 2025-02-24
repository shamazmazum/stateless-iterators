(defpackage stateless-iterators
  (:use #:cl)
  (:shadow #:nth #:every #:some #:find-if #:length)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria))
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
           #:foldr
           #:iterate
           #:unfold
           #:cycle
           #:concat
           #:product
           #:filter
           #:power
           #:indices
           #:every
           #:some
           #:find-if
           #:length
           #:undefined
           #:undefined-value))
