(deftask cider "CIDER profile"
   []
   (require 'boot.repl)
   (swap! @(resolve 'boot.repl/*default-dependencies*)
          concat '[[org.clojure/tools.nrepl "0.2.13"]
                   [cider/cider-nrepl "0.26.0"]
                   [refactor-nrepl "2.5.0"]])
   (swap! @(resolve 'boot.repl/*default-middleware*)
          concat '[cider.nrepl/cider-middleware
                   refactor-nrepl.middleware/wrap-refactor])
   identity)
