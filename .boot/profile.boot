(require 'boot.repl)
(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.25.0"]
                [refactor-nrepl "2.5.0"]])

(swap! boot.repl/*default-middleware*
       concat '[cider.nrepl/cider-middleware
                refactor-nrepl.middleware/wrap-refactor])
