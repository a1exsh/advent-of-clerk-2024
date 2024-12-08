(ns user
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.index :as index]))

(comment
  (clerk/serve! {:watch-paths ["src"] :port 8024 :browse? true})
  (nextjournal.clerk/show! 'nextjournal.clerk.tap)

  (clerk/clear-cache!)
  (clerk/halt!)

  (clerk/build! {:paths (index/build-paths) :browse? true}))
