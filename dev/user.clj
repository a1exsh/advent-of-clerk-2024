(ns user
  (:require [nextjournal.clerk :as clerk]
            [advent-of-clerk.index :as index]))

(comment
  (clerk/serve! {:watch-paths ["src"] :port 8024 :browse? true})

  (clerk/build! {:paths (index/build-paths) :browse? true}))
