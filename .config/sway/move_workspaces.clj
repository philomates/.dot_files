#!/bin/bb
(ns move-workspaces
  (:require [cheshire.core :as json]
            [babashka.tasks :as tasks :refer [shell]]))

(def direction
  (if (= "left" (first *command-line-args*))
    :left
    :right))

(def workspaces
  (->> (shell {:out :string} "swaymsg -t get_workspaces")
       :out
       json/parse-string
       (map (juxt #(get % "num")
                  #(get % "focused")))))

(def next-workspace
  (->> (cycle (case direction
            :left (reverse workspaces)
            :right workspaces))
       (drop-while (comp not second))
       rest
       first
       first))

(shell (str "swaymsg workspace " next-workspace))
