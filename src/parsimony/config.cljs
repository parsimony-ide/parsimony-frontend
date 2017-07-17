(ns parsimony.config)

(goog-define VERSION "^")
(goog-define BUILD "^")
(goog-define measurement-host-1 "localhost:9255")
(goog-define measurement-host-2 "localhost:9256")

(defn host []
  (if (= "dev" BUILD)
    "localhost:9254"
    (-> js/window .-location .-host)))
