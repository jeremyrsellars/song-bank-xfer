(ns us.sellars.song-transfer.core)

(defmulti xf
  "yields a transducer"
  (fn xf_dispatch [xf-k & _]
    xf-k))
