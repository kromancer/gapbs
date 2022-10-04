(setq kDamp 0.85)

(setq num-nodes 4)

(setq init-score (/ 1.0 num-nodes))

(setq base-score (/ (- 1 kDamp) num-nodes))

(setq in-neighs [(1) (0 3) (0 1 3) (2)])

(setq out-degree [0 0 0 0])
(dotimes (v num-nodes)
  (dolist (v (aref in-neighs v))
    (aset out-degree v (+ (aref out-degree v) 1))))

(setq scores [0 0 0 0])
(fillarray scores init-score)

(setq out-contrib (copy-sequence scores))
(dotimes (v num-nodes)
  (aset out-contrib v (/ (aref out-contrib v) (aref out-degree v))))

(setq err 0.0)
(dotimes (u num-nodes)
  (let* ((incoming-total (seq-reduce
			  (lambda (acc x) (+ acc (aref out-contrib x)))
			  (aref in-neighs u) 0.0))
	 (old-score (aref scores u))
	 (new-score (+ base-score (* kDamp incoming-total))))
    (progn
      (aset scores u new-score)
      (aset out-contrib u (/ new-score (aref out-degree u)))
      (setq err (+ err (abs (- new-score old-score)))))))
