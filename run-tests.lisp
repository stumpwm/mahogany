
(ql:quickload "mahogany" :quiet t)
(if (asdf:test-system :mahogany)
  (uiop:quit 0)
  (uiop:quit 1))

