(load-file "zet-mode.el")

(ert-deftest example ()
  (should (= 2 (+ 1 1))))

(ert-deftest example-not ()
  (should-not (= 1 (+ 1 1))))

(ert-deftest example-error ()
  (should-error (user-error "Uh oh!")))
