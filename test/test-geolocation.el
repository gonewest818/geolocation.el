;;; test-geolocation.el --- unit tests   -*- lexical-binding: t -*-

(load-file "test/undercover-init.el")
(require 'geolocation)
(require 'buttercup)

(describe "the placeholder test"
  (it "always succeeds"
    (expect t :to-be t)))

;; (describe "the osx-plist-parse-file function"
;;   :var ((good-file "test/data/airport.plist")
;;         (bad-file "test/data/malformed.plist"))
;;   (it "reads and parses a file"
;;     (let* ((p (osx-plist-parse-file good-file))
;;            (w (aref p 0)))
;;       (expect (length p) :to-be 5)
;;       (expect (gethash "BEACON_INT" w) :to-be 100)
;;       (expect (gethash "BSSID" w) :to-match "^ff:ff:aa:c4:5c:73$")
;;       (expect (gethash "IE" w) :to-match "^Lorem ipsum dolor sit amet$")
;;       (expect (gethash "TEST_DATE" w) :to-equal '(45 23 1 7 6 2019 5 nil 0))
;;       (expect (type-of (gethash "RATES" w)) :to-be 'vector)
;;       (expect (type-of (gethash "HT_IE" w)) :to-be 'hash-table)))
;;   (it "returns nil when xml is malformed"
;;     (let* ((p (osx-plist-parse-file bad-file)))
;;       (expect p :to-be nil))))

;;; test-geolocation.el ends here
