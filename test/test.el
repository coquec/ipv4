;;; -*- lexical-binding: t; -*-

(require 'ert)


;; Tests for IPv4 addresses.
(ert-deftest test-ipv4-valid-strings-to-ints ()
  (should (equal 0 (ipv4-string-to-int "0.0.0.0")))
  (should (equal #x0A8C1CFD (ipv4-string-to-int "10.140.28.253")))
  (should (equal #xC0A88001 (ipv4-string-to-int "192.168.128.1")))
  (should (equal #xFFFFFFFF (ipv4-string-to-int "255.255.255.255"))))

(ert-deftest test-ipv4-invalid-strings-to-ints ()
  (should-not (ipv4-string-to-int "0.0.0."))
  (should-not (ipv4-string-to-int "0.0.0.256"))
  (should-not (ipv4-string-to-int "0.0.256.0"))
  (should-not (ipv4-string-to-int "0.-1.0.0"))
  (should-not (ipv4-string-to-int "-1.0.0.0"))
  (should-not (ipv4-string-to-int "-1.0.0.0/32")))

(ert-deftest test-ipv4-valid-ints-to-strings ()
  (should (equal "0.0.0.0" (ipv4-int-to-string 0)))
  (should (equal "192.168.128.1" (ipv4-int-to-string #xC0A88001)))
  (should (equal  "255.255.255.255" (ipv4-int-to-string #xFFFFFFFF))))


;; Tests for CIDR functions.
(ert-deftest test-ipv4-valid-cidrs-to-cons ()
  (should (equal '(0 . 0) (ipv4-cidr-to-cons "0.0.0.0/0")))
  (should (equal '(#x0A000000 . 8) (ipv4-cidr-to-cons "10.0.0.0/8")))
  (should (equal '(#xAC100000 . 12) (ipv4-cidr-to-cons "172.16.0.0/12")))
  (should (equal '(#xC0A80000 . 16) (ipv4-cidr-to-cons "192.168.0.0/16")))
  (should (equal '(#xC0A80101 . 32) (ipv4-cidr-to-cons "192.168.1.1/32"))))

(ert-deftest test-ipv4-invalid-cidrs-to-cons ()
  (should-not (ipv4-cidr-to-cons "0.0.0.0"))
  (should-not (ipv4-cidr-to-cons "10.0.0.0/8/"))
  (should-not (ipv4-cidr-to-cons "172.16.0./12"))
  (should-not (ipv4-cidr-to-cons "0/1"))
  (should-not (ipv4-cidr-to-cons "192.168.1.1/33")))

(ert-deftest test-ipv4-valid-cons-to-cidrs ()
  (should (equal "0.0.0.0/0" (ipv4-cons-to-cidr '(0 . 0))))
  (should (equal "10.0.0.0/8" (ipv4-cons-to-cidr '(#x0A000000 . 8))))
  (should (equal "172.16.0.0/12" (ipv4-cons-to-cidr '(#xAC100000 . 12))))
  (should (equal "192.168.0.0/16" (ipv4-cons-to-cidr '(#xC0A80000 . 16))))
  (should (equal "192.168.1.1/32" (ipv4-cons-to-cidr '(#xC0A80101 . 32)))))

(ert-deftest test-ipv4-invalid-cons-to-cidrs ()
  (should-not (ipv4-cons-to-cidr nil))
  (should-not (ipv4-cons-to-cidr '(0 . "")))
  (should-not (ipv4-cons-to-cidr '(nil . 2)))
  (should-not (ipv4-cons-to-cidr '(0 . nil)))
  (should-not (ipv4-cons-to-cidr '(0 . -1)))
  (should-not (ipv4-cons-to-cidr '(0 . 33))))


;; Tests for mask functions.
(ert-deftest test-ipv4-valid-integer-masks ()
  (should (ipv4-integer-mask-p 0))
  (should (ipv4-integer-mask-p 32)))

(ert-deftest test-ipv4-invalid-integer-masks ()
  (should-not (ipv4-integer-mask-p nil))
  (should-not (ipv4-integer-mask-p -1))
  (should-not (ipv4-integer-mask-p 33)))
