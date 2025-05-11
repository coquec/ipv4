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
  (should-not (ipv4-string-to-int "1.0.0.0/32")))

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


;; Tests for network functions.
(ert-deftest test-ipv4-network ()
  (should (equal 0 (ipv4-network '(0 . 0))))
  (should (equal #x0A000000 (ipv4-network '(#x0A5D5D5D . 8))))
  (should (equal #xAC100000 (ipv4-network '(#xAC10FFFF . 12))))
  (should (equal #xC0A80000 (ipv4-network '(#xC0A85D5D . 16))))
  (should (equal nil (ipv4-network '(#xC0A80101 . 31))))
  (should (equal nil (ipv4-network '(#xC0A80101 . 32)))))

(ert-deftest test-ipv4-broadcast ()
  (should (equal #xFFFFFFFF (ipv4-broadcast '(0 . 0))))
  (should (equal #x0AFFFFFF (ipv4-broadcast '(#x0A5D5D5D . 8))))
  (should (equal #xAC1FFFFF (ipv4-broadcast '(#xAC105D5D . 12))))
  (should (equal #xC0A8FFFF (ipv4-broadcast '(#xC0A85D5D . 16))))
  (should (equal nil (ipv4-broadcast '(#xC0A80101 . 31))))
  (should (equal nil (ipv4-broadcast '(#xC0A80101 . 32)))))

(ert-deftest test-ipv4-host-count ()
  (should (equal #xFFFFFFFE (ipv4-host-count '(0 . 0))))
  (should (equal #xFFFFFE (ipv4-host-count '(#x0A5D5D5D . 8))))
  (should (equal #xFFFFE (ipv4-host-count '(#xAC10FFFF . 12))))
  (should (equal #xFFFE (ipv4-host-count '(#xC0A85D5D . 16))))
  (should (equal 2 (ipv4-host-count '(#xC0A80101 . 31))))
  (should (equal 1 (ipv4-host-count '(#xC0A80101 . 32)))))

(ert-deftest test-ipv4-first-host ()
  (should (equal 1 (ipv4-first-host '(0 . 0))))
  (should (equal #x0A000001 (ipv4-first-host '(#x0A5D5D5D . 8))))
  (should (equal #xAC100001 (ipv4-first-host '(#xAC10FFFF . 12))))
  (should (equal #xC0A80001 (ipv4-first-host '(#xC0A85D5D . 16))))
  (should (equal #xC0A80100 (ipv4-first-host '(#xC0A80101 . 31))))
  (should (equal #xC0A80101 (ipv4-first-host '(#xC0A80101 . 32)))))

(ert-deftest test-ipv4-last-host ()
  (should (equal #xFFFFFFFE (ipv4-last-host '(0 . 0))))
  (should (equal #x0AFFFFFE (ipv4-last-host '(#x0A5D5D5D . 8))))
  (should (equal #xAC1FFFFE (ipv4-last-host '(#xAC10FFFF . 12))))
  (should (equal #xC0A8FFFE (ipv4-last-host '(#xC0A85D5D . 16))))
  (should (equal #xC0A80101 (ipv4-last-host '(#xC0A80101 . 31))))
  (should (equal #xC0A80101 (ipv4-last-host '(#xC0A80101 . 32)))))


;; Tests for mask functions.
(ert-deftest test-ipv4-valid-cidr-masks ()
  (should (ipv4-cidr-mask-p 0))
  (should (ipv4-cidr-mask-p 32)))

(ert-deftest test-ipv4-invalid-cidr-masks ()
  (should-not (ipv4-cidr-mask-p nil))
  (should-not (ipv4-cidr-mask-p -1))
  (should-not (ipv4-cidr-mask-p 33)))

(ert-deftest test-ipv4-valid-cird-to-mask ()
  (should (equal 0 (ipv4-cidr-to-mask 0)))
  (should (equal #x80000000 (ipv4-cidr-to-mask 1)))
  (should (equal #xC0000000 (ipv4-cidr-to-mask 2)))
  (should (equal #xFFFFFFFE (ipv4-cidr-to-mask 31)))
  (should (equal #xFFFFFFFF (ipv4-cidr-to-mask 32))))

(ert-deftest test-ipv4-invalid-cird-to-mask ()
  (should-not (ipv4-cidr-to-mask -1))
  (should-not (ipv4-cidr-to-mask 33))
  (should-not (ipv4-cidr-to-mask "255.255.255.0")))

(ert-deftest test-ipv4-valid-mask ()
  (should (ipv4-mask-p 0))
  (should (ipv4-mask-p #x80000000))
  (should (ipv4-mask-p #xE0000000))
  (should (ipv4-mask-p #xFE000000))
  (should (ipv4-mask-p #xFFFF0000))
  (should (ipv4-mask-p #xFFFFFFFE))
  (should (ipv4-mask-p #xFFFFFFFF)))

(ert-deftest test-ipv4-invalid-mask ()
  (should-not (ipv4-mask-p -1))
  (should-not (ipv4-mask-p 1))
  (should-not (ipv4-mask-p #xFFFFEFFF))
  (should-not (ipv4-mask-p #xFFFFEFFF))
  (should-not (ipv4-mask-p #xFFFFCFFF))
  (should-not (ipv4-mask-p #x1FFFFFFFF))
  (should-not (ipv4-mask-p "255.255.255.0")))

(ert-deftest test-ipv4-valid-wildcard ()
  (should (equal #xFFFFFFFF (ipv4-cidr-to-wildcard 0)))
  (should (equal #xFFFFFF (ipv4-cidr-to-wildcard 8)))
  (should (equal #xFFFFF (ipv4-cidr-to-wildcard 12)))
  (should (equal #xFFFF (ipv4-cidr-to-wildcard 16)))
  (should (equal 1 (ipv4-cidr-to-wildcard 31)))
  (should (equal 0 (ipv4-cidr-to-wildcard 32))))

(ert-deftest test-ipv4-invalid-wildcard ()
  (should-not (ipv4-cidr-to-wildcard -1))
  (should-not (ipv4-cidr-to-wildcard 33)))
