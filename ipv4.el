;;; ipv4 --- Useful functions to manipulate IPv4 addresses -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Coque Couto

;; Author: Coque Couto <coque.couto at gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs))
;; Keywords: comm lisp tools
;; URL: https://github.com/coquec/ipv4

;; ipv4 is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; ipv4 is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; ipv4.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ipv4 contains some simple functions to manipulate IPv4 addresses and network
;; masks.

;;; Code:


;; IPv4 functions.

;;;###autoload
(defun ipv4-string-to-int (ip)
  "Convert the IPv4 address in the string IP to an integer.

Return NIL if IP is an invalid IPv4 address."
  (when (ipv4-valid ip)
    (let ((parts (mapcar #'string-to-number (split-string ip "\\." t))))
      (+ (ash (nth 0 parts) 24)
         (ash (nth 1 parts) 16)
         (ash (nth 2 parts) 8)
         (nth 3 parts)))))

;;;###autoload
(defun ipv4-int-to-string (ip)
  "Convert an integer IP to an IPv4 address string."
  (format "%d.%d.%d.%d"
          (logand (ash ip -24) #xFF)
          (logand (ash ip -16) #xFF)
          (logand (ash ip -8) #xFF)
          (logand ip #xFF)))

;;;###autoload
(defun ipv4-valid (ip)
  "Return the string IP if it is a valid IPv4 address.

Return nil otherwise."
  (when (string-match
         "^\\([1-9]?[0-9]\\|1[0-9][0-9]\\|2[0-4][0-9]\\|25[0-5]\\)\\(\\.\\([1-9]?[0-9]\\|1[0-9][0-9]\\|2[0-4][0-9]\\|25[0-5]\\)\\)\\{3\\}$"
         ip)
    ip))


;; CIDR functions.

;;;###autoload
(defun ipv4-cidr-to-cons (cidr)
  "Extract network address and mask from a string in CIDR format.

Convert a string with a CIDR network address \"aaa.bbb.ccc.ddd/xx\" to a
cons with the IP address in the first element and the length of the
network mask in the second element.

Return NIL if CIDR has an invalid format."
  (let ((parts (split-string cidr "/")))
    (when (= 2 (length parts))
      (let ((ip (ipv4-string-to-int (car parts)))
            (mask (string-to-number (cadr parts))))
        (when (and ip (ipv4-cidr-mask-p mask))
          (cons ip mask))))))

;;;###autoload
(defun ipv4-cons-to-cidr (cons)
  "Produce a string in CIDR format from a cons with a network and a mask.

Convert a CONS with an IP address in the first element and a network
mask length in the second one, both as integers, to a string with format
\"aaa.bbb.ccc.ddd/xx\".

Return NIL if the IPv4 address or the mask are invalid."
  (when (consp cons)
    (let ((ip (car cons))
          (mask (cdr cons)))
      (when (and (integerp ip) (ipv4-cidr-mask-p mask))
        (concat (ipv4-int-to-string ip) "/" (number-to-string mask))))))

;;;###autoload
(defun ipv4-network (cons)
  "Return the network address in CONS as an integer.

Return nil for network masks of 31 or 32 bits.  For 31 bits, the two
available addresses are for hosts, according to RFC-3021.  A 32 bit mask
denotes a host.

The expected values in CONS are described in `ipv4-cidr-to-cons'."
  (when (< (cdr cons) 31)
    (ipv4-raw-network cons)))

;;;###autoload
(defun ipv4-raw-network (cons)
  "Return the network address in CONS as an integer.

The expected values in CONS are described in `ipv4-cidr-to-cons'.

Do not take any special action for network masks of 31 or 32 bits.

The returned mask can be converted to a string such as \"255.255.255.0\"
with `ipv4-int-to-string'."
  (logand (car cons) (ipv4-cidr-to-mask (cdr cons))))

;;;###autoload
(defun ipv4-broadcast (cons)
  "Return the network broadcast address in CONS as an integer.

The expected values in CONS are described in `ipv4-cidr-to-cons'.

Return nil for network masks of 31 or 32 bits.  For 31 bits, the two
available addresses are for hosts, according to RFC-3021.  A 32 bit mask
denotes a host.

The returned mask can be converted to a string such as \"255.255.255.0\"
with `ipv4-int-to-string'."
  (when (< (cdr cons) 31)
    (ipv4-raw-broadcast cons)))

;;;###autoload
(defun ipv4-raw-broadcast (cons)
  "Return the network broadcast address in CONS as an integer.

The expected values in CONS are described in `ipv4-cidr-to-cons'.

Do not take any special action for network masks of 31 or 32 bits.

The returned mask can be converted to a string such as \"255.255.255.0\"
with `ipv4-int-to-string'."
  (+ (ipv4-raw-network cons)
     (logand #xFFFFFFFF (lognot (ipv4-cidr-to-mask (cdr cons))))))

;;;###autoload
(defun ipv4-host-count (cons)
  "Return the number of hosts in the network in CONS.

The expected values in CONS are described in `ipv4-cidr-to-cons'."
  (cond ((= (cdr cons) 32)
         1)
        ((= (cdr cons) 31)
         2)
        (t
         (- (expt 2 (- 32 (cdr cons))) 2))))

;;;###autoload
(defun ipv4-first-host (cons)
  "Return the IP address of the first host in the network in CONS.

The expected values in CONS are described in `ipv4-cidr-to-cons'."
  (if (>= (cdr cons) 31)
      (ipv4-raw-network cons)
    (+ 1 (ipv4-raw-network cons))))

;;;###autoload
(defun ipv4-last-host (cons)
  "Return the IP address of the last host in the network in CONS.

The expected values in CONS are described in `ipv4-cidr-to-cons'."
  (if (>= (cdr cons) 31)
      (ipv4-raw-broadcast cons)
    (- (ipv4-raw-broadcast cons) 1)))


;; Mask functions.

;;;###autoload
(defun ipv4-cidr-mask-p (cidr)
  "Return t if CIDR is an integer between 0 and 32."
  (and (integerp cidr)
       (>= cidr 0)
       (<= cidr 32)))

;;;###autoload
(defun ipv4-cidr-to-mask (cidr)
  "Convert a mask length CIDR into a integer mask.

Return nil if CIDR is not a valid mask length."
  (when (ipv4-cidr-mask-p cidr)
    (logand (ash -4294967296 (- cidr)) #xFFFFFFFF)))

;;;###autoload
(defun ipv4-mask-p (mask)
  "Return t if MASK is a valid integer mask.

An valid integer mask is a 32 bit integer with all of the 1s (if any) in
its binary representation grouped to the left, with no 0s between them."
  (and (integerp mask)
       (or (= 0 mask)
           (and (>= mask 0)
                (<= mask #xFFFFFFFF)
                (= #x80000000 (logand mask #x80000000) )
                (= 2 (logcount (logxor mask (ash mask 1))))))))

;;;###autoload
(defun ipv4-cidr-to-wildcard (cidr)
  "Return the wildcard corresponfig to a valid CIDR integer mask.

Return nil if CIDR is not a valid mask length."
  (let ((mask (ipv4-cidr-to-mask cidr)))
    (when mask
      (logand #xFFFFFFFF (lognot mask)))))

(provide 'ipv4)

;;; ipv4.el ends here
