;;; ipv4 --- Useful functions to manipulate IPv4 addresses -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Coque Couto

;; Author: Coque Couto <coque.couto at gmail.com>
;; Version: 0.1
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
  "Convert an IPv4 address in string format to an integer.

Return NIL if IP is an invalid IPv4 address."
  (let ((parts (mapcar #'string-to-number (split-string ip "\\." t))))
    (when (and (ipv4--byte-p (nth 0 parts))
               (ipv4--byte-p (nth 1 parts))
               (ipv4--byte-p (nth 2 parts))
               (ipv4--byte-p (nth 3 parts)))
      (+ (ash (nth 0 parts) 24)
         (ash (nth 1 parts) 16)
         (ash (nth 2 parts) 8)
         (nth 3 parts)))))

;;;###autoload
(defun ipv4-int-to-string (ip)
  "Convert an integer to an IPv4 address string."
  (format "%d.%d.%d.%d"
          (logand (ash ip -24) #xFF)
          (logand (ash ip -16) #xFF)
          (logand (ash ip -8) #xFF)
          (logand ip #xFF)))

;;;###autoload
(defun ipv4-valid-p (ip)
  "Return t if the string IP is a valid IPv4 address."
  (when (string-match
         "^\\([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]\\)\\(\\.\\([1-9]?[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]\\)\\)\\{3\\}$"
         ip)
    t))


;; CIDR functions.

;;;###autoload
(defun ipv4-cidr-to-cons (cidr)
  "Convert a CIDR network address \"aaa.bbb.ccc.ddd/xx\" to a
cons with the IP address in the first element and the length of the
network mask in the second element.

Return NIL if CIDR has an invalid format."
  (let ((parts (split-string cidr "/")))
    (when (= 2 (length parts))
      (let ((ip (ipv4-string-to-int (car parts)))
            (mask (string-to-number (cadr parts))))
        (when (and ip (ipv4-integer-mask-p mask))
          (cons ip mask))))))

;;;###autoload
(defun ipv4-cons-to-cidr (cons)
  "Convert a CONS with an IP address in the first element and a network
mask in the second one, both as integers, to a string with format
\"aaa.bbb.ccc.ddd/xx\".

Return NIL if the IPv4 address or the mask are invalid."
  (when (consp cons)
    (let ((ip (car cons))
          (mask (cdr cons)))
      (when (and (integerp ip)
                 (ipv4-integer-mask-p mask))
        (concat (ipv4-int-to-string ip) "/" (number-to-string mask))))))


;; Mask functions.

;;;###autoload
(defun ipv4-integer-mask-p (mask)
  "Return t if MASK is an integer between 0 and 32."
  (when (integerp mask)
    (and (>= mask 0) (<= mask 32))))


;; Useful internal functions.

;;;###autoload
(defun ipv4--byte-p (n)
  "Return t if N is an integer between 0 and 255."
  (and (integerp n)
       (>= n 0)
       (<= n 255)))

(provide 'ipv4)

;;; ipv4 ends here
