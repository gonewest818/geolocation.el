;;; geolocation.el --- obtain your current location

;; Author: Neil Okamoto <neil.okamoto+melpa@gmail.com>
;; Copyright (C) 2020  Neil Okamoto
;; Keywords: hardware
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/gonewest818/geolocation.el
;; Version 0.1.0

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This package obtains your current location by obtaining the MAC
;; addresses of nearby wifi access points and then submitting that
;; list to third-party geolocation APIs.  Those geolocation services
;; use the known locations of the wifi access points and the relative
;; strength of each signal to triangulate your latitude and longitude.

;; WARNING: THIS PACKAGE COLLECTS INFORMATION THAT IMPLIES YOUR
;; PHYSICAL LOCATION.  THAT DATA WILL BE SENT TO THIRD-PARTY
;; GEOLOCATION SERVICES WHICH CAN PINPOINT YOUR LOCATION TYPICALLY
;; WITHIN 100 METERS OR LESS.  WHILE THIS PACKAGE DOES NOT ITSELF
;; STORE YOUR LOCATION, WE CANNOT GUARANTEE WHAT A GIVEN THIRD-PARTY
;; SERVICE MAY DO WITH THE DATA IT RECEIVES IN THESE API TRANSACTIONS.

;;; Code:

(require 'request)
(require 'json)

; todo: defcustom
(setq geolocation-api-vendor :unwiredlabs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macintosh

;; Note: macOS provides a core location framework that is used for
;; Apple's own applications. It is possible to write native code, or
;; possibly AppleScript code to interact with those frameworks, and
;; other packages exist to do that. We've chosen geolocation via wifi
;; because it's more broadly applicable to many operating systems.

(setq geolocation--osx-airport-path
      (concat "/System/Library/PrivateFrameworks/"
              "Apple80211.framework/Versions/Current/Resources/"))

(setq geolocation--osx-airport-command
      (concat geolocation--osx-airport-path
              "airport --scan | cut -c 34-63 | tail -n +2"))

(defun geolocation--osx-scan-wifi ()
  "Run the \"airport\" utility and parse the default output."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)
          (result '()))
      (shell-command geolocation--osx-airport-command t nil)
      (goto-char (point-min))
      (while (not (eobp))
        (push (cl-mapcar (lambda (c k v)
                           (cons k (if (eq c 'int) (string-to-number v) v)))
                         '(str int str)
                         '(bssid signal channel)
                         (split-string (buffer-substring (point) (point-at-eol))))
              result)
        (beginning-of-line 2))
      result)))

;; macOS "airport" utilty also outputs a property list in XML format
;; which in principle should be more reliable to parse, and contains
;; more interesting information than the human readable output.
;; However as of 2/1/2020 the "airport" utility truncates output when
;; devices advertise themselves with names containing unprintable
;; characters. The truncation bug cannot be worked around by the
;; caller.  Hide this implementation until/unless the underlying bug
;; is fixed.

;; (require 'osx-plist)

;; (setq geolocation--osx-airport-command-xml
;;       (concat geolocation--osx-airport-path
;;               "airport --scan --xml"))

;; (defun geolocation--osx-parse-access-point (ap)
;;   "Select relevant fields from the access point record AP.

;; The result is an alist containing at least the following keys:
;;   `bssid'   : BSSID / MAC address of the access point
;;   `signal'  : signal strength or RSSI from that access point
;;   `channel' : channel on which that access point is operating"
;;   (list (cons 'bssid (gethash "BSSID" ap))
;;         (cons 'signal (gethash "RSSI" ap))
;;         (cons 'channel (gethash "CHANNEL" ap))))

;; (defun geolocation--osx-call-airport-xml ()
;;   "Run the \"airport\" utility and parse the xml output."
;;   (with-temp-buffer
;;     (let ((coding-system-for-read 'utf-8))
;;       (shell-command geolocation--osx-airport-command-xml t nil))
;;     (mapcar #'geolocation--osx-parse-access-point (osx-plist-parse-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows

(defun geolocation--windows-scan-wifi ()
  "Wifi scanning on Windows not yet implemented."
  (error "Wifi scanning on Windows not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux

(defun geolocation--linux-scan-wifi ()
  "Wifi scanning on Linux not yet implemented."
  (error "Wifi scanning on Linux not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google geolocation api

(setq geolocation-api-google-url nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unwired Labs geolocation api

(setq geolocation-api-unwiredlabs-url
;;    "https://us1.unwiredlabs.com/v2/process.php" ; Virginia
;;    "https://us2.unwiredlabs.com/v2/process.php" ; San Francisco
;;    "https://eu1.unwiredlabs.com/v2/process.php" ; France
;;    "https://ap1.unwiredlabs.com/v2/process.php" ; Singapore
      "https://us2.unwiredlabs.com/v2/process.php")

(setq geolocation-api-unwiredlabs-token
      (if-let ((pw (car (auth-source-search :host "unwiredlabs.com"))))
          (funcall (plist-get pw :secret))))

(defun geolocation--call-unwiredlabs-api (wifi)
  "Invoke the Unwiredlabs REST API with WIFI data and get position."
  (let ((response
         (request geolocation-api-unwiredlabs-url
           :type "POST"
           :data (json-encode `(("token" . ,geolocation-api-unwiredlabs-token)
                                ("wifi" . ,wifi)))
           :parser #'json-read
           :sync t                      ; todo: make this an async call
           :timeout 15)))
    (when (= 200 (request-response-status-code response))
      (request-response-data response))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(defun geolocation-scan-wifi ()
  "Return a list of nearby wifi access points.
The result is sorted by signal strength with the strongest first.

Each item in the list is itself an alist with the following keys:
  `bssid'   : mac address of the access point
  `signal'  : relative signal strength or rssi
  `channel' : broadcast channel"
  (if-let ((aps (cond
                 ((string-equal system-type "darwin")
                  (geolocation--osx-scan-wifi))
                 ((string-equal system-type "windows-nt")
                  (geolocation--windows-scan-wifi))
                 ((string-equal system-type "gnu/linux")
                  (geolocation--linux-scan-wifi)))))
      (sort aps (lambda (x y)
                  (> (alist-get 'signal x)
                     (alist-get 'signal y))))))

(defun geolocation-get-position ()
  "Return our current latitude and longitude.

The reply is an alist with at least the following keys:
  `lat'      : the current location's latitude
  `lon'      : the current location's longitude
  `accuracy' : accuracy of the estimate in meters

Other keys may be included in the result, but are not guaranteed
to be supported going forward."
  (if-let ((wifi (geolocation-scan-wifi)))
      (cond ((eq :unwiredlabs geolocation-api-vendor)
             (geolocation--call-unwiredlabs-api wifi)))))

(provide 'geolocation)
;;; geolocation.el ends here
