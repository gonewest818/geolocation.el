;;; geolocation.el --- obtain your current location

;; Author: Neil Okamoto <neil.okamoto+melpa@gmail.com>
;; Copyright (C) 2020  Neil Okamoto
;; Keywords: mapping
;; Package-Requires: ((osx-plist "2.0.0") (emacs "25.1"))
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

;; This package obtains your current location by submitting the MAC
;; addresses of nearby wifi access points to third-party geolocation
;; APIs.  Those services will return your latitude and longitude based
;; on the recorded positions of those access points.

;; WARNING: THIS PACKAGE SENDS INFORMATION THAT COORELLATES TO YOUR
;; LOCATION WILL BE EXPOSED TO THOSE THIRD PARTY GEOLOCATION SERVICES.
;; IF YOU AREN'T COMFORTABLE WITH THAT FOR ANY REASON THEN DON'T USE
;; THIS PACKAGE.

;;; Code:

(require 'osx-plist)
(require 'request)
(require 'json)

(setq geolocation-osx-airport-command
      (concat  "/System/Library/PrivateFrameworks/"
               "Apple80211.framework/Versions/Current/Resources/"
               "airport --scan --xml"
               "| cat -v"))

(setq geolocation-api-unwiredlabs-url
      "https://us2.unwiredlabs.com/v2/process.php")

(setq geolocation-api-unwiredlabs-token
      (if-let ((pw (car (auth-source-search :host "unwiredlabs.com"))))
          (funcall (plist-get pw :secret))))

(setq geolocation-api-google-url nil)

(setq geolocation-api-vendor :unwiredlabs)

(defun geolocation--osx-call-airport ()
  "Run the \"airport\" utility and get xml output."
  (with-temp-buffer
    ;; DEBUGING: with-current-buffer (get-buffer-create "airport.plist")
    (let ((coding-system-for-read 'utf-8))
      (shell-command geolocation-osx-airport-command t nil))
    (osx-plist-parse-buffer)))

(defun geolocation--osx-parse-access-point (ap)
  "Select relevant fields from the access point record AP.

The result is an alist containing at least the following keys:
  `bssid'   : BSSID / MAC address of the  access point
  `signal'  : signal strength or RSSI from that access point
  `channel' : channel on which that access point is operating"
  (list (cons 'bssid (gethash "BSSID" ap))
        (cons 'signal (gethash "RSSI" ap))
        (cons 'channel (gethash "CHANNEL" ap))))

(defun geolocation-get-wifi-access-points ()
  "Return a list of nearby wifi access points.

The result is sorted by signal strength with the strongest first."
  (if-let ((aps (geolocation--osx-call-airport)))
      (let ((parsed (mapcar #'geolocation--osx-parse-access-point aps)))
        (sort parsed (lambda (x y)
                       (> (alist-get 'signal x)
                          (alist-get 'signal y)))))))

(defun geolocation--call-unwiredlabs-api (wifi)
  "Invoke the Unwiredlabs REST API with WIFI data and get position."
  (let ((response
         (request geolocation-api-unwiredlabs-url
           :type "POST"
           :data (json-encode `(("token" . ,geolocation-api-unwiredlabs-token)
                                ("wifi" . ,wifi)))
           :parser #'json-read
           :sync t
           :timeout 15)))
    (when (= 200 (request-response-status-code response))
      (request-response-data response))))

(defun geolocation-get-position ()
  "Return our current latitude and longitude.

The reply is an alist with at least the following keys:
  `lat'      : the current location's latitude
  `lon'      : the current location's longitude
  `accuracy' : accuracy of the estimate in meters

Other keys may be included in the result, but are not guaranteed
to be supported going forward."
  (if-let ((wifi (geolocation-get-wifi-access-points)))
      (geolocation--call-unwiredlabs-api wifi)))

(provide 'geolocation)
;;; geolocation.el ends here
