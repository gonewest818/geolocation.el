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

(setq geolocation-osx-airport-command
      (concat  "/System/Library/PrivateFrameworks/"
               "Apple80211.framework/Versions/Current/Resources/"
               "airport --scan --xml"
               "| cat -v"))

(defun geolocation--osx-call-airport ()
  "Run the \"airport\" utility and get xml output."
  (with-temp-buffer
    (set-buffer-file-coding-system 'no-conversion) ; necessary?
    (shell-command geolocation-osx-airport-command t nil)
    (osx-plist-parse-buffer)))

(defun geolocation--osx-parse-access-point (ap)
  "Parse the access point entry AP."
  (list (gethash "BSSID" ap)
        (gethash "RSSI" ap)
        (gethash "CHANNEL" ap)
        (gethash "SSID_STR" ap)))

(defun geolocation--get-wifi-access-points ()
  "Return a list of nearby wifi access points."
  (if-let ((aps (geolocation--osx-call-airport)))
      (let ((parsed (mapcar #'geolocation--osx-parse-access-point aps)))
        (sort parsed (lambda (x y) (> (cadr x) (cadr y)))))))

(provide 'geolocation)
;;; geolocation.el ends here
