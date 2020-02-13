;;; geolocation.el --- Get your location on Earth -*- lexical-binding: t -*-

;; Author: Neil Okamoto <neil.okamoto+melpa@gmail.com>
;; Copyright (C) 2020  Neil Okamoto
;; Keywords: hardware
;; Package-Requires: ((request "0.3.1") (emacs "25.1"))
;; URL: https://github.com/gonewest818/geolocation.el
;; Version: 0.1.0

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

;; The main entry points are:

;; - `geolocation-get-position' which returns your estimated position as
;;   an alist with the following keys:
;;   - `lat' - latitude of the current position
;;   - `lon' - longitude of the current position
;;   - `accuracy' - an error radius, in meters

;; - `geolocation-scan-wifi' which scans for nearby wifi access points
;;   using available system utilites, and produces a complete list of
;;   everything in range sorted by signal strength.
;;   Returns a list of alists containing:
;;   - `bssid' - mac address that uniquely identifies the AP
;;   - `signal' - relative signal strength, or RSSI
;;   - `channel' - transmission channel
;;   At present, wifi scanning is supported on Mac OSX and Windows.
;;   Linux support is planned but not yet implemented.

;; You have a choice of third party services to use for the positioning:

;; - Google Maps Geolocation API
;;   https://developers.google.com/maps/documentation/geolocation/intro

;; - Unwired Labs Location API
;;   https://unwiredlabs.com/home

;; This package offers a set of customizable variables you can use to
;; select which service is used, declare your API access token, choose
;; the nearest API endpoint, and so on.  Those settings can be found in
;; `M-x customize`, then browse for "Environment", then "hardware", and
;; then "Geolocation".

;; By default, access tokens are expected to be accessed via the built-in
;; `auth-source` package.  You can configure any backend, as this package
;; only requires the token to be searchable via
;; `auth-source-pick-first-password`.

;; Rate Limits and Costs:

;; Each of these services requires you to create your own account, and
;; potentially provide billing information as well.  You will be agreeing
;; to the vendor's end-user agreements including such things as
;; acceptable usage policies and privacy terms, when you create your
;; account.

;; Care has been taken to choose services that include either a free
;; tier, or equivalently, an allowance of a certain amount of usage per
;; day or per month.  It's your responsibility to set up your account
;; with those services, obtain and protect your token, and manage your
;; usage of those APIs because there can be associated costs if your
;; usage exceeds the usage terms of the subscription or plan you choose.

;; WARNING: THIS PACKAGE COLLECTS INFORMATION THAT IMPLIES YOUR PHYSICAL
;; LOCATION.  THAT DATA WILL BE SENT TO THIRD-PARTY GEOLOCATION SERVICES
;; WHICH CAN PINPOINT YOUR LOCATION TYPICALLY WITHIN 100 METERS OR LESS.
;; WHILE THIS PACKAGE DOES NOT ITSELF STORE YOUR LOCATION, WE CANNOT
;; GUARANTEE WHAT A GIVEN THIRD-PARTY SERVICE MAY DO WITH THE DATA IT
;; RECEIVES IN THESE API TRANSACTIONS.  RATHER, YOU UNDERSTAND AND AGREE
;; TO THE PRIVACY AGREEMENTS AND END USER AGREEMENTS OF EACH OF THE
;; INDIVIDUAL GEOLOCATION APIS YOU USE.

;;; Code:

(require 'json)
(require 'request)
(require 'subr-x)

(defgroup geolocation nil
  "Get your location on Earth"
  :prefix "geolocation-"
  :group 'hardware
  :link '(url-link
          :tag "Github" "https://github.com/gonewest818/geolocation.el"))

(defcustom geolocation-api-vendor :google
  "Select which third party geolocation API will be called."
  :type '(radio (const :tag "Google Maps Geolocation API" :google)
                (const :tag "Unwired Labs Location API" :unwiredlabs))
  :group 'geolocation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macintosh

;; Note: OSX provides a core location framework that is used for
;; Apple's own applications. It is possible to write native code, or
;; possibly AppleScript code to interact with those frameworks, and
;; other packages exist to do that. We've chosen geolocation via wifi
;; because it's more broadly applicable to many operating systems.

(defgroup geolocation-system-osx nil
  "Mac OSX specific settings for the geolocation library"
  :prefix "geolocation-system-osx-"
  :group 'geolocation)

(defcustom geolocation-system-osx-airport-path
  (concat "/System/Library/PrivateFrameworks/"
          "Apple80211.framework/Versions/Current/Resources/")
  "Path to the Apple 'airport' binary on Mac OSX.

We need to specify this because the utility is not in a standard
location.  It's unlikely users need to change this.  However
we're exposing the setting as a customization in case Apple
changes the location."
  :type '(string)
  :group 'geolocation-system-osx)

(defcustom geolocation-system-osx-airport-command
  "airport --scan | cut -c 34-63 | tail -n +2"
  "Command line with arguments needed to invoke airport."
  :type '(string)
  :group 'geolocation-system-osx)

(defun geolocation--osx-scan-wifi ()
  "Run the \"airport\" utility and parse the default output.

Return a list of alists.  Each alist will contain these keys:
`bssid', `signal' and `channel'."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)
          (result '()))
      (shell-command (concat geolocation-system-osx-airport-path
                             geolocation-system-osx-airport-command) t nil)
      (goto-char (point-min))
      (while (not (eobp))
        (push (cl-mapcar (lambda (c k v)
                           (cons k (if (eq c 'int) (string-to-number v) v)))
                         '(str int str)
                         '(bssid signal channel)
                         (split-string (buffer-substring (point)
                                                         (point-at-eol))))
              result)
        (beginning-of-line 2))
      result)))

;; The OSX "airport" utilty also outputs a property list in XML format
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

(defgroup geolocation-system-windows nil
  "Windows-specific settings for the geolocation library"
  :prefix "geolocation-system-windows-"
  :group 'geolocation)

(defcustom geolocation-system-windows-netsh-command
  "netsh wlan show networks mode=bssid"
  "Command line with arguments needed to invoke netsh."
  :type '(string)
  :group 'geolocation-system-windows)

(defun geolocation--windows-bssid ()
  "Search via regexp for the next BSSID."
  (when (re-search-forward
         "\\s-*BSSID\\s-[0-9]+\\s-*:\\s-\\([a-f0-9:]+\\)" nil t)
    (match-string-no-properties 1)))

(defun geolocation--windows-signal ()
  "Search via regexp for the next Signal value."
  (when (re-search-forward "\\s-*Signal\\s-*:\\s-\\([0-9:]+\\)" nil t)
    (let ((sig (match-string-no-properties 1)))
      (- (/ (string-to-number sig) 2.0) 100))))

(defun geolocation--windows-channel ()
  "Search via regexp for the next Channel number."
  (when (re-search-forward "\\s-*Channel\\s-*:\\s-\\([0-9:]+\\)" nil t)
    (match-string-no-properties 1)))

(defun geolocation--windows-scan-wifi ()
  "Run \"netsh wlan show networks\" and parse the output.

Return a list of alists.  Each alist will contain these keys:
`bssid', `signal' and `channel'."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)
          (result '()))
      (shell-command geolocation-system-windows-netsh-command t nil)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((bssid (geolocation--windows-bssid))
               (sig (geolocation--windows-signal))
               (chan (geolocation--windows-channel)))
          (if (and bssid sig chan)
              (push (list (cons 'bssid bssid)
                          (cons 'signal sig)
                          (cons 'channel chan))
                    result)
            (goto-char (point-max)))))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux

(defgroup geolocation-system-linux nil
  "Linux-specific settings for the geolocation library"
  :prefix "geolocation-system-linux-"
  :group 'geolocation)

(defcustom geolocation-system-linux-dbus-command
  "echo not implemented"
  "Command line with arguments needed to query dbus to scan wifi."
  :type '(string)
  :group 'geolocation-system-linux)

(defun geolocation--linux-scan-wifi ()
  "Wifi scanning on Linux not yet implemented."
  (error "Wifi scanning on Linux not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google geolocation api

(defgroup geolocation-api-google nil
  "Configuration needed to call the Google Maps Geolocation API"
  :prefix "geolocation-api-google-"
  :group 'geolocation)

(defcustom geolocation-api-google-url
  "https://www.googleapis.com/geolocation/v1/geolocate"
  "URL for the Google Maps Geolocation API."
  :type '(string)
  :group 'geolocation-api-google)

(defcustom geolocation-api-google-token nil
  "Authorization token for the Google Geolocation API.

IMPORTANT NOTE: This customization is offered for the convenience
of people who want to quickly set up and test this package.
However you are strongly discouraged from leaving authorization
tokens (which are like passwords) in your Emacs configurations as
plainly readable text.  Setting this variable to nil will cause
the library to retrieve your token via `auth-source' instead."
  :type '(choice (const :tag "Retrieve token from `auth-source'" nil)
                 (string :tag "Google API token"))
  :group 'geolocation-api-google)

(defcustom geolocation-api-google-auth-source-host "googleapis.com"
  "The host name used for lookups in `auth-source'."
  :type '(string)
  :group 'geolocation-api-google)

(defcustom geolocation-api-google-auth-source-user "geolocation.el"
  "The user name used for lookups in `auth-source'."
  :type '(string)
  :group 'geolocation-api-google)

(defun geolocation--google-xform-wifi (wifi)
  "Transform WIFI list into the format needed for Google's API.

In particular Google wants the json payload to contain keys
\"macAddress\", \"signalStrength\" and \"channel\" whereas the
wifi scanning functions in the library produce different keys for
those fields."
  (mapcar (lambda (x)
            (list (cons 'macAddress     (alist-get 'bssid x))
                  (cons 'signalStrength (alist-get 'signal x))
                  (cons 'channel        (alist-get 'channel x))))
          wifi))

(defun geolocation--google-xform-location (response)
  "Transform the Google API response RESPONSE into the format needed.

In particular the Google API responds with a json object
\"location\" that contains inside the current \"lat\" and
\"lon\", whereas this library expects those keys to be flattened
at the same level as \"accuracy\"."
  (let* ((r (request-response-data response))
         (loc (alist-get 'location r))
         (lat (alist-get 'lat loc))
         (lng (alist-get 'lng loc))
         (acc (alist-get 'accuracy r)))
    (list (cons 'lat lat)
          (cons 'lon lng)
          (cons 'accuracy acc))))

(defun geolocation--google-get-token ()
  "Resolve the Google API token.

If `geolocation-google-token' is non-nil, then use
that.  Otherwise, retrieve the token via `auth-source-search'
under the hostname `geolocation-api-google-auth-source-host' and
username `geolocation-api-google-auth-source-user'."
  (or geolocation-api-google-token
      (auth-source-pick-first-password
       :host geolocation-api-google-auth-source-host
       :user geolocation-api-google-auth-source-user)))

(defun geolocation--call-google-api (wifi)
  "Invoke the Google Geolocation REST API with WIFI data."
  (message "calling google api")
  (let* ((wifi-g (geolocation--google-xform-wifi wifi))
         (response
          (request geolocation-api-google-url
            :type "POST"
            :params (list (cons "key" (geolocation--google-get-token)))
            :data (json-encode (list (cons "wifiAccessPoints" wifi-g)))
            :parser #'json-read
            :sync t                    ; todo: make this an async call
            :timeout 15)))
    (when (= 200 (request-response-status-code response))
      (geolocation--google-xform-location response))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unwired Labs geolocation api

(defgroup geolocation-api-unwiredlabs nil
  "Configuration needed to call the Unwired Labs Geolocation API."
  :prefix "geolocation-api-unwiredlabs-"
  :group 'geolocation)

(defcustom geolocation-api-unwiredlabs-url
  "https://us2.unwiredlabs.com/v2/process.php"
  "URL for the Unwired Labs Geolocation API."
  :type '(choice (const :tag "US Virginia"
                        "https://us1.unwiredlabs.com/v2/process.php")
                 (const :tag "US San Francisco"
                        "https://us2.unwiredlabs.com/v2/process.php")
                 (const :tag "EU France"
                        "https://eu1.unwiredlabs.com/v2/process.php")
                 (const :tag "AP Singapore"
                        "https://ap1.unwiredlabs.com/v2/process.php")
                 (string :tag "Other"))
  :group 'geolocation-api-unwiredlabs)

(defcustom geolocation-api-unwiredlabs-token nil
  "Authorization token for the Unwired Labs Geolocation API.

IMPORTANT NOTE: This customization is offered for the convenience
of people who want to quickly set up and test this package.
However you are strongly discouraged from leaving authorization
tokens (which are like passwords) in your Emacs configurations as
plainly readable text.  Setting this variable to nil will cause
the library to retrieve your token via `auth-source' instead."
  :type '(choice (const :tag "Retrieve token from `auth-source'" nil)
                 (string :tag "Unwired Labs API token"))
  :group 'geolocation-api-unwiredlabs)

(defcustom geolocation-api-unwiredlabs-auth-source-host "unwiredlabs.com"
  "The host name used for lookups in `auth-source'."
  :type '(string)
  :group 'geolocation-api-unwiredlabs)

(defcustom geolocation-api-unwiredlabs-auth-source-user "geolocation.el"
  "The user name used for lookups in `auth-source'."
  :type '(string)
  :group 'geolocation-api-unwiredlabs)

(defun geolocation--unwiredlabs-get-token ()
  "Resolve the Unwired Labs API token.

If `geolocation-api-unwiredlabs-token' is non-nil, then use that.
Otherwise, retrieve the token via `auth-source-search' under the
hostname `geolocation--unwiredlabs-auth-source-host' and username
`geolocation--unwiredlabs-auth-source-user'."
  (or geolocation-api-unwiredlabs-token
      (auth-source-pick-first-password
       :host geolocation-api-unwiredlabs-auth-source-host
       :user geolocation-api-unwiredlabs-auth-source-user)))

(defun geolocation--call-unwiredlabs-api (wifi)
  "Invoke the Unwiredlabs REST API with WIFI data."
  (message "calling unwiredlabs api")
  (let ((response
         (request geolocation-api-unwiredlabs-url
           :type "POST"
           :data (json-encode
                  `(("token" . ,(geolocation--unwiredlabs-get-token))
                    ("wifi" . ,wifi)))
           :parser #'json-read
           :sync t                      ; todo: make this an async call
           :timeout 15)))
    (when (= 200 (request-response-status-code response))
      (request-response-data response))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

;;;###autoload
(defun geolocation-scan-wifi ()
  "Return a list of nearby wifi access points.
The result is sorted by signal strength with the strongest first.

Each item in the list is an alist with the following keys:
  `bssid'   : mac address of the access point
  `signal'  : relative signal strength or rssi
  `channel' : broadcast channel"
  (when-let ((aps (cond
                   ((string-equal system-type "darwin")
                    (geolocation--osx-scan-wifi))
                   ((string-equal system-type "windows-nt")
                    (geolocation--windows-scan-wifi))
                   ((string-equal system-type "gnu/linux")
                    (geolocation--linux-scan-wifi)))))
    (sort aps (lambda (x y)
                (> (alist-get 'signal x)
                   (alist-get 'signal y))))))

;;;###autoload
(defun geolocation-get-position ()
  "Return your estimated position in terms of latitude and longitude.

The reply is an alist with at least the following keys:
  `lat'      : latitude of the current position
  `lon'      : longitude of the current position
  `accuracy' : accuracy of the estimate in meters

Other keys may be included in the result, but are not guaranteed
to be supported going forward."
  (when-let ((wifi (geolocation-scan-wifi)))
    (cond ((eq :unwiredlabs geolocation-api-vendor)
           (geolocation--call-unwiredlabs-api wifi))
          ((eq :google geolocation-api-vendor)
           (geolocation--call-google-api wifi)))))

(provide 'geolocation)
;;; geolocation.el ends here
