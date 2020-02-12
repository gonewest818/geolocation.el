# geolocation.el

Get your location on Earth.

[![MELPA Stable](https://stable.melpa.org/packages/geolocation-badge.svg)](https://stable.melpa.org/#/geolocation)
[![MELPA](https://melpa.org/packages/geolocation-badge.svg)](https://melpa.org/#/geolocation)
[![CircleCI](https://img.shields.io/circleci/project/github/gonewest818/geolocation.el.svg)](https://circleci.com/gh/gonewest818/geolocation.el)
[![codecov](https://codecov.io/gh/gonewest818/geolocation.el/branch/master/graph/badge.svg)](https://codecov.io/gh/gonewest818/geolocation.el)

## Description

This package obtains your current location by obtaining the MAC
addresses of nearby wifi access points and then submitting that
list to third-party geolocation APIs.  Those geolocation services
use the known locations of the wifi access points and the relative
strength of each signal to triangulate your latitude and longitude.

### Entry points

- `geolocation-get-position` which returns your estimated position as
  an alist with the following keys:
  - `lat` - latitude of the current position
  - `lon` - longitude of the current position
  - `accuracy` - an error radius, in meters

- `geolocation-scan-wifi` which scans for nearby wifi access points
  using available system utilites, and produces a complete list of
  everything in range sorted by signal strength.
  Returns a list of alists containing:
  - `bssid` - mac address that uniquely identifies the AP
  - `signal` - relative signal strength, or RSSI
  - `channel` - transmission channel

At present, wifi scanning is supported on Mac OSX and Windows.
Linux support is planned but not yet implemented.

### Geolocation services

You have a choice of third party services to use for the positioning:

- [Google Maps Geolocation API](https://developers.google.com/maps/documentation/geolocation/intro)
- [Unwired Labs Location API](https://unwiredlabs.com/home)

This package offers a set of customizable variables you can use to
select which service is used, declare your API access token, choose
the nearest API endpoint, and so on.  Those settings can be found in
`M-x customize`, then browse for "Environment", then "hardware", and
then "Geolocation".

By default, access tokens are expected to be accessed via the built-in
`auth-source` package.  You can configure any backend, as this package
only requires the token to be searchable via
`auth-source-pick-first-password`.

### Rate Limits and Costs

Each of these services requires you to create your own account, and
potentially provide billing information as well.  You will be agreeing
to the vendor's end-user agreements including such things as
acceptable usage policies and privacy terms, when you create your
account.

Care has been taken to choose services that include either a free
tier, or equivalently, an allowance of a certain amount of usage per
day or per month.  It's your responsibility to set up your account
with those services, obtain and protect your token, and manage your
usage of those APIs because there can be associated costs if your
usage exceeds the usage terms of the subscription or plan you choose.

WARNING: THIS PACKAGE COLLECTS INFORMATION THAT IMPLIES YOUR PHYSICAL
LOCATION.  THAT DATA WILL BE SENT TO THIRD-PARTY GEOLOCATION SERVICES
WHICH CAN PINPOINT YOUR LOCATION TYPICALLY WITHIN 100 METERS OR LESS.
WHILE THIS PACKAGE DOES NOT ITSELF STORE YOUR LOCATION, WE CANNOT
GUARANTEE WHAT A GIVEN THIRD-PARTY SERVICE MAY DO WITH THE DATA IT
RECEIVES IN THESE API TRANSACTIONS.  RATHER, YOU UNDERSTAND AND AGREE
TO THE PRIVACY AGREEMENTS AND END USER AGREEMENTS OF EACH OF THE
INDIVIDUAL GEOLOCATION APIS YOU USE.

## Documentation

- TBD documentation
  - how to sign up and obtain tokens
  - how to protect tokens using `auth-sources'

## Changelog:

- first snapshot
  - scan wifi access points using available system utilities
    - OSX : use the "airport" utility
    - Windows : use "netsh wlan show networks"
    - Linux : not yet implemented
  - get geolocation via third party APIs
    - [Google Maps Geolocation API](https://developers.google.com/maps/documentation/geolocation/intro)
    - [Unwired Labs Location API](https://unwiredlabs.com/home)

## Copyright

Copyright (C) 2020  Neil Okamoto <neil.okamoto+melpa@gmail.com>.
