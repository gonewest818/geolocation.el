# geolocation.el

Obtain current location using third-party geolocation APIs

(This is ALPHA quality code, not ready for general use.)


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

WARNING: THIS PACKAGE COLLECTS INFORMATION THAT IMPLIES YOUR
PHYSICAL LOCATION.  THAT DATA WILL BE SENT TO THIRD-PARTY
GEOLOCATION SERVICES WHICH CAN PINPOINT YOUR LOCATION TYPICALLY
WITHIN 100 METERS OR LESS.  WHILE THIS PACKAGE DOES NOT ITSELF
STORE YOUR LOCATION, WE CANNOT GUARANTEE WHAT A GIVEN THIRD-PARTY
SERVICE MAY DO WITH THE DATA IT RECEIVES IN THESE API TRANSACTIONS.

## Documentation

- to be added

## Changelog:

- to be added

## Copyright

Copyright (C) 2020  Neil Okamoto <neil.okamoto+melpa@gmail.com>.
