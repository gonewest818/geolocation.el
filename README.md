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

- `geolocation-update-position` which calls `geolocation-get-position`
  on a regular interval, and sets `geolocation-location` with the
  result.  Calls the `geolocation-update-hook` functions after each
  update.  Customize the hook if you want to invoke functions based on
  your position, and customize the `geolocation-update-interval` with
  the time granularity you need, keeping in mind the underlying
  positioning APIs may have rate limits and/or costs associated with
  high frequency querying.

  The variable `geolocation-location` will contain nil or an alist:
    - `latitude` - latitude of the current position
    - `longitude` - longitude of the current position
    - `accuracy` - an error radius, in meters
    - `timestamp` - timestamp via `float-time`

Other potentially useful functions include:

- `geolocation-get-position` which retrieves your estimated position
  once and invokes a callback with the position data.  The callback
  receives an alist with the same format as `geolocation-location`.

- `geolocation-scan-wifi` which scans asynchronously for nearby wifi
  access points using available system utilites, and invokes a callback
  with the wifi data.  The callback receives a list of alists containing:
  - `bssid` - mac address that uniquely identifies the AP
  - `signal` - relative signal strength, or RSSI
  - `channel` - transmission channel

At present, wifi scanning is supported on Mac OSX and Windows.
Linux support is planned but not yet implemented.

### Geolocation services

You have a choice of third party services to use for the positioning:

- [Google Maps Geolocation API](https://developers.google.com/maps/documentation/geolocation/intro)
- [HERE Technologies Positioning API](https://developer.here.com/develop/rest-apis)
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

### Obtaining the API token

Choose one of the following services and follow the corresponding link.
In each case you're going to have to create a developer account with the
company subject to the service's pricing and terms.

You only need one of the following:
- [Google Maps Geolocation API](https://developers.google.com/maps/documentation/geolocation/intro)
- [HERE Technologies Positioning API](https://developer.here.com/develop/rest-apis)
- [Unwired Labs Location API](https://unwiredlabs.com/home)

Once you have an account, you will be able to generate an API token.
Copy down that token (it's typically a long string of numbers and letters)
and store it somewhere safe (see next section).

### Protecting your token using `auth-sources`

The idea here is to configure `auth-sources` and select a backend
for your passwords to be stored.  Assuming you use a GnuPG encrypted
file named `~/.authinfo.gpg` then that file needs to contain a line
like this:

``` shell
machine googleapis.com user geolocation.el password "#############"
```

The `password` field must contain the token you obtained when you
created your account.  The `machine` and `user` fields must match the
values of the corresponding customizable variables described below,
and if you're in doubt, the default user and hostname will be fine.

See the article ["Keeping Secrets in Emacs with GnuPG and Auth
Sources"](https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources)
for detailed information about using the `auth-sources` library.

### Configuration settings

To configure the package use `M-x customize` and browse to
"Environment" then "Hardware" and finally "Geolocation".

You will need to customize `geolocation-api-vendor` to select the
service you want to use. Accepted values are `:google` (default),
`:here` and `:unwiredlabs`.

Then you need to set the host and usernames so this library can
find your token in `auth-sources`. Those are:

- Google
  - `geolocation-api-google-auth-source-host` (default is "googleapis.com")
  - `geolocation-api-google-auth-source-user` (default "geolocation.el")
- HERE
  - `geolocation-api-here-auth-source-host` (default "pos.ls.hereapi.com")
  - `geolocation-api-here-auth-source-user` (default "geolocation.el")
- Unwired Labs
  - `geolocation-api-unwiredlabs-auth-source-host`  (default "unwiredlabs.com")
  - `geolocation-api-unwiredlabs-auth-source-user` (default "geolocation.el")

In ordinary situations you won't need to customize anything else.

## Changelog:

- First snapshot released to MELPA ({include date when released})
  - Scan wifi access points using available system utilities
    - OSX: use the "airport" utility
    - Windows: use "netsh wlan show networks"
    - Linux:  not yet implemented
  - Get geolocation via third party APIs (Google, HERE, and Unwired Labs)

## Copyright

Copyright (C) 2020  Neil Okamoto <neil.okamoto+melpa@gmail.com>.
