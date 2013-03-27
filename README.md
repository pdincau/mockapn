mockapn
=======

A mock server for APN.

This software simulates APN. Currently it just receives message, no other functionality is implemented.

The first thing you have to do is set the proper values in `src/mock_apn_sup.erl` for the key, the password and the certificate.

How to compile:

    $ make

How to run:

    $ make start

