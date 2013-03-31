mockapn
=======

A mock server for APN.

This software simulates APN. Currently it just receives push notifications and prints on screen the token. 
If a token is invalid the connection is closed. No other functionality is implemented for now.

The first thing you have to do is set the proper values in `src/mock_apn_sup.erl` for the key, the password and the certificate.
You may want to change in `src/mock_apn_server.erl` the value list of the invalid tokens. 

How to compile:

    $ make

How to run:

    $ make start

How to test:

First you have to start the mock server:

    $ make start

Then you can in a separated shell:

    $ erl
    1> mock_apn_test:push().


