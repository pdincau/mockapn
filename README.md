mockapn
=======

A mock server for APN.

This software simulates an `APN server`. 

Currently it just receives a bunch of push notifications and prints on screen the token and the json. 

If a token is invalid (most common case in my experience with APN) the mock server sends back an error packet and then the connection is closed. 

No other functionality is implemented for now.

The first thing you have to do is to set the proper values in `src/mock_apn_sup.erl` for the key, the password and the certificate.

You have also to change in `src/mock_apn_server.erl` the value list of the invalid tokens and in `test/mock_apn_test.erl` the values of the tokens for the different notifications. 

### Compile:

    $ make

### Run:

    $ make start

### Test:

First you have to start the mock server:

    $ make start

Then you have to call in a separated shell:

    $ erl
    1> mock_apn_test:push().


