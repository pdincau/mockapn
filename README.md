mockapn
=======

This software simulates an `APN server`. 


Currently it just receives push notifications and prints on screen the tokens and the JSON message. 

If a token is invalid (most common error in my experience with APN) an error message is sent to client and the connection is closed. 

No other functionality is implemented for now.

The first thing you have to do is set the proper values in `src/mock_apn_sup.erl` for the key, the password and the certificate.

You may want to change in `src/mock_apn_server.erl` the value list of the invalid tokens.

You may also want to change the tokens in `test/mock_apn_test.erl` accordingly. 

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
