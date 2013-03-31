mockapn
=======

This software simulates an `APN server`. 


### What mockapn does:

Currently `mockapn` only receives push notifications printing:

1. the token and the JSON for a successful push
2. the token for an not successful push

If a token is invalid (most common error in my experience with APN) an error message is sent to client and the connection is closed. 

No other functionality is implemented for now.

### How to adapt the code to your needs:

In order to have `mock_apn` working, you have to:

1. change in `src/mock_apn_sup.erl` the values for the SSL key, password and certificate
2. change in `src/mock_apn_server.erl` the list representing the invalid tokens

For test purposes you may also want to change the tokens in `test/mock_apn_test.erl` accordingly. 

### How to compile:

    $ make

### How to run:

    $ make start

### How to test:

First you have to start the mock server:

    $ make start

Then you have to call in a separated shell:

    $ erl
    1> mock_apn_test:push().
