#!/bin/sh
erl -sname mock_apn -pa ebin/ -config mockapn -eval "application:start(mock_apn)."

