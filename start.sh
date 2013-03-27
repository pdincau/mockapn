#!/bin/sh
erl -sname mock_apn -pa ebin/ -eval "application:start(mock_apn)."

