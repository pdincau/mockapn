-module(mock_apn_test).
-export([push/0]).

push() ->
	Address = "localhost",
	Port = 5555,
	Cert = "../certs/certificate.pem",
	Key = "../certs/key.pem",
	Password = "password",  

	Options = [{mode, binary}, {packet, 0}, {active, false}],
  	Timeout = 1000,

  	ssl:start(),
  	{ok, Socket} = ssl:connect(Address, Port, Options, Timeout),

  	BinPayload = <<"{\"aps\":{\"alert\":\"Just testing 1!!\"}}">>,
  	PayloadLength = erlang:byte_size(BinPayload),
  
  	DeviceToken = "your_device_token_as_string",
	DeviceTokenNum = list_to_integer(DeviceToken, 16),
	BinDeviceToken = <<DeviceTokenNum:32/integer-unit:8>>,
    	BinTokenLength = byte_size(BinDeviceToken),

	Id = 1,
  	{MSeconds, Seconds, _} = erlang:now(),
  	Expiry = MSeconds * 1000000 + Seconds + 3600*1,

	Packet = <<1:8, Id:32/big, Expiry:32/big, BinTokenLength:16/big, BinDeviceToken/binary, PayloadLength:16/big, BinPayload/binary>>,
  	
  	ssl:send(Socket, Packet),
  	ssl:close(Socket).
