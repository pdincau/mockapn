-module(mock_apn_test).
-export([push/0]).

-define(PAYLOAD, <<"{\"aps\":{\"alert\":\"Just testing !!\"}}">>).

connect() ->
        Address = "localhost",
        Port = 2195,

        Options = [{mode, binary}, {packet, 0}, {active, false}],
        Timeout = 1000,

        ssl:start(),
        {ok, _Socket} = ssl:connect(Address, Port, Options, Timeout).

push() ->
        {ok, Socket} = connect(),
        
        PayloadLength = erlang:byte_size(?PAYLOAD),
  
        DeviceToken = "validdevicetokenhere",
        DeviceTokenNum = list_to_integer(DeviceToken, 16),
        BinDeviceToken = <<DeviceTokenNum:32/integer-unit:8>>,
        BinTokenLength = byte_size(BinDeviceToken),

        DeviceToken1 = "invaliddevicetokenhere",
        DeviceTokenNum1 = list_to_integer(DeviceToken1, 16),
        BinDeviceToken1 = <<DeviceTokenNum1:32/integer-unit:8>>,
        BinTokenLength1 = byte_size(BinDeviceToken1),

        Id1 = 1,
        Id2 = 2,
	Id3 = 3,
	
        {MSeconds, Seconds, _} = erlang:now(),
        Expiry = MSeconds * 1000000 + Seconds + 3600*1,

        Packet1 = <<1:8, Id1:32/big, Expiry:32/big, BinTokenLength:16/big, BinDeviceToken/binary, PayloadLength:16/big, ?PAYLOAD/binary>>,
        Packet2 = <<1:8, Id2:32/big, Expiry:32/big, BinTokenLength1:16/big, BinDeviceToken1/binary, PayloadLength:16/big, ?PAYLOAD/binary>>,
	Packet3 = <<1:8, Id3:32/big, Expiry:32/big, BinTokenLength1:16/big, BinDeviceToken1/binary, PayloadLength:16/big, ?PAYLOAD/binary>>,

        ssl:send(Socket, Packet1),
        ssl:send(Socket, Packet2),
        io:format("~p~n", [ssl:recv(Socket, 6)]),
	ssl:send(Socket, Packet3),
        ssl:close(Socket).

