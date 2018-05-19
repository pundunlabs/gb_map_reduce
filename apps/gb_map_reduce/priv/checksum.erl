-module(checksum).

%% Application callbacks
-export([run/0]).

run() ->
    %timer:sleep(5000),
    {ok, Entry, _It} = enterdb:first("test_mr"),
    {[{_,Key}],[{_,Bytes}]} = Entry,
    #{key => Key,
      bytes => Bytes,
      hash => crypto:hash(sha, Bytes)}.
