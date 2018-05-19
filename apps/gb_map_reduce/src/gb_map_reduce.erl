-module(gb_map_reduce).

-export([test_init/0,
	 start/0]).

test_init() ->
    {ok, Targets} = application:get_env(?MODULE, targets),
    connect(Targets),
    test_init(Targets).

%% Creates tables on each node seperatelly.
%% Nodes are not clustred.
test_init([#{node:=Node} | Rest]) ->
    <<ID:8, Data/binary>> = crypto:strong_rand_bytes(1024),
    rpc:call(Node, enterdb, delete_table, ["test_mr"]),
    rpc:call(Node, enterdb, create_table, ["test_mr", ["id"], [{num_of_shards, 1}]]),
    rpc:call(Node, enterdb, write, ["test_mr", [{"id", ID}], [{"bytes", Data}]]),
    test_init(Rest);
test_init([]) ->
    ok.

start()->
    {ok, Targets} = application:get_env(?MODULE, targets),
    {ok, MFA} = application:get_env(?MODULE, mfa),
    connect(Targets),
    {Mod, File, Bin} = compile(),
    load_code(Mod, File, Bin, Targets),
    Res = map_reduce(MFA, Targets),
    cleanup(Mod, Targets),
    Res.

connect([#{node := Node, cookie := Cookie} | Rest]) ->
    erlang:set_cookie(Node, Cookie),
    case net_adm:ping(Node) of
	pong -> connect(Rest);
	pang -> {connection_error, Node}
    end;
connect([]) ->
    ok.

compile() ->
    {ok, Basename} = application:get_env(?MODULE, source),
    PrivDir = code:priv_dir(?MODULE),
    File = filename:join(PrivDir, Basename),
    {ok, Mod, Bin} = compile:file(File, [binary]),
    {Mod, File, Bin}.
    
load_code(Mod, File, Bin, [#{node:=Node} | Rest]) ->
    Res = rpc:call(Node, code, load_binary, [Mod, File, Bin]),
    io:format("load ~p on ~p -> ~p~n",[Mod, Node, Res]),
    load_code(Mod, File, Bin, Rest);
load_code(_Mod, _File, _Bin, []) ->
    ok.

cleanup(Mod, [#{node:=Node} | Rest]) ->
    rpc:call(Node, code, purge, [Mod]),
    rpc:call(Node, code, delete, [Mod]),
    cleanup(Mod, Rest);
cleanup(_Mod, []) ->
    ok.

map_reduce(MFA, Targets) ->
    Count = length(Targets),
    Pid = self(),
    Reducer = spawn(fun() -> reduce(Pid, Count) end),
    {ok, Timeout} = application:get_env(?MODULE, timeout),
    _WorkerPids = [spawn(fun() -> map(Reducer, MFA, Timeout, T) end) || T <- Targets],
    receive
	{Pid, Res} ->
	    Res
    after Timeout ->
	Reducer ! timeout,
	receive
	    {Pid, Res} ->
		Res
	end
    end.

map(Reducer, {Mod, Fun, Args}, Timeout, #{node := Node}) ->
    Res = rpc:call(Node, Mod, Fun, Args, Timeout),
    Reducer ! {result, Node, Res}.

reduce(Pid, Count) ->
    reduce(Pid, Count, []).

reduce(Pid, 0, Acc) ->
    Pid ! {Pid, {ok, Acc, complete}};
reduce(Pid, Count, Acc) ->
    receive
	timeout ->
	    Pid ! {Pid, {timeout, Acc, {remaining, Count}}};
	{result, Node, R} ->
	    reduce(Pid, Count-1, [{Node, R} | Acc])
    end.
