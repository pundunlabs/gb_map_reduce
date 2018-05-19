# gb_map_reduce

A simple library to run code on remote erlang nodes and to collect results back.
It compiles given erlang source code into binary beam and loads it on configured target nodes. Then, an rpc call is performed towards target nodes with given Mod, Fun and Args.
Results are collected and loaded beam codes removed from targets.

## Configure
Edit env in  apps/gb_map_reduce/src/gb_map_reduce.app.src
```
{env,[
    {targets, [#{node => 'pundun832a87@sleepy',
		 cookie => pundun}]},
    {source, "checksum.erl"},
    {mfa, {checksum, run, []}},
    {timeout, 10000}
]}
```
- targets: configuration for remote nodes that the code will run on.
- source: erlang source code that will be compiled to beam code and distributed.
- mfa: tuple of three that defines Module, Function and Arguments that will be called on remote node.
- timout: time in milliseconds to wait for responses from remote nodes.

Place the erlang source code under apps/gb_map_reduce/priv/ directory.

## Build and Run
```sh
$ rebar3 release
$ _build/default/rel/gb_map_reduce/bin/gb_map_reduce console
(gb_map_reduce@host)1> gb_map_reduce:test_init().
ok
(gb_map_reduce@host)2> gb_map_reduce:start().
{ok,[{pundun832a87@sleepy,#{bytes =>
				<<145,122,80,31,97,208,10,136,33,152,58,32,16,18,250,
				207,153,161,197,199,86,14,129,...>>,
			    hash =>
				<<228,130,67,50,252,90,124,6,229,49,83,118,242,210,162,
				227,226,148,181,16>>,
			    key => 234}}],
    complete}
```
