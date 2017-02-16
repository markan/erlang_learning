= Debugging Erlang


== Attaching to a running erlang process:

Erlang lets you attach a remote shell to a running process

=== Discovering what erlang services are running

```
% epmd -names
epmd: up and running on port 4369 with data:
name erchef at port 33806
name bookshelf at port 45061
name oc_bifrost at port 49756
name rabbit at port 25672
```

=== Connecting via remsh

```
erl -hidden -remsh erchef@127.0.0.1 -setcookie erchef -name mark@127.0.0.1
```
Notes:
+ This connects to erchef@127.0.0.1; other processes have different
  names. The -name field in ps may provide cues.

  ```
  opscode   4976  0.8  1.8 483544 72028 ?        Ssl  Oct19 157:23 /opt/opscode/embedded/service/opscode-erchef/erts-6.4/bin/beam.smp -Bd -K true -A 5 -- -root /opt/opscode/embedded/service/opscode-erchef -progname opt/opscode/embedded/service/opscode-erchef/bin/oc_erchef -- -home /var/opt/opscode/opscode-erchef -- -noshell -noshell -noinput -boot /opt/opscode/embedded/service/opscode-erchef/releases/12.9.1+20160926211031/oc_erchef -mode embedded -config /opt/opscode/embedded/service/opscode-erchef/sys.config -boot_var ERTS_LIB_DIR /opt/opscode/embedded/service/opscode-erchef/erts-6.4/../lib -name erchef@127.0.0.1 -setcookie erchef -smp enable -pa lib/patches -- foreground
 ```
 Here, the name we will provide is erchef@127.0.0.1

* The value for -setcookie may require research; newer releases
  obscure it.
  * process command line in ps (deprecated)
  * ~/.erlang.cookie
  * runit script for process.

* -name is our local name (not the thing we're connecting to). It
  should be unique; choosing your own name is helpful but not
  necessary.

=== Shortcuts
chef-delivery-ctl provides a connect method that is easier.


== Manuvering in the shell

http://erlang.org/doc/man/shell.html

Check the node we're on
    1> node().
    'erchef@127.0.0.1'

Enumerate named processes running
    1> io:format("~p",[registered()]).
The io:format prevents the list from being trucuated. As a quick trick, the 'rp' shell command also has this effect (so ```rp(registered())``` works).


=== Records

By default, the erlang shell doesn't load the record information

If you have the source code available, you can manually load the record definitions. For example, this grabs all the records defined in oc_erchef's include dir. Note that this path changes with version numbers and releases.
    rr("/opt/opscode/embedded/service/opscode-erchef/lib/oc_erchef-12.9.1+20160926211031/include/*.hrl").

To print a record definition: (rl/0 prints them all) 
    rl(base_state). 

To print a structure using all loaded record defs
	rp(MyData).

== Redbug (Part of eper)

Redbug is my first choice for debugging. It builds a nice layer on top of the native tracing/debugging layers.
https://github.com/massemanet/eper/blob/master/doc/redbug.txt

=== Basic trace
redbug:start("chef_wm_validate:to_json").

=== Stack and return annotations add more
redbug:start("chef_wm_validate:to_json->stack,return").

=== Can set params
Params = [{time, 30000}, % useful for long running tests (default 15,000 ms)
          {msgs, 10}]     % for chatty 
redbug:start("chef_wm_validate:to_json->stack,return", Params). 

=== Can generate pretty much arbitrary match expressions (except no records)

This is useful when trying to debug a heavily used function
For example if you are interested in looking at oc_chef_wm_base:is_authorized *only* when we are
calling from chef_wm_validate and not the rest of the webmachine stack, you would want to match when the state record indicates we're coming from the chef_wm_validate endpoint.

Unfortunately, redbug doesn't grok records (and the underlying trace system can't either), so you can't do this:
```
redbug:start("oc_chef_wm_base:is_authorized(R,#base_state{resource_mod=chef_wm_validate})->stack").
```
But that just won't work.

But you can construct guards that get you there...

```
redbug:start("oc_chef_wm_base:is_authorized(R,S) when element(2,S)==chef_wm_validate->stack", [{time, 30000}]).
```

=== Can add custom filters.
f(F), f(Params).
F = fun(A) -> io:format("Foo->~p~n<-",[erlang:tuple_size(A)]) end.                       
Params = [{time, 30000}, {print_fun, F}].


redbug:start("chef_wm_validate:to_json->stack", [{time, 30000}, {print_fun, F}]).

Format of 
redbug:start("chef_wm_validate:to_json->stack,return", Params).




== Dtop

dtop:start().
```
memory:      proc   18.9M, atom  818.1k, bin    1.5M, code   15.3M, ets    1.8M
                      
pid            name                         current             msgq    mem cpu
<0.26.0>       file_server_2                gen_server:loop/6      0  34.5k   7
<0.568.0>      sync_scanner                 gen_server:loop/6      0   4.7M   4
<0.18226.0>    prfTarg                      prfPrc:pid_info/2      0 514.4k   1
<0.31.0>       user_drv                     user_drv:server_l      0  21.7k   0
<0.33.0>       group:server/3               group:more_data/5      0  55.4k   0
<0.10177.1>    dtop                         prfHost:loop/1         0 109.2k   0

```
dtop:stop().


== Erlang builtin etop

```
etop:start().
========================================================================================
 'erchef@127.0.0.1'                                                        17:58:47
 Load:  cpu         0               Memory:  total       43006    binary        859
        procs     354                        processes   21780    code        14022
        runq        1                        atom          791    ets          1471

Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function
----------------------------------------------------------------------------------------
<0.569.0>      sync_scanner             '-'******** 5692920       0 gen:do_call/4       
<0.26.0>       file_server_2            '-'********   21768       1 prim_file:drv_get_re
<0.3.0>        erl_prim_loader          '-'14610173  284576       0 erl_prim_loader:loop
<0.27.0>       code_server              '-'14319387 1115080       0 code_server:loop/1  
<0.568.0>      sync_options             '-' 3937281   67904       0 gen_server:loop/6   
<0.543.0>      webmachine_mochiweb      '-' 2981192   11384       0 gen_server:loop/6   
<0.391.0>      oc_chef_authz_cleanu     '-' 1098454   11848       0 gen_fsm:loop/7      
<0.260.0>      timer_server             '-'  893210   16896       0 gen_server:loop/6   
<0.33.0>       group:server/3           '-'  205942 4119680       0 group:server_loop/3 
<0.96.0>       epgsql_sock:init/1       '-'  111542   21664       0 gen_server:loop/6   
========================================================================================
```

It doesn't seem to respond to interrupts to make it stop. However ^G
brings you into the 'JCL' mode, which lets you terminate it:
```
User switch command
 --> i
 --> ?
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
 --> j
   1* {shell,start,[init]}
 --> i 1
 --> c
```

== sys module inspection/tracing
Works on otp services (gen_server, gen_fsm, etc.)

```
rp(lists:sort(registered())). 
... registered process list...

sys:statistics(chef_keygen_cache, true).

sys:get_status(chef_keygen_cache).
sys:get_state(chef_keygen_cache).

statistics(chef_keygen_cache, true).
statistics(chef_keygen_cache, get).
statistics(chef_keygen_cache, false).
```

Events can be traced; but it can get verbose...
sys:trace(chef_keygen_cache, true).   

== dbg module (not the same as 'debug')
Incredibly powerful.

```
dbg:tracer(). 
dbg:p(all, c). % capture all process (can also capture message traffic
dbg:tpl(chef_wm_validate, c). % matches all *calls* (other choices are x, and cx)
dbg:tpl(chef_wm_validate, [{'_', [], [{return_trace}]}]). % adds return value, matches everything


dbg:ctpl(chef_wm_validate).
```

Lots of other things to match on; events (tpe)

This is really, really awesome, once you learn it. The big downside is
that the flexibility has a cost. You quickly end up writing match
specs, which can be a little bit hard to get started with. dbg:fun2ms
can help.

Note: In the end, this is a wrapper for an even more powerful low level library 'trace'

== Webmachine tracing

https://github.com/webmachine/webmachine/wiki/Debugging

Modify the appropriate resource
```
init(Config) ->
   {{trace, "/tmp"}, Config}.  %% debugging code
```

wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp").

== io:format and lager

If all fails, io:format can be your friend.

```io:format("DebugLog ~s~n", [Value]).```

Downside is that it can require a recompile.


== Observer

Observer is a fabulously helpful visualization tool to show
supervision trees and the like. However it uses the erlang Wx library,
which we don't build by default as it's kind of a heavyweight
dependency.

Remote shells from a node with WxWidgets installed will will work;
you'll need a build of erlang configured to use WxWidgets, and most
likely an ssh tunnel to reach epmd (since we are turning off remote
connections to epmd by default in new releases.)

== Extreme measures

=== Dtrace/systemtap:

Erlang can be built with dtrace/systemtap support, and has a large supply of
probes.

When is it useful
+ debugging NIFs'
+ systems under extreme load.

You'll need to build a custom version of erlang with dtrace on, and
install the systemtap libraries if you are on linux. This most likely
will require a custom omnibus package.



