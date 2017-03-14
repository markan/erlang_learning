## Proposed syllabus

### Session 0:
#### Starting reading list
##### Learn you some erlang http://learnyousomeerlang.com/
It's worth reading these sections:
1. Introduction
2. Starting Out
3. Starting Out (for real)
   In the near term, you can skip reading bit syntax and binary comprehensions
4. Modules
5. Syntax in Functions
   This will be very different from what you're used to if you're coming from Ruby or Python
6. Types
7. A short visit to common data structures, especially the records section.
   http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#records

#### More advanced:
##### Erlang Programming by Francesco Cesarini and Simon Thompson, O'Reilly press
It's what I learned erlang from, but it's not been updated in a while
##### Erlang and OTP in Action by Eric Merritt, Martin Logan, and Richard Carlsson, Manning Press
This covers some introductory material, but where it really shines is in it's coverage of the OTP runtime system.
##### Erlang In Anger http://www.erlang-in-anger.com

#### exercisim.io
Not all of the exercises seem particularly valuable, but I did like a few (expand)
1. http://www.exercism.io/exercises/erlang/accumulate/readme

### Session 1:

#### Why do we use erlang
* Functional programming makes concurrency easy to reason about
* Throughput vs single threaded performance
* Memory behavior is ideal for REST APIs

#### Highlights of syntax
* matching, guards, fun heads
* Tail Recursion and looping style in a function
* Functional programming and closure
* No if (case statement/fun head instead)
* Message passing
* Missing concepts
* Maps

#### Chef Style
* Avoiding left leaning style.
* Let it crash (in practice) vs error handling

This may be too much to talk about in a single session.

### Session 2:
* Message passing, processes
* Processes are *cheap* (fun call almost)
* Processes can be used like objects (repository of state (state management))
** Message passing is like method call (and even some OO languages use that term)
** Singleton instances (registered processes) vs generic instances (just a pid)
* Processes have their own GC
** But you may never call it;
*** common pattern is to start a process, use a bunch of memory, and terminate
*** In that case, we just deallocate the process memory; no GC needed.
*** But binaries are handled separately.
* Spawn, Monitoring and linking
* Supervision tree as a core concept.

### Session 3: OTP concepts
Supervisors gen server and gen fsm

### Session 4: Gen server

Exercise; write fizbuz as message passing process, then rewrite as gen_server

### Session 5: Gen FSM
#### gen_fsm
* What do we use FSMs for at Chef?
* Row oriented vs column oriented structures
* Init patterns

#### Code walkthrough

#### Should we look at new behavior that was intro in V19?
(Research needed, mention that there are some new things)


## Miscellaneous sessions

### Session ?: Ecosystem
Core language ets, (dets mnesia exists)
Chef specific (sqerl, pooler)
Common libraries (lager, )

### Session ?: Testing usage and our practices (may be two sessions)
* TDD styles
* eunit 
* ct
* hoax vs mech
* Talk about how we would test our examples above
* Testing gen servers, gen fsms

### Session ?: Debugging erlang
* Redbug
* gen_ helpers (including tracing functions)
* Process viewing

### Session ?:  Bonus debugging

There's a whole class in advanced debugging
in erlang we could  do
* Full bore usage tracing and debugging libraries.
* Specialty debugging of webmachine
* Debugging pooler
* Debugging tricks for server (and delivery)
*

### Session ?: Ports NIFs and external libraries oh my!

### Session ?: Formal tools
* Dialyzer
* proper
* Concolic testing (https://github.com/aggelgian/cuter)


