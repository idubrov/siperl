Demonstrational projects
=======================

* busy: replies with busy after 10 seconds of ringing
* hang: calls, then hangs after 2 seconds

To run the demos, compile the code and generate the release.
Assuming the current directory is the project root directory
(parent to the directory with this README.md), run the following
command:

    $ ./rebar compile generate

Then, run the generated release:

    $ ./demos/rel/demos/bin/demos console

Erlang node should start with all demos launched on it. See
the output for the usage scenarios of different demos.
