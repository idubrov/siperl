RFC 3261 (SIP) implementation in Erlang
=======================================

This project is implementation of RFC 3261 (SIP: Session Initiation Protocol)
in Erlang.

Currently, a lot of the pieces are missing, some implemented only partially.

Here is a checklist of most important areas:

* Syntax and enconding layer is mostly implemented, however a lot of headers
  are not supported yet. They are not parsed (they are retained in binary form)
  and are not formatted when generating outgoing messages.
* Message parsing/generation API is somewhat clumsy.
* Error handling at syntax and encoding layer is not done. i.e., the code can
  crash on invalid messages, headers, etc, even if such errors are to be
  ignored/handled according to the RFC.
* Transport layer is mostly implemented, but only TCP and UDP are supported.
  TLS is not supported yet.
* RFC 3263 (Locating SIP Servers) is mostly implemented, but is not tested
  well.
* Transaction layer is mostly implemented and tested. However, complex,
  highly concurrent or high load scenarios are not tested.
* Transaction user layer is mostly unimplemented. Only simplest UAC and UAS
  behaviour is in place. No tests are implemented yet.
* Dialogs are not implemented at all.
* The code can crash on invalid messages, headers, etc, even if such errors
  are to be ignored/handled according to the RFC.
* The code was not tested for performance, concurrency, corner cases. In fact,
  only the simplest cases are covered by tests.

Hacking the code
----------------

Compiling:

    $ rebar compile

Running tests for siperl only:

    $ rebar eunit app=sip 

Hacking the code:

    $ rebare compile & shell/siperl

Running simplest scenario of UAC and UAS interaction:

    $ rebar compile & shell/siperl -run sip_test_ua

