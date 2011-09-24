RFC 3261 (SIP) implementation in Erlang
=======================================

This project is implementation Session Initiation Protocol in Erlang (RFC 3261
and related).

General notes
-------------

Currently, a lot of the pieces are missing, some are implemented only partially.

Here is a checklist of most important areas:

* Syntax and enconding layer is mostly implemented. All RFC 3261 headers
  support is implemented.
* Message parsing/generation API is somewhat clumsy.
* Error handling at syntax and encoding layer is not done. i.e., the code can
  crash on invalid messages, headers, etc, even if such errors are to be
  ignored/handled according to the RFC.
* Transport layer is mostly implemented, but only TCP and UDP are supported.
  TLS is not supported yet.
* RFC 3263 (Locating SIP Servers) is mostly implemented, but is not tested
  well.
* Transaction layer is mostly implemented and tested.
* Transaction user layer is mostly unimplemented. Only simplest UAC and UAS
  behaviour is in place. Very simple tests are present. API is far from stable.
* Minimal dialogs implementation present.
* No sessions support yet. Offer/answer model is not supported.
* The code was not tested for performance, concurrency, corner cases. In fact,
  only the simplest cases are covered by tests.

Hacking the code
----------------

Compiling:

    $ rebar compile

Running tests for siperl only:

    $ rebar eunit ct app=sip 

Hacking the code:

    $ rebare compile & shell/siperl

Running simplest scenario of UAC and UAS interaction:

    $ shell/ct.sh apps/sip/test/ua.spec

API Notes
---------

 * Parsing, formatting, generating and processing messages is mostly in
   `sip_headers` and `sip_message`. modules The The former is mostly used for
   individual headers, the latter -- to update message as a whole. Data structures
   for the supported headers and messages are in `sip.hrl`
 * Transport layer API is `sip_transport` module. Transport layer dispatches
   messages to the transaction layer. If transaction layer cannot handle the
   message, transport layer lookups core via `sip_cores:lookup_core/1`
   invocation. If core process is found, message is sent to it in form of either
   `{request, Msg}` or `{response, Msg}`.
 * Transaction layer API is in `sip_transaction` module. When transaction is
   started, a TU PID is provided to the transaction. Responses from client
   transactions are sent to TU in form of `{response, Msg, TxPid}`. Requests
   from server transactions (the only possible request is `ACK`) are sent to TU
   in form of `{request, Msg, TxPid}`. Transport errors from server transactions
   are reported in form of `{tx_error, Reason, TxPid}`.
 * Cores are registered via `sip_cores:register_core/1` invocation. Basically,
   registration is a `gproc` property added to the core process with value of
   `#sip_core_info{}`. For now, it contains single element -- function that returns
   if core is applicable for given message or not.
 * Utility functions for processing binaries are in `sip_binary` and `sip_syntax`.
 * Functions to process supported URIs (for now, only `sip` scheme is supported)
   are in `sip_uri`.

Implemented RFCs
----------------
* [RFC 3261](http://tools.ietf.org/html/rfc3261 "RFC 3261") SIP: Session Initiation Protocol (partial support)
* [RFC 6026](http://tools.ietf.org/html/rfc6026 "RFC 6026") "Correct Transaction Handling for 2xx Responses to Session Initiation Protocol (SIP) INVITE Requests"
