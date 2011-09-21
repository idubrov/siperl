%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS implementation that always responds with 486 Busy Here
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(hang_ua).
-extends(sip_ua_default).

%% API
-export([start_link/0, is_applicable/1, call/1, init/1]).

%% UA callbacks
-export([handle_call/3, handle_response/4]).

%% Include files
-include_lib("sip/include/sip.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%-----------------------------------------------------------------
%% API
%%-----------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    sip_ua:start_link({local, ?SERVER}, ?MODULE, {}).

-spec call(string()) -> ok.
call(To) ->
    URI = sip_uri:parse(list_to_binary(To)),
    gen_server:call(?SERVER, {call, URI}).

%%-----------------------------------------------------------------
%% UA callbacks
%%-----------------------------------------------------------------
-spec is_applicable(#sip_request{}) -> boolean().
%% @doc We do not serve any requests. We server 2xx responses (which could
%% be additional 2xx responses to our requests).
%% @end
is_applicable(#sip_request{}) -> false;
is_applicable(#sip_response{status = Status}) when Status >= 200; Status =< 299 -> true;
is_applicable(#sip_response{}) -> false.

-spec init({}) -> {ok, #state{}}.
init({}) ->
    io:format("HANG: Call someone by running hang_ua:call(\"SIP URI\") in console ~n"),
    {ok, #state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call({call, To}, _From, #state{} = State) ->
    Request = sip_ua:create_request('INVITE', sip_headers:address(<<>>, To, [])),
    Contact = sip_headers:address(<<"Hang">>, <<"sip:127.0.0.1:5060">>, []),
    Request2 = sip_message:append_header(contact, Contact, Request),

    % Request will be cancelled automatically after 5 seconds
    Request3 = sip_message:append_header(expires, 5, Request2),

    % Call
    io:format("HANG: Calling to ~s~n", [to(Request3)]),
    {ok, _RequestId} = sip_ua:send_request(Request3),

    {reply, ok, State}.

%-spec handle_response(ReqId, Response).
handle_response(#sip_request{method = 'BYE'}, _Response, _RequestId, State) ->
    {noreply, State};

handle_response(#sip_request{method = 'INVITE'}, #sip_response{status = Status} = Response, _RequestId, State)
  when Status >= 100, Status =< 199 ->
    io:format("HANG: Progress from ~s: ~w ~s~n",
              [to(Response), Status, binary_to_list(Response#sip_response.reason)]),
    {noreply, State};

handle_response(#sip_request{method = 'INVITE'}, #sip_response{status = Status} = Response, RequestId, State) when Status >= 200, Status =< 299 ->
    io:format("HANG: Got success response ~s: ~w ~s~n",
              [to(Response), Status, binary_to_list(Response#sip_response.reason)]),

    % FIXME: Where do we get dialog id? Maybe, sip_ua_client should pass it?
    DialogId = sip_dialog:dialog_id(uac, Response),

    % FIXME: Should have create_ack in sip_ua, so we don't have to deal with CSeq constructions...
    % Or: maybe sip_ua_client should generate ACK on its own (it is required anyway), by
    % retrieving SDP from callback? Like: Callback:answer(Offer).
    ACK = sip_ua:create_request('ACK', DialogId),

    CSeq = sip_message:header_top_value(cseq, Response),
    ACK2 = sip_message:replace_top_header(cseq, CSeq#sip_hdr_cseq{method = 'ACK'}, ACK),
    ACK3 = ACK2#sip_request{body = sdp()},
    sip_ua:send_request(ACK3),

    % Send BYE request immediately..
    BYE = sip_ua:create_request('BYE', DialogId),
    sip_ua:send_request(BYE),

    {noreply, State};

handle_response(#sip_request{method = 'INVITE'}, #sip_response{status = Status} = Response, RequestId, State) when Status >= 300 ->
    io:format("HANG: Got failure response ~s: ~w ~s~n",
              [to(Response), Status, binary_to_list(Response#sip_response.reason)]),
    {noreply, State}.

to(Msg) ->
    #sip_hdr_address{uri = To} = sip_message:header_top_value(to, Msg),
    binary_to_list(sip_uri:format(To)).

%% @doc Generate fake SDP
%% Our UAC immediately sends BYE after 2xx response is received. Since UAC MUST
%% put session answer in ACK, we use this fake SDP.
%% @end
sdp() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    Timestamp = sip_binary:integer_to_binary(MegaSecs * 1000000 + Secs),
    Self = sip_config:self(),
    Port = sip_binary:integer_to_binary(49000), % XXX: arbitrary port...
    <<"v=0\r\n",
      "o=hang ", Timestamp/binary, " ", Timestamp/binary, " IN IP4 ", Self/binary, "\r\n",
      "s=hang\r\n",
      "c=IN IP4 0.0.0.0\r\n",
      "t=0 0\r\n",
      "m=audio ", Port/binary, " RTP/AVP 112 113\r\n",
      "a=rtpmap:112 L16/48000/2\r\n",
      "a=rtpmap:113 DAT12/32000/4\r\n",
      "a=fmtp:113 contents:DV L/R/C/WO\r\n">>.

