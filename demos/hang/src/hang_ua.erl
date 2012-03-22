%%% @author Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc UAS implementation that always responds with 486 Busy Here
%%% @end
%%% @copyright 2011 Ivan Dubrov
-module(hang_ua).
-extends(sip_ua_default).

%% API
-export([start_link/0, is_applicable/1, call/1, bye/1, init/1]).

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
    sip_ua:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec call(string()) -> ok.
call(To) ->
    URI = sip_uri:parse(list_to_binary(To)),
    gen_server:call(?SERVER, {call, URI}).

-spec bye(#sip_dialog_id{}) -> ok.
bye(DialogId) ->
    gen_server:call(?SERVER, {bye, DialogId}).

%%-----------------------------------------------------------------
%% UA callbacks
%%-----------------------------------------------------------------
-spec is_applicable(#sip_request{}) -> boolean().
%% @doc Serve requests to user `hang'
%% @end
is_applicable(#sip_request{uri = #sip_uri{user = <<"hang">>}}) -> true;
is_applicable(#sip_request{} = Request) ->
    To = sip_message:header_top_value('to', Request),
    To#sip_hdr_address.uri#sip_uri.user == <<"hang">>.

-spec init({}) -> {ok, #state{}}.
init({}) ->
    gen_event:add_sup_handler(sip_dialog_man, hang_ua_sessions, {self()}),
    io:format("HANG: Call someone by running hang_ua:call(\"SIP URI\") in console ~n"),
    {ok, #state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call({call, To}, _From, #state{} = State) ->
    Request = sip_ua:create_request('INVITE', sip_headers:address(<<>>, To, [])),
    Contact = sip_headers:address(<<"Hang">>, <<"sip:hang@127.0.0.1:5060">>, []),
    Request2 = sip_message:append_header(contact, Contact, Request),

    From = Contact#sip_hdr_address{params = [{tag, sip_idgen:generate_tag()}]},
    Request3 = sip_message:replace_top_header(from, From, Request2),

    % Request will be cancelled automatically after 5 seconds
    Request4 = sip_message:append_header(expires, 5, Request3),

    % put session in INVITE
    Request5 = sip_message:append_header('content-type', <<"application/sdp">>, Request4),
    Request6 = Request5#sip_request{body = sdp()},

    % Call
    io:format("HANG: Calling to ~s~n", [to(Request6)]),
    {ok, _RequestId} = sip_ua:send_request(Request6),

    {reply, ok, State};

handle_call({bye, DialogId}, _From, State) ->
    BYE = sip_ua:create_request('BYE', DialogId),
    {ok, _Id} = sip_ua:send_request(BYE),
    {reply, ok, State}.

-spec handle_response(#sip_request{}, #sip_response{}, reference(), #state{}) -> {noreply, #state{}}.
handle_response(#sip_request{method = 'BYE'}, Response, _RequestId, State) ->
    io:format("HANG: Got ~w response on BYE from ~s~n", [Response#sip_response.status, to(Response)]),
    {noreply, State};

handle_response(#sip_request{method = 'INVITE'}, #sip_response{status = Status} = Response, _RequestId, State)
  when Status >= 100, Status =< 199 ->
    io:format("HANG: Progress from ~s: ~w ~s~n",
              [to(Response), Status, binary_to_list(Response#sip_response.reason)]),
    {noreply, State};

handle_response(#sip_request{method = 'INVITE'}, #sip_response{status = Status} = Response, _RequestId, State) when Status >= 200, Status =< 299 ->
    io:format("HANG: Got success response ~s: ~w ~s~n",
              [to(Response), Status, binary_to_list(Response#sip_response.reason)]),

    ACK = sip_ua:create_ack(Response),
    % put session in ACK
    %ACK2 = sip_message:append_header('content-type', <<"application/sdp">>, ACK),
    %ACK3 = ACK2#sip_request{body = sdp()},
    {ok, _Ref} = sip_ua:send_request(ACK),

    {noreply, State};
handle_response(#sip_request{method = 'INVITE'}, #sip_response{status = Status} = Response, _RequestId, State) when Status >= 300 ->
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
      "c=IN IP4 ", Self/binary, "\r\n",
      "t=0 0\r\n",
      "m=audio ", Port/binary, " RTP/AVP 112 111 110 0 3 8 101\r\n",
      "a=rtpmap:112 speex/32000/1\r\n",
      "a=fmtp:112 vbr=on\r\n",
      "a=rtpmap:111 speex/16000/1\r\n",
      "a=fmtp:111 vbr=on\r\n",
      "a=rtpmap:110 speex/8000/1\r\n",
      "a=fmtp:110 vbr=on\r\n",
      "a=rtpmap:0 PCMU/8000/1\r\n",
      "a=rtpmap:3 GSM/8000/1\r\n",
      "a=rtpmap:8 PCMA/8000/1\r\n",
      "a=rtpmap:101 telephone-event/8000/1\r\n",
      "a=fmtp:101 0-11\r\n">>.
