%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP offer/answer detection
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
-module(sip_offer_answer).

%% API
-export([process_request/1, process_response/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%% API


-spec process_request(#sip_request{}) ->
          offer |
          answer |
          ok.
%% @doc Validate the request according to the offer/answer model.
%% @end
process_request(Request = #sip_request{method = Method}) when
  Method =:= 'INVITE'; Method =:= 'UPDATE'; Method =:= 'ACK' ->

    #sip_hdr_disposition{type = ReqDisp} = sip_message:disposition(Request),
    case ReqDisp of
        session when Method =:= 'INVITE'; Method =:= 'UPDATE' ->
            offer;

        % FIXME: should make sure session was offered in 2xx response?
        session when Method =:= 'ACK' ->
            answer;

        false ->
            ok
    end;
process_request(_Request) ->
    ok.

-spec process_response(#sip_request{}, #sip_response{}) ->
          offer |
          answer |
          answer_expected |
          answer_cancel |
          ok.
%% @doc Validate the response according to the offer/answer model.
%% @end
process_response(Request = #sip_request{method = Method}, Response = #sip_response{status = Status})
  when Method =:= 'INVITE' orelse Method =:= 'UPDATE',
       Status >= 200, Status =< 299 ->

    #sip_hdr_disposition{type = ReqDisp} = sip_message:disposition(Request),
    #sip_hdr_disposition{type = RespDisp} = sip_message:disposition(Response),
    case {ReqDisp, RespDisp} of
        {session, session} ->
            answer;

        {session, _Other} ->
            answer_expected;

        {_Other, session} when Method =:= 'INVITE' ->
            offer;

        _ ->
            ok
    end;
process_response(Request = #sip_request{method = Method}, Response = #sip_response{status = Status})
  when Method =:= 'INVITE' orelse Method =:= 'UPDATE', Status >= 300 ->

    #sip_hdr_disposition{type = ReqDisp} = sip_message:disposition(Request),
    case ReqDisp of
        session ->
            answer_cancel;

        _Other ->
            ok
    end;
process_response(_Request, _Response) ->
    ok.
