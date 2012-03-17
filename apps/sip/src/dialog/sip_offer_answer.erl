%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP offer/answer detection
%%% @end
%%% @copyright 2011-2012 Ivan Dubrov. See LICENSE file.
%%% @reference See <a href="http://tools.ietf.org/html/rfc6337">RFC 6337</a> for different offer/answer patterns.
-module(sip_offer_answer).

%% API
-export([validate_request/1, validate_response/2]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%% API

-spec validate_request(#sip_request{}) ->
          offer |
          answer |
          ok.
%% @doc Validate the request according to the offer/answer model.
%% Return values:
%% `answer' -- request MUST contain an answer
%% `offer' -- request MUST contain an offer
%% `ok' -- request does not contain offer nor answer
%% @end
validate_request(Request = #sip_request{method = Method}) when
  Method =:= 'INVITE'; Method =:= 'UPDATE'; Method =:= 'ACK' ->

    #sip_hdr_disposition{type = ReqDisp} = sip_message:disposition(Request),
    case ReqDisp of
        session when Method =:= 'INVITE'; Method =:= 'UPDATE' ->
            offer;

        % FIXME: should make sure session was offered in 2xx response?
        session when Method =:= 'ACK' ->
            answer;

        _Other ->
            ok
    end;
validate_request(_Request) ->
    ok.

-spec validate_response(#sip_request{}, #sip_response{}) ->
          offer |
          answer |
          cancel |
          ok.
%% @doc Validate the response according to the offer/answer model.
%% Return values:
%% `answer' -- response MUST contain an answer
%% `offer' -- response MUST contain an offer
%% `cancel' -- response cancels the offer made by request
%% `ok' -- response does not contain offer nor answer
%% @end
validate_response(Request = #sip_request{method = Method}, #sip_response{status = Status})
  when Method =:= 'INVITE' orelse Method =:= 'UPDATE',
       Status >= 200, Status =< 299 ->

    case sip_message:disposition(Request) of
        #sip_hdr_disposition{type = session} ->
            answer; % response must contain an answer
        _Other when Method =:= 'INVITE' ->
            offer; % response must contain an offer

        _Other ->
            ok
    end;
validate_response(Request = #sip_request{method = Method}, #sip_response{status = Status})
  when Method =:= 'INVITE' orelse Method =:= 'UPDATE', Status >= 300 ->

    #sip_hdr_disposition{type = ReqDisp} = sip_message:disposition(Request),
    case ReqDisp of
        session ->
            cancel; % cancel the offer made by the request

        _Other ->
            ok
    end;
validate_response(_Request, _Response) ->
    ok.

%% Tests
-ifdef(TEST).

-spec offer_answer_invite_test_() -> list().
offer_answer_invite_test_() ->
    Disposition = {'content-disposition', #sip_hdr_disposition{type = session}},
    InviteSession = #sip_request{method = 'INVITE', headers = [Disposition]},
    AckSession = #sip_request{method = 'ACK', headers = [Disposition]},
    InviteNoSession = #sip_request{method = 'INVITE'},
    ResponseOk = #sip_response{status = 200, headers = [Disposition]},
    ResponseErr = #sip_response{status = 300},
    [% pattern 1, INVITE contains offer, response contains answer
     ?_assertEqual(offer, validate_request(InviteSession)),
     ?_assertEqual(answer, validate_response(InviteSession, ResponseOk)),
     % pattern 1', INVITE contains offer, response cancels it
     ?_assertEqual(offer, validate_request(InviteSession)),
     ?_assertEqual(cancel, validate_response(InviteSession, ResponseErr)),
     % pattern 2, INVITE does not contain offer, response contains offer
     ?_assertEqual(ok, validate_request(InviteNoSession)),
     ?_assertEqual(offer, validate_response(InviteNoSession, ResponseOk)),
     ?_assertEqual(answer, validate_request(AckSession)),
     % pattern 2', INVITE does not contain offer, response is error response
     ?_assertEqual(ok, validate_request(InviteNoSession)),
     ?_assertEqual(ok, validate_response(InviteNoSession, ResponseErr))
     ].

-spec offer_answer_update_test_() -> list().
offer_answer_update_test_() ->
    Disposition = {'content-disposition', #sip_hdr_disposition{type = session}},
    UpdateSession = #sip_request{method = 'UPDATE', headers = [Disposition]},
    UpdateNoSession = #sip_request{method = 'UPDATE'},
    ResponseOk = #sip_response{status = 200, headers = [Disposition]},
    ResponseErr = #sip_response{status = 300},
    [% pattern 6, UPDATE contains offer, response contains answer
     ?_assertEqual(offer, validate_request(UpdateSession)),
     ?_assertEqual(answer, validate_response(UpdateSession, ResponseOk)),
     % pattern 6', UPDATE contains offer, response cancels it
     ?_assertEqual(offer, validate_request(UpdateSession)),
     ?_assertEqual(cancel, validate_response(UpdateSession, ResponseErr)),
     % pattern 6'', UPDATE does not contain offer
     ?_assertEqual(ok, validate_request(UpdateNoSession)),
     ?_assertEqual(ok, validate_response(UpdateNoSession, ResponseOk))
     ].

-spec offer_answer_other_test_() -> list().
offer_answer_other_test_() ->
    Options = #sip_request{method = 'OPTIONS'},
    ResponseOk = #sip_response{status = 200},
    [?_assertEqual(ok, validate_request(Options)),
     ?_assertEqual(ok, validate_response(Options, ResponseOk))
     ].

-endif.