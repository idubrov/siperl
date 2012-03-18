%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Internal module to support sessions from the UA side (offer/answer interactions)
%%%
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_ua_session).
-compile({parse_transform, do}).

%% Internal API
-export([process_request/2, process_invite/3, process_response/3]).

%% Include files
-include("../sip_common.hrl").
-include("sip.hrl").

%%-----------------------------------------------------------------
%% Internal API
%%-----------------------------------------------------------------

%% @doc Update session based on the offer/answer from the remote side
%% @end
-spec process_request(uac | uas, #sip_request{}) -> ok.
process_request(UA, Request) when is_record(Request, sip_request) ->
    case sip_message:is_within_dialog(Request) of
        true ->
            Result = analyze_request(Request),
            process_internal(UA, Result, Request, Request);
        false ->
            % INVITEs coming outside of the dialog should be processed
            % through `process_invite/3' when response is received.
            ok
    end.

%% @doc Update session based on the offer from the out-of-dialog INVITE
%% Since initial INVITE is outside of the dialog, we do not have a place
%% to record the offer. Therefore, we delay such INVITE processing unless
%% we receive 2xx response.
%% All other offer/answer interactions are always inside an established
%% dialog.
%% @end
-spec process_invite(uac | uas, #sip_request{}, #sip_response{}) -> ok.
process_invite(UA, #sip_request{method = 'INVITE'} = Request, Response)
  when is_record(Response, sip_response) ->
    false = sip_message:is_within_dialog(Request),
    Result = analyze_request(Request),
    process_internal(UA, Result, Request, Response).

%% @doc Update session based on the offer/answer from the local side
%% @end
-spec process_response(uac | uas, #sip_request{}, #sip_response{}) -> ok.
process_response(UA, Request, Response)
  when is_record(Request, sip_request),
       is_record(Response, sip_response) ->
    Result = analyze_response(Request, Response),
    process_internal(UA, Result, Response, Response).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% @doc Update session based on the offer/answer that could be present in message
%% @end
process_internal(UA, Result, Msg, DialogMsg) ->
    case Result of
        % no offer/answer, nothing to update
        ok -> ok;
        % cancel the previous offer
        cancel ->
            DialogId = sip_dialog:dialog_id(UA, DialogMsg),
            io:format("FIXME: ~p ~p SESSION CANCEL~n", [UA, DialogId]),
            ok;
        % offer or answer expected
        Kind when Kind =:= offer; Kind =:= answer ->
            DialogId = sip_dialog:dialog_id(UA, DialogMsg),
            case sip_message:session(Msg) of
                false ->
                    {error, session_expected};
                #sip_session_desc{} = SessionDesc ->
                    io:format("FIXME: ~p ~p SESSION ~p ~p~n", [UA, DialogId, Kind, SessionDesc]),
                    ok
            end
    end.

%%-----------------------------------------------------------------
%% Offer/answer interactions
%%-----------------------------------------------------------------

%% @doc Analyze the request according to the offer/answer model.
%% Return values:
%% `answer' -- request MUST contain an answer
%% `offer' -- request MUST contain an offer
%% `ok' -- request does not contain offer nor answer
%% @end
analyze_request(Request = #sip_request{method = Method}) when
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
analyze_request(_Request) ->
    ok.

%% @doc Validate the response according to the offer/answer model.
%% Return values:
%% `answer' -- response MUST contain an answer
%% `offer' -- response MUST contain an offer
%% `cancel' -- response cancels the offer made by request
%% `ok' -- response does not contain offer nor answer
%% @end
analyze_response(Request = #sip_request{method = Method}, #sip_response{status = Status})
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
analyze_response(Request = #sip_request{method = Method}, #sip_response{status = Status})
  when Method =:= 'INVITE' orelse Method =:= 'UPDATE', Status >= 300 ->

    #sip_hdr_disposition{type = ReqDisp} = sip_message:disposition(Request),
    case ReqDisp of
        session ->
            cancel; % cancel the offer made by the request

        _Other ->
            ok
    end;
analyze_response(_Request, _Response) ->
    ok.

%% Tests
-ifdef(TEST).

-spec analyze_invite_test_() -> list().
analyze_invite_test_() ->
    Disposition = {'content-disposition', #sip_hdr_disposition{type = session}},
    InviteSession = #sip_request{method = 'INVITE', headers = [Disposition]},
    AckSession = #sip_request{method = 'ACK', headers = [Disposition]},
    InviteNoSession = #sip_request{method = 'INVITE'},
    ResponseOk = #sip_response{status = 200, headers = [Disposition]},
    ResponseErr = #sip_response{status = 300},
    [% pattern 1, INVITE contains offer, response contains answer
     ?_assertEqual(offer, analyze_request(InviteSession)),
     ?_assertEqual(answer, analyze_response(InviteSession, ResponseOk)),
     % pattern 1', INVITE contains offer, response cancels it
     ?_assertEqual(offer, analyze_request(InviteSession)),
     ?_assertEqual(cancel, analyze_response(InviteSession, ResponseErr)),
     % pattern 2, INVITE does not contain offer, response contains offer
     ?_assertEqual(ok, analyze_request(InviteNoSession)),
     ?_assertEqual(offer, analyze_response(InviteNoSession, ResponseOk)),
     ?_assertEqual(answer, analyze_request(AckSession)),
     % pattern 2', INVITE does not contain offer, response is error response
     ?_assertEqual(ok, analyze_request(InviteNoSession)),
     ?_assertEqual(ok, analyze_response(InviteNoSession, ResponseErr))
     ].

-spec analyze_update_test_() -> list().
analyze_update_test_() ->
    Disposition = {'content-disposition', #sip_hdr_disposition{type = session}},
    UpdateSession = #sip_request{method = 'UPDATE', headers = [Disposition]},
    UpdateNoSession = #sip_request{method = 'UPDATE'},
    ResponseOk = #sip_response{status = 200, headers = [Disposition]},
    ResponseErr = #sip_response{status = 300},
    [% pattern 6, UPDATE contains offer, response contains answer
     ?_assertEqual(offer, analyze_request(UpdateSession)),
     ?_assertEqual(answer, analyze_response(UpdateSession, ResponseOk)),
     % pattern 6', UPDATE contains offer, response cancels it
     ?_assertEqual(offer, analyze_request(UpdateSession)),
     ?_assertEqual(cancel, analyze_response(UpdateSession, ResponseErr)),
     % pattern 6'', UPDATE does not contain offer
     ?_assertEqual(ok, analyze_request(UpdateNoSession)),
     ?_assertEqual(ok, analyze_response(UpdateNoSession, ResponseOk))
     ].

-spec analyze_other_test_() -> list().
analyze_other_test_() ->
    Options = #sip_request{method = 'OPTIONS'},
    ResponseOk = #sip_response{status = 200},
    [?_assertEqual(ok, analyze_request(Options)),
     ?_assertEqual(ok, analyze_response(Options, ResponseOk))
     ].

-endif.