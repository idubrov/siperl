%%%----------------------------------------------------------------
%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc
%%% SIP application core.
%%% @end
%%% @copyright 2011 Ivan Dubrov
%%%----------------------------------------------------------------
-module(sip_cores).

%% Include files
-include("sip_common.hrl").
-include("sip.hrl").

%% API
-export([register_core/1, lookup_core/1]).

-spec register_core(#sip_core_info{}) -> ok.
register_core(Info) ->
    gproc:add_local_property(?CORE_PROPERTY, Info),
    ok.

-spec lookup_core(#sip_message{}) -> {ok, pid(), #sip_core_info{}} | undefined.
lookup_core(Msg) when is_record(Msg, sip_message) ->
    % SIP core is any process with `core_registration' property registered
    % value of the property is `sip_core_info' record
    Regs = gproc:lookup_local_properties(?CORE_PROPERTY),
    DropFun = fun ({_Pid, #sip_core_info{is_applicable = IsApplicable}}) -> not IsApplicable(Msg) end,
    FilteredRegs = lists:dropwhile(DropFun, Regs),
    case FilteredRegs of
        [{Pid, Reg} | _Rest] -> {ok, Pid, Reg};
        [] -> undefined
    end.

