%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc SIP headers parsing, formating and utility functions to construct
%%% header values
%%%
%%% @end
%%% @reference See <a href="http://tools.ietf.org/html/rfc3261#section-20">RFC 3261</a> for details.
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.
-module(sip_headers).

%% Exports
-export([parse_headers/1, format_headers/1]).
-export([parse/2, format/2]).
-export([media/3, language/2, encoding/2, auth/2, info/2, via/3, cseq/2, address/3, retry/3]).
-export([add_tag/3]).

%% Includes
-include("../sip_common.hrl").
-include("sip_syntax.hrl").
-include("sip.hrl").

%% API functions

%% @doc Split binary into list of headers
%%
%% Convert binary containing headers into list of non-parsed headers
%% (with binary values). Binary must contain a sequence headers, each
%% header terminated by `CRLF'. Empty binary is valid argument that
%% results in empty list returned.
%% @end
-spec parse_headers(binary()) -> [{Name :: sip_name(), Value :: term()}].
parse_headers(<<>>) -> [];
parse_headers(Headers) when is_binary(Headers) ->
    Pos = size(Headers) - 2,
    <<Headers2:Pos/binary, "\r\n">> = Headers,
    Lines = binary:split(Headers2, <<"\r\n">>, [global]),
    lists:reverse(lists:foldl(fun (Bin, List) -> fold_header(Bin, List) end, [], Lines)).

%% @doc Formats headers into the binary
%%
%% For supported header representations, see {@link parse/2}/{@link format/2} functions.
%% @end
-spec format_headers([{sip_name(), term()}]) -> binary().
format_headers(Headers) ->
    << <<(process(fn, Name, ignore))/binary, ": ",
         (format(Name, Value))/binary, "\r\n">> ||
       {Name, Value} <- Headers>>.

%%-----------------------------------------------------------------
%% Header parsing/format functions
%%-----------------------------------------------------------------

-spec parse('accept', binary()) -> [#sip_hdr_mediatype{}];
           ('accept-encoding', binary()) -> [#sip_hdr_encoding{}];
           ('accept-language', binary()) -> [#sip_hdr_language{}];
           ('alert-info', binary()) -> [#sip_hdr_info{}];
           ('allow', binary()) -> [Method :: sip_name()];
           ('authentication-info', binary()) -> #sip_hdr_auth{};
           ('authorization', binary()) -> #sip_hdr_auth{};
           ('call-id', binary()) -> binary();
           ('call-info', binary()) -> [#sip_hdr_info{}];
           ('contact', binary()) -> [#sip_hdr_address{}]; % FIXME '*'
           ('content-disposition', binary()) -> #sip_hdr_disposition{};
           ('content-encoding', binary()) -> [ContentCoding :: sip_name()];
           ('content-language', binary()) -> [LanguageTag :: sip_name()];
           ('content-length', binary()) -> integer();
           ('content-type', binary()) -> #sip_hdr_mediatype{};
           ('cseq', binary()) -> #sip_hdr_cseq{};
           ('date', binary()) -> calendar:t_datetime();
           ('error-info', binary()) -> [#sip_hdr_info{}];
           ('expires', binary()) -> integer();
           ('from', binary()) -> #sip_hdr_address{};
           ('in-reply-to', binary()) -> [CallID :: binary()];
           ('max-forwards', binary()) -> integer();
           ('min-expires', binary()) -> integer();
           ('mime-version', binary()) -> {Major :: integer(), Minor :: integer()};
           ('organization', binary()) -> Organization :: binary();
           ('priority', binary()) -> Priority :: sip_name();
           ('proxy-authenticate', binary()) -> #sip_hdr_auth{};
           ('proxy-authorization', binary()) -> #sip_hdr_auth{};
           ('proxy-require', binary()) -> [OptionTag :: sip_name()];
           ('record-route', binary()) -> [#sip_hdr_address{}];
           ('reply-to', binary()) -> #sip_hdr_address{};
           ('require', binary()) -> [OptionTag :: sip_name()];
           ('retry-after', binary()) -> #sip_hdr_retry{};
           ('route', binary()) -> [#sip_hdr_address{}];
           ('server', binary()) -> Server :: binary();
           ('subject', binary()) -> Subject :: binary();
           ('supported', binary()) -> [OptionTag :: sip_name()];
           ('timestamp', binary()) -> #sip_hdr_timestamp{};
           ('to', binary()) -> #sip_hdr_address{};
           ('unsupported', binary()) -> [OptionTag :: sip_name()];
           ('user-agent', binary()) -> UserAgent :: binary();
           ('via', binary()) -> [#sip_hdr_via{}];
           ('warning', binary()) -> [#sip_hdr_warning{}];
           ('www-authenticate', binary()) -> #sip_hdr_auth{};
           (Other :: sip_name(), binary()) -> binary();
           (Name :: sip_name(), Parsed :: any()) -> Parsed :: any().
%% @doc Parse binary header value into the Erlang term representation
%%
%% See type specification for information about which term is used to represent
%% particular header value.
%% @end
parse(Name, Bin) -> process(p, Name, Bin).

-spec format('accept', [#sip_hdr_mediatype{}]) -> binary();
           ('accept-encoding', [#sip_hdr_encoding{}]) -> binary();
           ('accept-language', [#sip_hdr_language{}]) -> binary();
           ('alert-info', [#sip_hdr_info{}]) -> binary();
           ('allow', [Method :: sip_name()]) -> binary();
           ('authentication-info', #sip_hdr_auth{}) -> binary();
           ('authorization', #sip_hdr_auth{}) -> binary();
           ('call-id', binary()) -> binary();
           ('call-info', [#sip_hdr_info{}]) -> binary();
           ('contact', [#sip_hdr_address{}]) -> binary(); % FIXME '*'
           ('content-disposition', #sip_hdr_disposition{}) -> binary();
           ('content-encoding', [ContentCoding :: sip_name()]) -> binary();
           ('content-language', [LanguageTag :: sip_name()]) -> binary();
           ('content-length', integer()) -> binary();
           ('content-type', #sip_hdr_mediatype{}) -> binary();
           ('cseq', #sip_hdr_cseq{}) -> binary();
           ('date', calendar:t_datetime()) -> binary();
           ('error-info', [#sip_hdr_info{}]) -> binary();
           ('expires', integer()) -> binary();
           ('from', #sip_hdr_address{}) -> binary();
           ('in-reply-to', [CallID :: binary()]) -> binary();
           ('max-forwards', integer()) -> binary();
           ('min-expires', integer()) -> binary();
           ('mime-version', {Major :: integer(), Minor :: integer()}) -> binary();
           ('organization', Organization :: binary()) -> binary();
           ('priority', Priority :: sip_name()) -> binary();
           ('proxy-authenticate', #sip_hdr_auth{}) -> binary();
           ('proxy-authorization', #sip_hdr_auth{}) -> binary();
           ('proxy-require', [OptionTag :: sip_name()]) -> binary();
           ('record-route', [#sip_hdr_address{}]) -> binary();
           ('reply-to', #sip_hdr_address{}) -> binary();
           ('require', [OptionTag :: sip_name()]) -> binary();
           ('retry-after', #sip_hdr_retry{}) -> binary();
           ('route', [#sip_hdr_address{}]) -> binary();
           ('server', Server :: binary()) -> binary();
           ('subject', Subject :: binary()) -> binary();
           ('supported', [OptionTag :: sip_name()]) -> binary();
           ('timestamp', #sip_hdr_timestamp{}) -> binary();
           ('to', #sip_hdr_address{}) -> binary();
           ('unsupported', [OptionTag :: sip_name()]) -> binary();
           ('user-agent', UserAgent :: binary()) -> binary();
           ('via', [#sip_hdr_via{}]) -> binary();
           ('warning', [#sip_hdr_warning{}]) -> binary();
           ('www-authenticate', #sip_hdr_auth{}) -> binary();
           (Other :: sip_name(), binary()) -> binary();
           (Name :: sip_name(), Parsed :: any()) -> binary().
%% @doc Format header value into the binary.
%% @end
format(Name, [Value]) -> process(f, Name, Value);
format(Name, [Top | Rest]) ->
    Joiner =
        fun (Elem, Bin) ->
                 ElemBin = process(f, Name, Elem),
                 <<Bin/binary, ?COMMA, ?SP, ElemBin/binary>>
        end,
    TopBin = process(f, Name, Top),
    lists:foldl(Joiner, TopBin, Rest);
format(Name, Value) -> process(f, Name, Value).

%% @doc Parse/format header name/value
%%
%% Parsing/formatting merged into single function for better locality of changes.
%% @end
-spec process(p | f | pn | fn, Name :: sip_name(), Value :: any()) -> term().

% Default header processing
process(p, _Name, Header) when not is_binary(Header) -> Header; % already parsed
process(f, _Name, Value) when is_binary(Value) -> Value; % already formatted

%% 20.1 Accept
%% http://tools.ietf.org/html/rfc3261#section-20.1
process(fn, 'accept', _Ignore) -> <<"Accept">>;
process(p, 'accept', Bin) ->
    {Media, Rest} = parse_media_range(Bin, fun parse_q_param/2),
    parse_list('accept', Media, Rest);

process(f, 'accept', Accept) when is_record(Accept, sip_hdr_mediatype) ->
    Type = sip_syntax:format_name(Accept#sip_hdr_mediatype.type),
    SubType = sip_syntax:format_name(Accept#sip_hdr_mediatype.subtype),
    append_params(<<Type/binary, ?SLASH, SubType/binary>>, Accept#sip_hdr_mediatype.params);

%% 20.2 Accept-Encoding
%% http://tools.ietf.org/html/rfc3261#section-20.2
process(fn, 'accept-encoding', _Ignore) -> <<"Accept-Encoding">>;
process(p, 'accept-encoding', Bin) ->
    {EncodingBin, Params, Rest} = parse_accept(Bin),
    Encoding = encoding(sip_syntax:parse_name(EncodingBin), Params),
    parse_list('accept-encoding', Encoding, Rest);

process(f, 'accept-encoding', Accept) when is_record(Accept, sip_hdr_encoding) ->
    Encoding = sip_syntax:format_name(Accept#sip_hdr_encoding.encoding),
    append_params(Encoding, Accept#sip_hdr_encoding.params);

%% 20.3 Accept-Language
%% http://tools.ietf.org/html/rfc3261#section-20.3
process(fn, 'accept-language', _Ignore) -> <<"Accept-Language">>;
process(p, 'accept-language', Bin) ->
    {Language, Rest} = parse_language(sip_binary:trim_leading(Bin)),
    {Params, Rest2} = parse_params(Rest, fun parse_q_param/2),
    parse_list('accept-language', language(Language, Params), Rest2);

process(f, 'accept-language', Accept) when is_record(Accept, sip_hdr_language) ->
    LangBin = sip_syntax:format_name(Accept#sip_hdr_language.language),
    append_params(LangBin, Accept#sip_hdr_language.params);

%% 20.4 Alert-Info
%% http://tools.ietf.org/html/rfc3261#section-20.4
process(fn, 'alert-info', _Ignore) -> <<"Alert-Info">>;
process(p, 'alert-info', Bin) ->
    parse_info('alert-info', Bin, fun parse_generic_param/2);

process(f, 'alert-info', Info) when is_record(Info, sip_hdr_info) ->
    URI = sip_uri:format(Info#sip_hdr_info.uri),
    Bin = <<?LAQUOT, URI/binary, ?RAQUOT>>,
    append_params(Bin, Info#sip_hdr_info.params);

%% 20.5 Allow
%% http://tools.ietf.org/html/rfc3261#section-20.5
process(fn, 'allow', _Ignore) -> <<"Allow">>;
process(p, 'allow', Bin) ->
    {MethodBin, Rest} = sip_syntax:parse_token(Bin),
    Method = sip_syntax:parse_name(sip_binary:to_upper(MethodBin)),
    parse_list('allow', Method, Rest);

process(f, 'allow', Allow) ->
    sip_syntax:format_name(Allow);

%% 20.6 Authentication-Info
%% http://tools.ietf.org/html/rfc3261#section-20.6
process(fn, 'authentication-info', _Ignore) -> <<"Authentication-Info">>;
process(p, 'authentication-info', Bin) ->
    parse_auths(Bin);

process(f, 'authentication-info', {_Key, _Value} = Pair) ->
    format_auth(Pair);

%% 20.7 Authorization
%% http://tools.ietf.org/html/rfc3261#section-20.7
process(fn, 'authorization', _Ignore) -> <<"Authorization">>;
process(p, 'authorization', Bin) ->
    {SchemeBin, Bin2} = sip_syntax:parse_token(Bin),
    % parse scheme, the rest is list of paris param=value
    Scheme = sip_syntax:parse_name(SchemeBin),
    auth(Scheme, parse_auths(Bin2));

process(f, 'authorization', Auth) when is_record(Auth, sip_hdr_auth) ->
    SchemeBin = sip_syntax:format_name(Auth#sip_hdr_auth.scheme),
    [First | Rest] = Auth#sip_hdr_auth.params,
    FirstBin = format_auth(First),
    Fun = fun (Val, Acc) -> <<Acc/binary, ?COMMA, ?SP, (format_auth(Val))/binary>> end,
    lists:foldl(Fun, <<SchemeBin/binary, ?SP, FirstBin/binary>>, Rest);

%% 20.8 Call-ID
%% http://tools.ietf.org/html/rfc3261#section-20.8
process(pn, <<"i">>, _Ignore) -> 'call-id';
process(fn, 'call-id', _Ignore) -> <<"Call-ID">>;
process(p, 'call-id', Bin) -> Bin;

%% 20.9 Call-Info
%% http://tools.ietf.org/html/rfc3261#section-20.9
process(fn, 'call-info', _Ignore) -> <<"Call-Info">>;
process(p, 'call-info', Bin) ->
    ParamFun =
        fun(purpose, Value) -> sip_syntax:parse_name(Value);
           (_Name, Value) -> Value
        end,
    parse_info('call-info', Bin, ParamFun);

process(f, 'call-info', Info) when is_record(Info, sip_hdr_info) ->
    URI = sip_uri:format(Info#sip_hdr_info.uri),
    Bin = <<?LAQUOT, URI/binary, ?RAQUOT>>,
    append_params(Bin, Info#sip_hdr_info.params);

%% 20.10 Contact
%% http://tools.ietf.org/html/rfc3261#section-20.10
process(pn, <<"m">>, _Ignore) -> 'contact';
process(fn, 'contact', _Ignore) -> <<"Contact">>;
process(p, 'contact', <<"*">>) -> '*';
process(p, 'contact', Bin) ->
    {Top, Rest} = parse_address(Bin, fun parse_contact_param/2),
    parse_list('contact', Top, Rest);

process(f, 'contact', '*') -> <<"*">>;
process(f, 'contact', Addr) when is_record(Addr, sip_hdr_address) ->
    format_address(Addr);

%% 20.11 Content-Disposition
%% http://tools.ietf.org/html/rfc3261#section-20.11
process(fn, 'content-disposition', _Ignore) -> <<"Content-Disposition">>;
process(p, 'content-disposition', Bin) ->
    {TypeBin, Rest} = sip_syntax:parse_token(Bin),
    ParamFun = fun(handling, Value) -> sip_syntax:parse_name(Value);
                  (_Name, Value) -> Value
               end,
    {Params, <<>>} = parse_params(Rest, ParamFun),
    Type = sip_syntax:parse_name(TypeBin),
    #sip_hdr_disposition{type = Type, params = Params};

process(f, 'content-disposition', Disp) when is_record(Disp, sip_hdr_disposition) ->
    TypeBin = sip_syntax:format_name(Disp#sip_hdr_disposition.type),
    append_params(TypeBin, Disp#sip_hdr_disposition.params);

%% 20.12 Content-Encoding
%% http://tools.ietf.org/html/rfc3261#section-20.12
process(pn, <<"e">>, _Ignore) -> 'content-encoding';
process(fn, 'content-encoding', _Ignore) -> <<"Content-Encoding">>;
process(p, 'content-encoding', Bin) ->
    {EncodingBin, Rest} = sip_syntax:parse_token(Bin),
    Encoding = sip_syntax:parse_name(sip_binary:to_lower(EncodingBin)),
    parse_list('content-encoding', Encoding, Rest);

process(f, 'content-encoding', Encoding) ->
    sip_syntax:format_name(Encoding);

%% 20.13 Content-Language
%% http://tools.ietf.org/html/rfc3261#section-20.13
process(fn, 'content-language', _Ignore) -> <<"Content-Language">>;
process(p, 'content-language', Bin) ->
    {LangBin, Rest} = sip_syntax:parse_token(Bin),
    {Language, <<>>} = parse_language(LangBin),
    parse_list('content-language', Language, Rest);

process(f, 'content-language', Lang) ->
    sip_syntax:format_name(Lang);

%% 20.14 Content-Length
%% http://tools.ietf.org/html/rfc3261#section-20.14
process(pn, <<"l">>, _Ignore) -> 'content-length';
process(fn, 'content-length', _Ignore) -> <<"Content-Length">>;
process(p, 'content-length', Bin) ->
    sip_binary:binary_to_integer(Bin);

process(f, 'content-length', Length) when is_integer(Length) ->
    sip_binary:integer_to_binary(Length);

%% 20.15 Content-Type
%% http://tools.ietf.org/html/rfc3261#section-20.15
process(pn, <<"c">>, _Ignore) -> 'content-type';
process(fn, 'content-type', _Ignore) -> <<"Content-Type">>;
process(p, 'content-type', Bin) ->
    {Media, <<>>} = parse_media_range(Bin, fun parse_generic_param/2),
    Media;

process(f, 'content-type', CType) when is_record(CType, sip_hdr_mediatype) ->
    Type = sip_syntax:format_name(CType#sip_hdr_mediatype.type),
    SubType = sip_syntax:format_name(CType#sip_hdr_mediatype.subtype),
    append_params(<<Type/binary, ?SLASH, SubType/binary>>, CType#sip_hdr_mediatype.params);

%% 20.16 CSeq
%% http://tools.ietf.org/html/rfc3261#section-20.16
process(fn, 'cseq', _Ignore) -> <<"CSeq">>;
process(p, 'cseq', Bin) ->
    {Sequence, Bin2} = sip_syntax:parse_integer(Bin),
    {MethodBin, <<>>} = sip_syntax:parse_token(Bin2),
    Method = sip_syntax:parse_name(sip_binary:to_upper(MethodBin)),
    cseq(Sequence, Method);

process(f, 'cseq', CSeq) when is_record(CSeq, sip_hdr_cseq) ->
    SequenceBin = sip_binary:integer_to_binary(CSeq#sip_hdr_cseq.sequence),
    MethodBin = sip_syntax:format_name(CSeq#sip_hdr_cseq.method),
    <<SequenceBin/binary, " ", MethodBin/binary>>;

%% 20.17 Date
%% http://tools.ietf.org/html/rfc3261#section-20.17
process(fn, 'date', _Ignore) -> <<"Date">>;
process(p, 'date', Bin) ->
    httpd_util:convert_request_date(binary_to_list(Bin));

process(f, 'date', Date) ->
    Local = calendar:universal_time_to_local_time(Date),
    list_to_binary(httpd_util:rfc1123_date(Local));

%% 20.18 Error-Info
%% http://tools.ietf.org/html/rfc3261#section-20.18
process(fn, 'error-info', _Ignore) -> <<"Error-Info">>;
process(p, 'error-info', Bin) ->
    parse_info('call-info', Bin, fun parse_generic_param/2);

process(f, 'error-info', Info) when is_record(Info, sip_hdr_info) ->
    URI = sip_uri:format(Info#sip_hdr_info.uri),
    Bin = <<?LAQUOT, URI/binary, ?RAQUOT>>,
    append_params(Bin, Info#sip_hdr_info.params);

%% 20.19 Expires
%% http://tools.ietf.org/html/rfc3261#section-20.19
process(fn, 'expires', _Ignore) -> <<"Expires">>;
process(p, 'expires', Bin) ->
    sip_binary:binary_to_integer(Bin);

process(f, 'expires', Length) when is_integer(Length) ->
    sip_binary:integer_to_binary(Length);

%% 20.20 From
%% http://tools.ietf.org/html/rfc3261#section-20.20
process(pn, <<"f">>, _Ignore) -> 'from';
process(fn, 'from', _Ignore) -> <<"From">>;
process(p, 'from', Bin) ->
    {Top, <<>>} = parse_address(Bin, fun parse_generic_param/2),
    Top;

process(f, 'from', Addr) when is_record(Addr, sip_hdr_address) ->
    format_address(Addr);

%% 20.21 In-Reply-To
%% http://tools.ietf.org/html/rfc3261#section-20.21
process(fn, 'in-reply-to', _Ignore) -> <<"In-Reply-To">>;
process(p, 'in-reply-to', Bin) ->
    Bin2 = sip_binary:trim_leading(Bin),
    Fun = fun (C) -> sip_syntax:is_space_char(C) orelse C =:= ?COMMA end,
    {InReplyTo, Rest} = sip_binary:parse_until(Bin2, Fun),
    parse_list('in-reply-to', InReplyTo, Rest);

%% 20.22 Max-Forwards
%% http://tools.ietf.org/html/rfc3261#section-20.22
process(fn, 'max-forwards', _Ignore) -> <<"Max-Forwards">>;
process(p, 'max-forwards', Bin) ->
    sip_binary:binary_to_integer(Bin);

process(f, 'max-forwards', Hops) when is_integer(Hops) ->
    sip_binary:integer_to_binary(Hops);

%% 20.23 Min-Expires
%% http://tools.ietf.org/html/rfc3261#section-20.23
process(fn, 'min-expires', _Ignore) -> <<"Min-Expires">>;
process(p, 'min-expires', Bin) ->
    sip_binary:binary_to_integer(Bin);

process(f, 'min-expires', Length) when is_integer(Length) ->
    sip_binary:integer_to_binary(Length);

%% 20.24 MIME-Version
%% http://tools.ietf.org/html/rfc3261#section-20.24
process(fn, 'mime-version', _Ignore) -> <<"MIME-Version">>;
process(p, 'mime-version', Bin) ->
    {Major, <<$., MinorBin/binary>>} = sip_syntax:parse_integer(Bin),
    {Major, sip_binary:binary_to_integer(MinorBin)};

process(f, 'mime-version', {Major, Minor}) ->
    MajorBin = sip_binary:integer_to_binary(Major),
    MinorBin = sip_binary:integer_to_binary(Minor),
    <<MajorBin/binary, $., MinorBin/binary>>;

%% 20.25 Organization
%% http://tools.ietf.org/html/rfc3261#section-20.25
process(fn, 'organization', _Ignore) -> <<"Organization">>;
process(p, 'organization', Bin) ->
    sip_binary:trim(Bin);

%% 20.26 Priority
%% http://tools.ietf.org/html/rfc3261#section-20.26
process(fn, 'priority', _Ignore) -> <<"Priority">>;
process(p, 'priority', Bin) ->
    {Priority, <<>>} = sip_syntax:parse_token(Bin),
    sip_syntax:parse_name(Priority);

%% 20.27 Proxy-Authenticate
%% http://tools.ietf.org/html/rfc3261#section-20.27
process(fn, 'proxy-authenticate', _Ignore) -> <<"Proxy-Authenticate">>;
process(p, 'proxy-authenticate', Bin) ->
    {SchemeBin, Bin2} = sip_syntax:parse_token(Bin),
    % parse scheme, the rest is list of paris param=value
    Scheme = sip_syntax:parse_name(SchemeBin),
    auth(Scheme, parse_auths(Bin2));

process(f, 'proxy-authenticate', Auth) when is_record(Auth, sip_hdr_auth) ->
    SchemeBin = sip_syntax:format_name(Auth#sip_hdr_auth.scheme),
    [First | Rest] = Auth#sip_hdr_auth.params,
    FirstBin = format_auth(First),
    Fun = fun (Val, Acc) -> <<Acc/binary, ?COMMA, ?SP, (format_auth(Val))/binary>> end,
    lists:foldl(Fun, <<SchemeBin/binary, ?SP, FirstBin/binary>>, Rest);

%% 20.28 Proxy-Authorization
%% http://tools.ietf.org/html/rfc3261#section-20.28
process(fn, 'proxy-authorization', _Ignore) -> <<"Proxy-Authorization">>;
process(p, 'proxy-authorization', Bin) ->
    % same as Authorization header
    process(p, 'authorization', Bin);

process(f, 'proxy-authorization', Auth) when is_record(Auth, sip_hdr_auth) ->
    % same as Authorization header
    process(f, 'authorization', Auth);

%% 20.29 Proxy-Require
%% http://tools.ietf.org/html/rfc3261#section-20.29
process(fn, 'proxy-require', _Ignore) -> <<"Proxy-Require">>;
process(p, 'proxy-require', Bin) ->
    {ReqBin, Rest} = sip_syntax:parse_token(Bin),
    Ext = sip_syntax:parse_name(sip_binary:to_lower(ReqBin)),
    parse_list('require', Ext, Rest);

process(f, 'proxy-require', Bin) ->
    sip_syntax:format_name(Bin);

%% 20.30 Record-Route
%% http://tools.ietf.org/html/rfc3261#section-20.30
process(fn, 'record-route', _Ignore) -> <<"Record-Route">>;
process(p, 'record-route', Bin) ->
    {Top, Rest} = parse_address(Bin, fun parse_generic_param/2),
    parse_list('record-route', Top, Rest);

process(f, 'record-route', RecordRoute) when is_record(RecordRoute, sip_hdr_address) ->
    format_address(RecordRoute);

%% 20.31 Reply-To
%% http://tools.ietf.org/html/rfc3261#section-20.31
process(fn, 'reply-to', _Ignore) -> <<"Reply-To">>;
process(p, 'reply-to', Bin) ->
    {Top, <<>>} = parse_address(Bin, fun parse_generic_param/2),
    Top;

process(f, 'reply-to', Addr) when is_record(Addr, sip_hdr_address) ->
    format_address(Addr);

%% 20.32 Require
%% http://tools.ietf.org/html/rfc3261#section-20.32
process(fn, 'require', _Ignore) -> <<"Require">>;
process(p, 'require', Bin) ->
    {ExtBin, Rest} = sip_syntax:parse_token(Bin),
    Ext = sip_syntax:parse_name(sip_binary:to_lower(ExtBin)),
    parse_list('require', Ext, Rest);

process(f, 'require', Ext) ->
    sip_syntax:format_name(Ext);

%% 20.33 Retry-After
%% http://tools.ietf.org/html/rfc3261#section-20.33
process(fn, 'retry-after', _Ignore) -> <<"Retry-After">>;
process(p, 'retry-after', Bin) ->
    {Delay, CommentParams} = sip_binary:parse_while(Bin, fun sip_syntax:is_digit_char/1),
    {Comment, ParamsBin} =
        case sip_binary:trim_leading(CommentParams) of
            <<?LPAREN, _/binary>> = CommentBin ->
                sip_syntax:parse_comment_string(CommentBin);
            Rest ->
                {<<>>, Rest}
        end,
    ParamFun = fun(duration, Value) -> sip_binary:binary_to_integer(Value);
                  (_Name, Value) -> Value
               end,
    {Params, <<>>} = parse_params(ParamsBin, ParamFun),
    retry(sip_binary:binary_to_integer(Delay), Comment, Params);

process(f, 'retry-after', Retry) when is_record(Retry, sip_hdr_retry) ->
    Bin = sip_binary:integer_to_binary(Retry#sip_hdr_retry.delay),
    Bin2 = case Retry#sip_hdr_retry.comment of
               <<>> -> Bin;
               Comment ->
                   <<Bin/binary, ?SP, Comment/binary>>
           end,
    append_params(Bin2, Retry#sip_hdr_retry.params);

%% 20.34 Route
%% http://tools.ietf.org/html/rfc3261#section-20.34
process(fn, 'route', _Ignore) -> <<"Route">>;
process(p, 'route', Bin) ->
    {Top, Rest} = parse_address(Bin, fun parse_generic_param/2),
    parse_list('route', Top, Rest);

process(f, 'route', Route) when is_record(Route, sip_hdr_address) ->
    format_address(Route);

%% 20.35 Server
%% http://tools.ietf.org/html/rfc3261#section-20.35
process(fn, 'server', _Ignore) -> <<"Server">>;
process(p, 'server', Bin) -> Bin;

%% 20.36 Subject
%% http://tools.ietf.org/html/rfc3261#section-20.36
process(pn, <<"s">>, _Ignore) -> 'subject';
process(fn, 'subject', _Ignore) -> <<"Subject">>;
process(p, 'subject', Bin) ->
    sip_binary:trim(Bin);

%% 20.37 Supported
%% http://tools.ietf.org/html/rfc3261#section-20.37
process(pn, <<"k">>, _Ignore) -> 'supported';
process(fn, 'supported', _Ignore) -> <<"Supported">>;
process(p, 'supported', Bin) ->
    {ExtBin, Rest} = sip_syntax:parse_token(Bin),
    Ext = sip_syntax:parse_name(sip_binary:to_lower(ExtBin)),
    parse_list('supported', Ext, Rest);

process(f, 'supported', Ext) ->
    sip_syntax:format_name(Ext);

%% 20.38 Timestamp
%% http://tools.ietf.org/html/rfc3261#section-20.38
process(fn, 'timestamp', _Ignore) -> <<"Timestamp">>;
process(p, 'timestamp', Bin) ->
    case sip_syntax:parse_token(Bin) of
        {TimestampBin, <<>>} ->
            timestamp(sip_binary:binary_to_float(TimestampBin), 0.0);
        {TimestampBin, DelayBin} ->
            timestamp(sip_binary:binary_to_float(TimestampBin),
                      sip_binary:binary_to_float(DelayBin))
    end;

process(f, 'timestamp', Timestamp) when is_record(Timestamp, sip_hdr_timestamp) ->
    Bin = sip_binary:float_to_binary(Timestamp#sip_hdr_timestamp.timestamp),
    case Timestamp#sip_hdr_timestamp.delay of
        0.0 -> Bin;
        Delay ->
            DelayBin = sip_binary:float_to_binary(Delay),
            <<Bin/binary, ?SP, DelayBin/binary>>
    end;

%% 20.39 To
%% http://tools.ietf.org/html/rfc3261#section-20.39
process(pn, <<"t">>, _Ignore) -> 'to';
process(fn, 'to', _Ignore) -> <<"To">>;
process(p, 'to', Bin) ->
    {Top, <<>>} = parse_address(Bin, fun parse_generic_param/2),
    Top;

process(f, 'to', Addr) when is_record(Addr, sip_hdr_address) ->
    format_address(Addr);

%% 20.40 Unsupported
%% http://tools.ietf.org/html/rfc3261#section-20.40
process(fn, 'unsupported', _Ignore) -> <<"Unsupported">>;
process(p, 'unsupported', Bin) ->
    {ExtBin, Rest} = sip_syntax:parse_token(Bin),
    Ext = sip_syntax:parse_name(sip_binary:to_lower(ExtBin)),
    parse_list('require', Ext, Rest);

process(f, 'unsupported', Ext) ->
    sip_syntax:format_name(Ext);

%% 20.41 User-Agent
%% http://tools.ietf.org/html/rfc3261#section-20.41
process(fn, 'user-agent', _Ignore) -> <<"User-Agent">>;
process(p, 'user-agent', Bin) -> Bin;

%% 20.42 Via
%% http://tools.ietf.org/html/rfc3261#section-20.42
process(pn, <<"v">>, _Ignore) -> 'via';
process(fn, 'via', _Ignore) -> <<"Via">>;
process(p, 'via', Bin) ->
    {{<<"SIP">>, Version, Transport}, Bin2} = parse_sent_protocol(Bin),
    % Parse parameters (which should start with semicolon)
    {Host, Port, Bin3} = sip_syntax:parse_host_port(Bin2),
    {Params, Rest} = parse_params(Bin3, fun parse_via_param/2),

    Top = #sip_hdr_via{transport = Transport,
                       version = Version,
                       host = Host,
                       port = Port,
                       params = Params},
    parse_list('via', Top, Rest);

process(f, 'via', Via) when is_record(Via, sip_hdr_via) ->
    Version = Via#sip_hdr_via.version,
    Transport = sip_binary:to_upper(atom_to_binary(Via#sip_hdr_via.transport, latin1)),
    Bin = <<"SIP/", Version/binary, $/, Transport/binary>>,
    Host = sip_syntax:format_addr(Via#sip_hdr_via.host),
    Bin2 =
        case Via#sip_hdr_via.port of
            undefined -> <<Bin/binary, ?SP, Host/binary>>;
            Port -> <<Bin/binary, ?SP, Host/binary, ?HCOLON, (sip_binary:integer_to_binary(Port))/binary>>
        end,
    append_params(Bin2, Via#sip_hdr_via.params);

%% 20.43 Warning
%% http://tools.ietf.org/html/rfc3261#section-20.43
process(fn, 'warning', _Ignore) -> <<"Warning">>;
process(p, 'warning', Bin) ->
    {Code, Bin2} = sip_syntax:parse_integer(sip_binary:trim_leading(Bin)),
    {Agent, Bin3} = sip_binary:parse_until(sip_binary:trim_leading(Bin2), fun sip_syntax:is_space_char/1),
    {Text, Rest} = sip_syntax:parse_quoted_string(Bin3),
    parse_list('warning', warning(Code, Agent, Text), Rest);

process(f, 'warning', Warning) when is_record(Warning, sip_hdr_warning) ->
    CodeBin = sip_binary:integer_to_binary(Warning#sip_hdr_warning.code),
    Agent = Warning#sip_hdr_warning.agent,
    Text = sip_syntax:quote_string(Warning#sip_hdr_warning.text),
    <<CodeBin/binary, ?SP, Agent/binary, ?SP, Text/binary>>;

%% 20.44 WWW-Authenticate
%% http://tools.ietf.org/html/rfc3261#section-20.44
process(fn, 'www-authenticate', _Ignore) -> <<"WWW-Authenticate">>;
process(p, 'www-authenticate', Bin) ->
    process(p, 'proxy-authenticate', Bin);

process(f, 'www-authenticate', Auth) when is_record(Auth, sip_hdr_auth) ->
    process(f, 'proxy-authenticate', Auth);

% Default header processing
process(p, _Name, Header) -> Header; % cannot parse, leave as is
process(f, _Name, Value) -> format_value(Value); % do our best to format header value
process(pn, Name, _Ignore) when is_binary(Name) -> sip_syntax:parse_name(Name);
process(fn, Name, _Ignore) when is_binary(Name) -> Name;
process(fn, Name, _Ignore) when is_atom(Name) -> atom_to_binary(Name, utf8).


%% Internal helpers to parse header parts

%% sent-protocol     =  protocol-name SLASH protocol-version
%%                      SLASH transport
%% protocol-name     =  "SIP" / token
%% protocol-version  =  token
%% transport         =  "UDP" / "TCP" / "TLS" / "SCTP"
%%                      / other-transport
%% other-transport   =  token
parse_sent_protocol(Bin) ->
    {Protocol, <<$/, Bin2/binary>>} = sip_syntax:parse_token(Bin),
    {Version, <<$/, Bin3/binary>>} = sip_syntax:parse_token(Bin2),
    {Transport, Bin4} = sip_syntax:parse_token(Bin3),
    Transport2 = sip_syntax:parse_name(sip_binary:to_lower(Transport)),
    {{Protocol, Version, Transport2}, Bin4}.

%% Parse parameters lists
%% *( SEMI param )
%% param  =  token [ EQUAL value ]
%% value  =  token / host / quoted-string
parse_params(Bin, ParseFun) ->
    parse_params_loop(sip_binary:trim_leading(Bin), ParseFun, []).

parse_params_loop(<<?SEMI, Bin/binary>>, ParseFun, List) ->
    {NameBin, MaybeValue} = sip_syntax:parse_token(Bin),
    Name = sip_syntax:parse_name(NameBin),
    {Value, Rest} =
        case MaybeValue of
            % Parameter with value
            <<?EQUAL, Bin2/binary>> ->
                parse_token_or_quoted(Bin2);
            % Parameter without a value ('true' value)
            Next ->
                {true, Next}
        end,
    ParsedValue = ParseFun(Name, Value),
    Property = proplists:property(Name, ParsedValue),
    parse_params_loop(Rest, ParseFun, [Property | List]);
parse_params_loop(Bin, _ParseFun, List) ->
    {lists:reverse(List), sip_binary:trim_leading(Bin)}.

parse_token_or_quoted(Bin) ->
    case sip_binary:trim_leading(Bin) of
        <<?DQUOTE, _Rest/binary>> -> sip_syntax:parse_quoted_string(Bin);
        _Token -> sip_syntax:parse_token(Bin)
    end.

%% Parse address-like headers (`Contact:', `To:', `From:')
parse_address(Bin, ParamFun) ->
    {Display, URI, Bin2} = parse_address_uri(sip_binary:trim_leading(Bin)),
    {Params, Bin3} = parse_params(Bin2, ParamFun),
    Value = address(sip_binary:trim(Display), URI, Params),
    {Value, Bin3}.

parse_address_uri(<<?DQUOTE, _/binary>> = Bin) ->
    % name-addr with quoted-string display-name
    {Display, <<?LAQUOT, Rest/binary>>} = sip_syntax:parse_quoted_string(Bin),
    {URI, <<?RAQUOT, Params/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
    {Display, URI, Params};
parse_address_uri(<<?LAQUOT, Rest/binary>>) ->
    % name-addr without display-name
    {URI, <<?RAQUOT, Params/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
    {<<>>, URI, Params};
parse_address_uri(Bin) ->
    % either addr-spec or name-addr with token-based display-name
    case sip_binary:parse_until(Bin, ?LAQUOT) of
        {_Any, <<>>} ->
            % addr-spec
            % Section 20
            % If the URI is not enclosed in angle brackets, any semicolon-delimited
            % parameters are header-parameters, not URI parameters.
            % so, parse until comma (next header value), space character or semicolon
            Fun = fun (C) -> sip_syntax:is_space_char(C) orelse C =:= ?SEMI orelse C =:= ?COMMA end,

            {URI, Params} = sip_binary:parse_until(Bin, Fun),
            {<<>>, URI, Params};

        {Display, <<?LAQUOT, Rest/binary>>} ->
            % name-addr with token-based display-name
            {URI, <<?RAQUOT, Params/binary>>} = sip_binary:parse_until(Rest, ?RAQUOT),
            {Display, URI, Params}
    end.

format_address(Addr) ->
    URIBin = sip_uri:format(Addr#sip_hdr_address.uri),
    Bin = case Addr#sip_hdr_address.display_name of
              <<>> -> <<?LAQUOT, URIBin/binary, ?RAQUOT>>;
              DisplayName ->
                  Quoted = sip_syntax:quote_string(DisplayName),
                  <<Quoted/binary, " ", ?LAQUOT, URIBin/binary, ?RAQUOT>>
          end,
    append_params(Bin, Addr#sip_hdr_address.params).


%% Parse accept-range or media-type grammar
parse_media_range(Bin, ParamFun) ->
    {Type2, SubType2, ParamsBin2} =
        case sip_binary:trim_leading(Bin) of
            <<"*/*", ParamsBin/binary>> ->
                {'*', '*', ParamsBin};
        _ ->
            {TypeBin, <<?SLASH, Bin2/binary>>} = sip_syntax:parse_token(Bin),
            Type = sip_syntax:parse_name(TypeBin),
            case sip_binary:trim_leading(Bin2) of
                <<"*", ParamsBin/binary>> -> {Type, '*', ParamsBin};
                Bin3 ->
                    {SubTypeBin, ParamsBin} = sip_syntax:parse_token(Bin3),
                    SubType = sip_syntax:parse_name(SubTypeBin),
                    {Type, SubType, ParamsBin}
            end
    end,
    {Params, Rest} = parse_params(ParamsBin2, ParamFun),
    {media(Type2, SubType2, Params), Rest}.


%% Parse `Alert-Info' or `Call-Info' like headers
parse_info(Name, Bin, ParamFun) ->
    <<?LAQUOT, Bin2/binary>> = sip_binary:trim_leading(Bin),
    {URI, <<?RAQUOT, Bin3/binary>>} = sip_binary:parse_until(Bin2, ?RAQUOT),
    {Params, Rest} = parse_params(Bin3, ParamFun),
    Info = info(URI, Params),
    parse_list(Name, Info, Rest).

%% Parse Accept-Encoding grammar
parse_accept(Bin) ->
    {TokenBin, ParamsBin} =
        case sip_binary:trim_leading(Bin) of
            <<"*", P/binary>> -> {'*', P};
            Bin2 -> sip_syntax:parse_token(Bin2)
    end,
    {Params, Rest} = parse_params(ParamsBin, fun parse_q_param/2),
    {TokenBin, Params, Rest}.

%% @doc Append parameters to the binary
%% @end
append_params(Bin, Params) ->
    lists:foldl(fun format_param/2, Bin, Params).

%% @doc
%% Format semi-colon separated list of parameters
%% Each parameter is either binary (parameter name) or
%% tuple of two binaries (parameter name and value).
%% @end
format_param({Name, Value}, Bin) ->
    Name2 = sip_syntax:format_name(Name),

    % If contains non-token characters, write as quoted string
    Value2 =
        case need_quoting(Value) of
            true -> sip_syntax:quote_string(Value);
            false -> format_value(Value)
        end,
    <<Bin/binary, ?SEMI, Name2/binary, ?EQUAL, Value2/binary>>;
format_param(Name, Bin) ->
    Name2 = sip_syntax:format_name(Name),
    <<Bin/binary, ?SEMI, Name2/binary>>.

need_quoting(Value) when not is_binary(Value) -> false; % integer, atom, etc, no quoting
need_quoting(<<>>) -> true;
need_quoting(<<C, Rest/binary>>)  ->
    (not sip_syntax:is_token_char(C)) orelse (Rest =/= <<>> andalso need_quoting(Rest)).

%% format unknown parameter value
format_value(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
format_value(Bin) when is_binary(Bin) -> Bin;
format_value(Int) when is_integer(Int) -> sip_binary:integer_to_binary(Int);
format_value(Float) when is_float(Float) -> sip_binary:float_to_binary(Float);
format_value(List) when is_list(List) -> list_to_binary(List);
format_value({_A, _B, _C, _D} = Addr) -> sip_syntax:format_addr(Addr);
format_value({_A, _B, _C, _D, _E, _F, _G, _H} = Addr) -> sip_syntax:format_addr(Addr).

%%-----------------------------------------------------------------
%% Header-specific helpers
%%-----------------------------------------------------------------

%% @doc
%% Construct Via header value.
%% @end
-spec via(atom(), {string() | inet:ip_address(), integer() | 'undefined'} | string(), [any()]) -> #sip_hdr_via{}.
via(Transport, {Host, Port}, Params) when
  is_atom(Transport), is_list(Params), (is_list(Host) orelse is_tuple(Host)) ->
    #sip_hdr_via{transport = Transport, host = Host, port = Port, params = Params};
via(Transport, Host, Params) when is_list(Host); is_tuple(Host) ->
    via(Transport, {Host, 5060}, Params).

%% @doc Construct media type value.
%% @end
-spec media(sip_name(), sip_name(), [any()]) -> #sip_hdr_mediatype{}.
media(Type, SubType, Params) when is_list(Params) ->
    #sip_hdr_mediatype{type = Type, subtype = SubType, params = Params}.

%% @doc Construct encoding type value.
%% @end
-spec encoding(sip_name(), [any()]) -> #sip_hdr_encoding{}.
encoding(Encoding, Params) when is_list(Params) ->
    #sip_hdr_encoding{encoding = Encoding, params = Params}.

%% @doc Construct language type value.
%% @end
-spec language(sip_name(), [any()]) -> #sip_hdr_language{}.
language(Language, Params) when is_list(Params) ->
    #sip_hdr_language{language = Language, params = Params}.

%% @doc Construct `Alert-Info', `Call-Info' headers value
%% @end
-spec info(#sip_uri{} | binary(), [any()]) -> #sip_hdr_info{}.
info(URI, Params) ->
    #sip_hdr_info{uri = URI, params = Params}.

%% @doc Construct `Authorization:' header value
%% @end
-spec auth(binary() | atom(), [any()]) -> #sip_hdr_auth{}.
auth(Scheme, Params) ->
    #sip_hdr_auth{scheme = Scheme, params = Params}.

%% @doc Construct `CSeq:' header value.
%% @end
-spec cseq(integer(), sip_name()) -> #sip_hdr_cseq{}.
cseq(Sequence, Method) when
  is_integer(Sequence),
  (is_atom(Method) orelse is_binary(Method)) ->
    #sip_hdr_cseq{method = Method, sequence = Sequence}.

%% @doc Construct `Retry-After:' header value.
%% @end
-spec retry(integer(), binary(), [any()]) -> #sip_hdr_retry{}.
retry(Delay, Comment, Params) ->
    #sip_hdr_retry{delay = Delay, comment = Comment, params = Params}.


%% @doc Construct address (value of From/To headers).
%%
%% <em>Note: parses URI if it is given in binary form</em>
%% @end
-spec address(binary(), #sip_uri{} | #tel_uri{} | binary(), list()) -> #sip_hdr_address{}.
address(DisplayName, URI, Params) when is_binary(DisplayName), is_list(Params), is_binary(URI) ->
    #sip_hdr_address{display_name = DisplayName, uri = sip_uri:parse(URI), params = Params};
address(DisplayName, URI, Params) when is_binary(DisplayName), is_list(Params) ->
    #sip_hdr_address{display_name = DisplayName, uri = URI, params = Params}.

%% @doc Construct `Timestamp:' header value.
%% @end
-spec timestamp(float(), float()) -> #sip_hdr_timestamp{}.
timestamp(Timestamp, Delay) when is_float(Timestamp), is_float(Delay) ->
    #sip_hdr_timestamp{timestamp = Timestamp, delay = Delay}.

%% @doc Construct `Warning:' header value.
%% @end
-spec warning(integer(), binary() | binary(), binary()) -> #sip_hdr_warning{}.
warning(Code, Agent, Text) when is_integer(Code), is_binary(Agent), is_binary(Text) ->
    #sip_hdr_warning{code = Code, agent = Agent, text = Text}.

%% @doc Add tag to the `From:' or `To:' header.
%% @end
-spec add_tag(atom(), #sip_hdr_address{}, binary()) -> #sip_hdr_address{}.
add_tag(Name, Value, Tag) when Name =:= 'to'; Name =:= 'from' ->
    Value2 = parse(Name, Value),
    Params = lists:keystore('tag', 1, Value2#sip_hdr_address.params, {'tag', Tag}),
    Value2#sip_hdr_address{params = Params}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%% Parsing/formatting authentication/authorization parameters
%% XXX: Probably, should be separated for Authentication-Info, Authorization, Proxy-Authenticate, etc.
parse_auths(Bin) ->
    {NameBin, <<?EQUAL, ValueBin/binary>>} = sip_syntax:parse_token(Bin),
    Name = sip_syntax:parse_name(NameBin),
    {Value, Rest} =
        case Name of
            _ when Name =:= nextnonce; Name =:= nonce; Name =:= cnonce;
                   Name =:= username; Name =:= realm; Name =:= uri;
                   Name =:= opaque; Name =:= domain ->
                sip_syntax:parse_quoted_string(ValueBin);
            _ when Name =:= qop, binary_part(ValueBin, {0, 1}) =:= <<?DQUOTE>> ->
                % special case for Proxy-Authenticate, qop parameter is quoted string
                % which can contain several qop-value's
                {QOPsBin, R} = sip_syntax:parse_quoted_string(ValueBin),
                List = binary:split(QOPsBin, [<<?COMMA>>], [global]),
                QOPs = [sip_syntax:parse_name(sip_binary:trim(QOP)) || QOP <- List],
                {QOPs, R};
            _ when Name =:= qop; Name =:= algorithm ->
                {Val, R} = sip_syntax:parse_token(ValueBin),
                {sip_syntax:parse_name(Val), R};
            _ when Name =:= rspauth; Name =:= response ->
                {Digest, R} = sip_syntax:parse_quoted_string(ValueBin),
                {sip_binary:hexstr_to_binary(Digest), R};
            _ when Name =:= stale ->
                {Stale, R} = sip_syntax:parse_token(ValueBin),
                case sip_binary:to_lower(Stale) of
                    <<"false">> -> {false, R};
                    <<"true">> -> {true, R}
                end;
            nc ->
                {NC, R} = sip_binary:parse_while(ValueBin, fun sip_syntax:is_alphanum_char/1),
                {list_to_integer(binary_to_list(NC), 16), sip_binary:trim_leading(R)};
            % arbitrary auth-param
            _Other ->
                parse_token_or_quoted(ValueBin)
        end,
    Info = {Name, Value},
    case Rest of
        <<>> -> [Info];
        <<?COMMA, Rest2/binary>> ->
            [Info | parse_auths(Rest2)]
    end.

format_auth({Name, Value}) ->
    NameBin = sip_syntax:format_name(Name),
    ValBin = format_auth(Name, Value),
    <<NameBin/binary, ?EQUAL, ValBin/binary>>.

format_auth(Name, Value) when
  Name =:= nextnonce; Name =:= nonce; Name =:= cnonce;
  Name =:= username; Name =:= realm; Name =:= uri;
  Name =:= opaque; Name =:= domain ->
    sip_syntax:quote_string(Value);
format_auth(qop, [First|Rest]) ->
    % special case for Proxy-Authenticate, qop is a list
    Acc0 = <<(sip_syntax:format_name(First))/binary>>,
    Bin = lists:foldl(fun(QOP, Acc) -> <<Acc/binary, ?COMMA, ?SP, (sip_syntax:format_name(QOP))/binary>> end, Acc0, Rest),
    sip_syntax:quote_string(Bin);
format_auth(Name, Qop)
  when Name =:= qop; Name =:= algorithm ->
    sip_syntax:format_name(Qop);
format_auth(Name, Bin) when Name =:= rspauth; Name =:= response ->
    HexStr = sip_binary:binary_to_hexstr(Bin),
    sip_syntax:quote_string(HexStr);
format_auth(nc, NonceCount) ->
    [NCBin] = io_lib:format("~8.16.0b", [NonceCount]),
    list_to_binary(NCBin);
format_auth(stale, false) -> <<"false">>;
format_auth(stale, true) -> <<"true">>;
format_auth(_Name, Value) when is_binary(Value) ->
    % arbitrary auth-param
    case need_quoting(Value) of
        true -> sip_syntax:quote_string(Value);
        false -> Value
    end.

%% @doc Multi-headers parse helper
%% @end
parse_list(_Name, Top, <<>>) -> [Top];
parse_list(Name, Top, <<?COMMA, Rest/binary>>) -> [Top | parse(Name, Rest)].

%% @doc Parse language range
%% language-range   =  ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) / "*" )
%% @end
parse_language(<<$*, Rest/binary>>) -> {'*', Rest};
parse_language(Bin) ->
    IsLangChar = fun(C) -> sip_syntax:is_alpha_char(C) orelse C =:= $- end,
    {LangBin, Rest} = sip_binary:parse_while(Bin, IsLangChar),
    {sip_syntax:parse_name(LangBin), Rest}.


% RFC 3261, 7.3.1
% The line break and the whitespace at the beginning of the next
% line are treated as a single SP character. This function appends
% such lines to the last header.
fold_header(<<C/utf8, _/binary>> = Line, [{Name, Value} | Tail]) when
  C =:= ?SP; C =:= ?HTAB ->
    Line2 = sip_binary:trim_leading(Line),
    Value2 = sip_binary:trim_trailing(Value),
    [{Name, <<Value2/binary, ?SP, Line2/binary>>} | Tail];
fold_header(HeaderLine, List) ->
    [Name, Value] = binary:split(HeaderLine, <<?HCOLON>>),
    Name2 = sip_binary:to_lower(sip_binary:trim_trailing(Name)),
    Name3 = process(pn, Name2, ignored),
    [{Name3, sip_binary:trim_leading(Value)} | List].

%% Parse standard Via: parameters
parse_via_param('ttl', TTL) -> sip_binary:binary_to_integer(TTL);
parse_via_param('maddr', MAddr) ->
    case sip_syntax:parse_ip_address(MAddr) of
        {ok, Addr} -> Addr;
        {error, einval} -> binary_to_list(MAddr)
    end;
parse_via_param('received', Received) ->
    {ok, Addr} = sip_syntax:parse_ip_address(Received),
    Addr;
parse_via_param(_Name, Value) -> Value.

%% Parse q parameter (used mostly in accept headers)
parse_q_param(q, Value) -> sip_binary:binary_to_float(Value);
parse_q_param(_Name, Value) -> Value.

%% Parse contact header parameters
parse_contact_param(q, Value) -> sip_binary:binary_to_float(Value);
parse_contact_param(expires, Value) -> sip_binary:binary_to_integer(Value);
parse_contact_param(_Name, Value) -> Value.

%% Parse generic parameters
parse_generic_param(_Name, Value) -> Value.


%%-----------------------------------------------------------------
%% Tests
%%-----------------------------------------------------------------
-ifdef(TEST).

-spec header_names_test_() -> list().
header_names_test_() ->
    [% compact names support
     ?_assertEqual([{'call-id', <<"callid">>}, {'contact', <<"Alice <sip:example.com>">>},
                    {'content-encoding', <<"gzip">>}, {'content-length', <<"5">>},
                    {'content-type', <<"application/sdp">>}, {'from', <<"sip:alice@localhost">>},
                    {'subject', <<"Need more boxes">>}, {'supported', <<"100rel">>},
                    {'to', <<"sip:bob@localhost">>}, {'via', <<"SIP/2.0/UDP localhost">>}],
                   parse_headers(
                     <<"i: callid\r\nm: Alice <sip:example.com>\r\n",
                       "e: gzip\r\nl: 5\r\n",
                       "c: application/sdp\r\nf: sip:alice@localhost\r\n",
                       "s: Need more boxes\r\nk: 100rel\r\n",
                       "t: sip:bob@localhost\r\nv: SIP/2.0/UDP localhost\r\n">>)),

     % formatting, check that header names have proper case
     ?_assertEqual(<<"Accept: */*\r\nAccept-Encoding: identity\r\n",
                     "Accept-Language: en\r\nAlert-Info: <http://www.example.com/sounds/moo.wav>\r\n",
                     "Allow: INVITE\r\nAuthentication-Info: nextnonce=\"47364c23432d2e131a5fb210812c\"\r\n",
                     "Authorization: Digest username=\"Alice\"\r\nCall-ID: callid\r\n",
                     "Call-Info: <http://www.example.com/alice/photo.jpg>\r\nContact: *\r\n",
                     "Content-Disposition: session\r\nContent-Encoding: gzip\r\n",
                     "Content-Language: en\r\nContent-Length: 5\r\n",
                     "Content-Type: application/sdp\r\nCSeq: 123 INVITE\r\n",
                     "Date: Sat, 13 Nov 2010 23:29:00 GMT\r\nError-Info: <sip:not-in-service-recording@atlanta.com>\r\n",
                     "Expires: 213\r\nFrom: sip:alice@localhost\r\n",
                     "In-Reply-To: 70710@saturn.bell-tel.com, 17320@saturn.bell-tel.com\r\nMax-Forwards: 70\r\n",
                     "Min-Expires: 213\r\nMIME-Version: 1.0\r\n",
                     "Organization: Boxes by Bob\r\nPriority: non-urgent\r\n",
                     "Proxy-Authenticate: Digest realm=\"atlanta.com\"\r\nProxy-Authorization: Digest username=\"Alice\"\r\n",
                     "Proxy-Require: foo\r\nRecord-Route: <sip:server10.biloxi.com;lr>\r\n",
                     "Reply-To: Bob <sip:bob@biloxi.com>\r\nRequire: foo\r\n",
                     "Retry-After: 120\r\nRoute: <sip:server10.biloxi.com;lr>\r\n",
                     "Server: HomeServer v2\r\nSubject: Need more boxes\r\n",
                     "Supported: 100rel\r\nTimestamp: 54\r\n",
                     "To: sip:bob@localhost\r\nUnsupported: bar, baz\r\n",
                     "User-Agent: Softphone Beta1.5\r\nVia: SIP/2.0/UDP localhost\r\n",
                     "Warning: 307 isi.edu \"Session parameter 'foo' not understood\"\r\n",
                     "WWW-Authenticate: Digest realm=\"atlanta.com\"\r\n",
                     "x-custom-atom: 25\r\nX-Custom: value\r\n">>,
                   format_headers(
                     [{'accept', <<"*/*">>}, {'accept-encoding', <<"identity">>},
                      {'accept-language', <<"en">>}, {'alert-info', <<"<http://www.example.com/sounds/moo.wav>">>},
                      {'allow', <<"INVITE">>}, {'authentication-info', <<"nextnonce=\"47364c23432d2e131a5fb210812c\"">>},
                      {'authorization', <<"Digest username=\"Alice\"">>}, {'call-id', <<"callid">>},
                      {'call-info', <<"<http://www.example.com/alice/photo.jpg>">>}, {'contact', <<"*">>},
                      {'content-disposition', <<"session">>}, {'content-encoding', gzip},
                      {'content-language', <<"en">>}, {'content-length', <<"5">>},
                      {'content-type', <<"application/sdp">>}, {'cseq', <<"123 INVITE">>},
                      {'date', <<"Sat, 13 Nov 2010 23:29:00 GMT">>}, {'error-info', <<"<sip:not-in-service-recording@atlanta.com>">>},
                      {'expires', <<"213">>}, {'from', <<"sip:alice@localhost">>},
                      {'in-reply-to', <<"70710@saturn.bell-tel.com, 17320@saturn.bell-tel.com">>}, {'max-forwards', 70},
                      {'min-expires', <<"213">>}, {'mime-version', <<"1.0">>},
                      {'organization', <<"Boxes by Bob">>}, {'priority', <<"non-urgent">>},
                      {'proxy-authenticate', <<"Digest realm=\"atlanta.com\"">>}, {'proxy-authorization', <<"Digest username=\"Alice\"">>},
                      {'proxy-require', <<"foo">>}, {'record-route', <<"<sip:server10.biloxi.com;lr>">>},
                      {'reply-to', <<"Bob <sip:bob@biloxi.com>">>}, {'require', <<"foo">>},
                      {'retry-after', <<"120">>}, {'route', <<"<sip:server10.biloxi.com;lr>">>},
                      {'server', <<"HomeServer v2">>}, {'subject', <<"Need more boxes">>},
                      {'supported', <<"100rel">>}, {'timestamp', <<"54">>},
                      {'to', <<"sip:bob@localhost">>}, {'unsupported', <<"bar, baz">>},
                      {'user-agent', <<"Softphone Beta1.5">>}, {'via', <<"SIP/2.0/UDP localhost">>},
                      {'warning', <<"307 isi.edu \"Session parameter 'foo' not understood\"">>},
                      {'www-authenticate', <<"Digest realm=\"atlanta.com\"">>},
                      {'x-custom-atom', 25}, {<<"X-Custom">>, <<"value">>}]))
     ].

-spec parse_headers_test_() -> list().
parse_headers_test_() ->
    [% verify parsing/formatting of all supported headers

     % parsing
     ?_assertEqual([], parse_headers(<<>>)),

     % multi-line headers
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   parse_headers(<<"Subject: I know you're there,\r\n               pick up the phone   \r\n               and talk to me!\r\n">>)),
     ?_assertEqual([{'subject', <<"I know you're there, pick up the phone and talk to me!">>}],
                   parse_headers(<<"Subject: I know you're there,\r\n\tpick up the phone    \r\n               and talk to me!\r\n">>)),

     % Already parsed
     ?_assertEqual({parsed, value}, parse('x-custom2', {parsed, value})),

     % Custom header
     ?_assertEqual(<<"custom">>, parse('x-custom2', <<"custom">>)),
     ?_assertEqual(<<"25">>, format('x-custom2', 25)),

     % Accept
     ?_assertEqual([media('application', 'sdp', [{level, <<"1">>}]),
                    media('application', '*', [{q, 0.5}]),
                    media('*', '*', [{q, 0.3}])],
                   parse('accept', <<"application/sdp;level=1, application/*;q=0.5, */*;q=0.3">>)),
     ?_assertEqual(<<"application/sdp;level=1, application/*;q=0.5, */*;q=0.3">>,
                   format('accept', [media('application', 'sdp', [{level, <<"1">>}]),
                                     media('application', '*', [{q, 0.5}]),
                                     media('*', '*', [{q, 0.3}])])),

     % Accept-Encoding
     ?_assertEqual([encoding('gzip', []),
                    encoding('identity', [{q, 0.3}]),
                    encoding('*', [{q, 0.2}])],
                   parse('accept-encoding', <<"gzip, identity;q=0.3, *;q=0.2">>)),
     ?_assertEqual(<<"gzip, identity;q=0.3, *;q=0.2">>,
                   format('accept-encoding',
                          [encoding('gzip', []),
                           encoding('identity', [{q, 0.3}]),
                           encoding('*', [{q, 0.2}])])),

     % Accept-Language
     ?_assertEqual([language('da', []),
                    language('en-gb', [{q, 0.8}]),
                    language('en', [{q, 0.7}]),
                    language('*', [{q, 0.6}])],
                   parse('accept-language', <<"da, en-gb;q=0.8, en;q=0.7, *;q=0.6">>)),
     ?_assertEqual(<<"da, en-gb;q=0.8, en;q=0.7, *;q=0.6">>,
                   format('accept-language',
                          [language('da', []),
                           language('en-gb', [{q, 0.8}]),
                           language('en', [{q, 0.7}]),
                           language('*', [{q, 0.6}])])),

     % Alert-Info
     ?_assertEqual([info(<<"http://www.example.com/sounds/moo.wav">>, []),
                    info(<<"http://www.example.com/sounds/boo.wav">>, [{foo, <<"value">>}])],
                   parse('alert-info', <<"<http://www.example.com/sounds/moo.wav>, <http://www.example.com/sounds/boo.wav>;foo=value">>)),
     ?_assertEqual(<<"<http://www.example.com/sounds/moo.wav>, <http://www.example.com/sounds/boo.wav>;foo=value">>,
                   format('alert-info',
                          [info(<<"http://www.example.com/sounds/moo.wav">>, []),
                           info(<<"http://www.example.com/sounds/boo.wav">>, [{foo, <<"value">>}])])),

     % Allow
     ?_assertEqual(['INVITE', 'ACK', 'CANCEL', 'OPTIONS', 'BYE'],
                   parse('allow', <<"INVITE, ACK, CANCEL, OPTIONS, BYE">>)),
     ?_assertEqual(<<"INVITE, ACK, CANCEL, OPTIONS, BYE">>,
                   format('allow', ['INVITE', 'ACK', 'CANCEL', 'OPTIONS', 'BYE'])),

     % Authentication-Info
     ?_assertEqual([{nextnonce, <<"47364c23432">>},
                    {qop, auth},
                    {rspauth, <<95, 17, 58, 84, 50>>},
                    {cnonce, <<"42a2187831a9e">>},
                    {nc, 25}],
                   parse('authentication-info', <<"nextnonce=\"47364c23432\", qop=auth, rspauth=\"5f113a5432\", cnonce=\"42a2187831a9e\", nc=00000019">>)),
     ?_assertEqual(<<"nextnonce=\"47364c23432\", qop=auth, rspauth=\"5f113a5432\", cnonce=\"42a2187831a9e\", nc=00000019">>,
                   format('authentication-info',
                          [{nextnonce, <<"47364c23432">>},
                           {qop, auth},
                           {rspauth, <<95, 17, 58, 84, 50>>},
                           {cnonce, <<"42a2187831a9e">>},
                           {nc, 25}])),

     % Authorization
     ?_assertEqual(auth('Digest',
                        [{username, <<"Alice">>}, {realm, <<"atlanta.com">>},
                         {nonce, <<"84a4cc6f3082121f32b42a2187831a9e">>}, {uri, <<"sip:alice@atlanta.com">>},
                         {response, <<117,135,36,82,52,179,67,76,195,65,34,19,229,241,19,165>>}, {algorithm, 'MD5'},
                         {cnonce, <<"0a4f113b">>}, {opaque, <<"5ccc069c403ebaf9f0171e9517f40e41">>},
                         {qop, auth}, {nc, 1}, {param, <<"value">>}, {param2, <<"va lue">>}]),
                   parse('authorization',
                         <<"Digest username=\"Alice\", realm=\"atlanta.com\", ",
                           "nonce=\"84a4cc6f3082121f32b42a2187831a9e\", uri=\"sip:alice@atlanta.com\", ",
                           "response=\"7587245234b3434cc3412213e5f113a5\", algorithm=MD5, ",
                           "cnonce=\"0a4f113b\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\", ",
                           "qop=auth, nc=00000001, param=\"value\", param2=\"va lue\"">>)),
     ?_assertEqual(<<"Digest username=\"Alice\", realm=\"atlanta.com\", ",
                     "nonce=\"84a4cc6f3082121f32b42a2187831a9e\", uri=\"sip:alice@atlanta.com\", ",
                     "response=\"7587245234b3434cc3412213e5f113a5\", algorithm=MD5, ",
                     "cnonce=\"0a4f113b\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\", ",
                     "qop=auth, nc=00000001, param=value, param2=\"va lue\"">>,
                   format('authorization',
                          auth('Digest',
                               [{username, <<"Alice">>}, {realm, <<"atlanta.com">>},
                                {nonce, <<"84a4cc6f3082121f32b42a2187831a9e">>}, {uri, <<"sip:alice@atlanta.com">>},
                                {response, <<117,135,36,82,52,179,67,76,195,65,34,19,229,241,19,165>>}, {algorithm, 'MD5'},
                                {cnonce, <<"0a4f113b">>}, {opaque, <<"5ccc069c403ebaf9f0171e9517f40e41">>},
                                {qop, auth}, {nc, 1}, {param, <<"value">>}, {param2, <<"va lue">>}]))),


     % Call-Id
     ?_assertEqual(<<"somecallid">>, parse('call-id', <<"somecallid">>)),
     ?_assertEqual(<<"somecallid">>, format('call-id', <<"somecallid">>)),

     % Call-Info
     ?_assertEqual([info(<<"http://wwww.example.com/alice/photo.jpg">>, [{purpose, icon}]),
                    info(<<"http://www.example.com/alice/">>, [{purpose, info}, {param, <<"value">>}])],
                   parse('call-info', <<"<http://wwww.example.com/alice/photo.jpg> ;purpose=icon, <http://www.example.com/alice/> ;purpose=info;param=\"value\"">>)),
     ?_assertEqual(<<"<http://wwww.example.com/alice/photo.jpg>;purpose=icon, <http://www.example.com/alice/>;purpose=info;param=value">>,
                   format('call-info',
                          [info(<<"http://wwww.example.com/alice/photo.jpg">>, [{purpose, icon}]),
                           info(<<"http://www.example.com/alice/">>, [{purpose, info}, {param, <<"value">>}])])),

     % Contact
     ?_assertEqual([address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, 0.1}])],
                   parse('contact', <<"Bob <sip:bob@biloxi.com>;q=0.1">>)),
     ?_assertEqual([address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, 0.1}]),
                    address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, 0.2}])],
                   parse('contact', <<"Bob <sip:bob@biloxi.com>;q=0.1,\"Alice\" <sip:alice@atlanta.com>;q=0.2">>)),
     ?_assertEqual([address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, 0.1}]),
                    address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, 0.2}]),
                    address(<<>>, <<"sip:anonymous@example.com">>, [{param, <<"va lue">>}])],
                   parse('contact', <<"Bob <sip:bob@biloxi.com>;q=0.1, \"Alice\" <sip:alice@atlanta.com>;q=0.2, <sip:anonymous@example.com>;param=\"va lue\"">>)),
     ?_assertEqual('*', parse('contact', <<"*">>)),
     ?_assertEqual([address(<<>>, <<"sip:bob@biloxi.com">>, []),
                    address(<<>>, <<"sip:alice@atlanta.com">>, [])],
                   parse('contact', <<"sip:bob@biloxi.com, sip:alice@atlanta.com">>)),
     ?_assertEqual([address(<<"Mr. Watson">>, <<"sip:watson@worcester.bell-telephone.com">>, [{q, 0.7}, {expires, 3600}]),
                    address(<<"Mr. Watson">>, <<"mailto:watson@bell-telephone.com">>, [{q, 0.1}])],
                   parse('contact', <<"\"Mr. Watson\" <sip:watson@worcester.bell-telephone.com>;q=0.7; expires=3600, ",
                                      "\"Mr. Watson\" <mailto:watson@bell-telephone.com> ;q=0.1">>)),

     ?_assertEqual(<<"\"Bob\" <sip:bob@biloxi.com>;q=0.1">>,
                   format('contact', address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, 0.1}]))),
     ?_assertEqual(<<"\"Bob\" <sip:bob@biloxi.com>;q=0.1, \"Alice\" <sip:alice@atlanta.com>;q=0.2">>,
                   format('contact', [address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, 0.1}]),
                                      address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, 0.2}])])),
     ?_assertEqual(<<"\"Bob\" <sip:bob@biloxi.com>;q=0.1, \"Alice\" <sip:alice@atlanta.com>;q=0.2, <sip:anonymous@example.com>;param=\"va lue\"">>,
                   format('contact', [address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{q, 0.1}]),
                                      address(<<"Alice">>, <<"sip:alice@atlanta.com">>, [{q, 0.2}]),
                                      address(<<>>, <<"sip:anonymous@example.com">>, [{param, <<"va lue">>}])])),
     ?_assertEqual(<<"*">>, format('contact', '*')),
     ?_assertEqual(<<"<sip:bob@biloxi.com>, <sip:alice@atlanta.com>">>,
                   format('contact', [address(<<>>, <<"sip:bob@biloxi.com">>, []),
                                      address(<<>>, <<"sip:alice@atlanta.com">>, [])])),
     ?_assertEqual(<<"\"Mr. Watson\" <sip:watson@worcester.bell-telephone.com>;q=0.7;expires=3600, ",
                     "\"Mr. Watson\" <mailto:watson@bell-telephone.com>;q=0.1">>,
                   format('contact', [address(<<"Mr. Watson">>, <<"sip:watson@worcester.bell-telephone.com">>, [{q, 0.7}, {expires, 3600}]),
                                      address(<<"Mr. Watson">>, <<"mailto:watson@bell-telephone.com">>, [{q, 0.1}])])),

     % Content-Disposition
     ?_assertEqual(#sip_hdr_disposition{type = 'icon', params = [{handling, optional}, {param, <<"value">>}]},
                   parse('content-disposition', <<"icon;handling=optional;param=value">>)),
     ?_assertEqual(<<"icon;handling=optional;param=value">>,
                   format('content-disposition', #sip_hdr_disposition{type = 'icon', params = [{handling, optional}, {param, value}]})),


     % Content-Encoding
     ?_assertEqual([gzip, tar], parse('content-encoding', <<"gzip, tar">>)),
     ?_assertEqual(<<"gzip, tar">>, format('content-encoding', [gzip, tar])),

     % Content-Language
     ?_assertEqual([en, 'en-us-some'],
                   parse('content-language', <<"en, en-us-some">>)),
     ?_assertEqual(<<"en, en-us-some">>,
                   format('content-language', [en, 'en-us-some'])),

     % Content-Length
     ?_assertEqual(32543523, parse('content-length', <<"32543523">>)),
     ?_assertEqual(<<"98083">>, format('content-length', 98083)),

     % Content-Type
     ?_assertEqual(media('application', 'sdp', [{param, <<"value">>}]),
                   parse('content-type', <<"application/sdp;param=value">>)),
     ?_assertEqual(<<"application/sdp;param=value">>,
                   format('content-type', media('application', 'sdp', [{param, <<"value">>}]))),

     % CSeq
     ?_assertEqual(cseq(1231, 'ACK'), parse('cseq', <<"1231 ACK">>)),
     ?_assertEqual(<<"123453 INVITE">>, format('cseq', cseq(123453, 'INVITE'))),

     % Date
     ?_assertEqual({{2010, 11, 13}, {23, 29, 00}},
                    parse('date', <<"Sat, 13 Nov 2010 23:29:00 GMT">>)),
     ?_assertEqual(<<"Sat, 13 Nov 2010 23:29:00 GMT">>,
                    format('date', {{2010, 11, 13}, {23, 29, 00}})),

     % Error-Info
     ?_assertEqual([info(<<"sip:not-in-service-recording@atlanta.com">>, [{param, <<"value">>}])],
                   parse('error-info', <<"<sip:not-in-service-recording@atlanta.com>;param=\"value\"">>)),
     ?_assertEqual(<<"<sip:not-in-service-recording@atlanta.com>;param=value">>,
                   format('error-info', [info(<<"sip:not-in-service-recording@atlanta.com">>, [{param, <<"value">>}])])),

     % Expires
     ?_assertEqual(213, parse('expires', <<"213">>)),
     ?_assertEqual(<<"213">>, format('expires', 213)),

     % From
     ?_assertEqual(address(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"Bob  Zert <sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"sip:bob@biloxi.com ;tag=1928301774">>)),
     ?_assertEqual(address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"<sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(address(<<"Bob Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('from', <<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>)),

     ?_assertEqual(<<"\"Bob  Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<"Bob  Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"<sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"<sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<>>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('from', address(<<"Bob Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),

     % In-Reply-To
     ?_assertEqual([<<"70710@saturn.bell-tel.com">>, <<"17320@saturn.bell-tel.com">>],
                   parse('in-reply-to', <<"70710@saturn.bell-tel.com, 17320@saturn.bell-tel.com">>)),
     ?_assertEqual(<<"70710@saturn.bell-tel.com, 17320@saturn.bell-tel.com">>,
                   format('in-reply-to', [<<"70710@saturn.bell-tel.com">>, <<"17320@saturn.bell-tel.com">>])),

     % Max-Forwards
     ?_assertEqual(70, parse('max-forwards', <<"70">>)),
     ?_assertEqual(<<"70">>, format('max-forwards', 70)),

     % Min-Expires
     ?_assertEqual(213, parse('min-expires', <<"213">>)),
     ?_assertEqual(<<"213">>, format('min-expires', 213)),

     % MIME-Version
     ?_assertEqual({1, 0}, parse('mime-version', <<"1.0">>)),
     ?_assertEqual(<<"1.0">>, format('mime-version', {1, 0})),

     % Organization
     ?_assertEqual(<<"Boxes by Bob">>, parse('organization', <<"  Boxes by Bob  ">>)),
     ?_assertEqual(<<"Boxes by Bob">>, format('organization', <<"Boxes by Bob">>)),

     % Priority
     ?_assertEqual('non-urgent', parse('priority', <<"non-urgent">>)),
     ?_assertEqual(<<"non-urgent">>, format('priority', 'non-urgent')),

     % Proxy-Authenticate
     ?_assertEqual(auth('Digest',
                        [{realm, <<"atlanta.com">>}, {domain, <<"sip:ss1.carrier.com">>},
                         {nonce, <<"f84f1cec41e6cbe5aea9c8e88d359">>}, {opaque, <<>>},
                         {stale, false}, {algorithm, 'MD5'}, {qop, [auth, 'auth-int']},
                         {param, <<>>}]),
                   parse('proxy-authenticate',
                         <<"Digest realm=\"atlanta.com\", domain=\"sip:ss1.carrier.com\", ",
                           "nonce=\"f84f1cec41e6cbe5aea9c8e88d359\", opaque=\"\", ",
                           "stale=FALSE, algorithm=MD5, qop=\"auth, auth-int\", ",
                           "param=\"\"">>)),
     ?_assertEqual(<<"Digest realm=\"atlanta.com\", domain=\"sip:ss1.carrier.com\", ",
                     "nonce=\"f84f1cec41e6cbe5aea9c8e88d359\", opaque=\"\", ",
                     "stale=false, algorithm=MD5, qop=\"auth, auth-int\", ",
                     "param=\"\"">>,
                   format('proxy-authenticate',
                          auth('Digest',
                               [{realm, <<"atlanta.com">>}, {domain, <<"sip:ss1.carrier.com">>},
                                {nonce, <<"f84f1cec41e6cbe5aea9c8e88d359">>}, {opaque, <<>>},
                                {stale, false}, {algorithm, 'MD5'}, {qop, [auth, 'auth-int']},
                                {param, <<>>}]))),

     ?_assertEqual(auth('Digest', [{realm, <<"atlanta.com">>}, {stale, true}]),
                   parse('proxy-authenticate', <<"Digest realm=\"atlanta.com\", stale=true">>)),
     ?_assertEqual(<<"Digest realm=\"atlanta.com\", stale=true">>,
                   format('proxy-authenticate', auth('Digest', [{realm, <<"atlanta.com">>}, {stale, true}]))),

     % Proxy-Authorization
     % XXX: copy-pasted from Authorization header tests.
     ?_assertEqual(auth('Digest',
                        [{username, <<"Alice">>}, {realm, <<"atlanta.com">>},
                         {nonce, <<"84a4cc6f3082121f32b42a2187831a9e">>}, {uri, <<"sip:alice@atlanta.com">>},
                         {response, <<117,135,36,82,52,179,67,76,195,65,34,19,229,241,19,165>>}, {algorithm, 'MD5'},
                         {cnonce, <<"0a4f113b">>}, {opaque, <<"5ccc069c403ebaf9f0171e9517f40e41">>},
                         {qop, auth}, {nc, 1}, {param, <<"value">>}, {param2, <<"va lue">>}]),
                   parse('proxy-authorization',
                         <<"Digest username=\"Alice\", realm=\"atlanta.com\", ",
                           "nonce=\"84a4cc6f3082121f32b42a2187831a9e\", uri=\"sip:alice@atlanta.com\", ",
                           "response=\"7587245234b3434cc3412213e5f113a5\", algorithm=MD5, ",
                           "cnonce=\"0a4f113b\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\", ",
                           "qop=auth, nc=00000001, param=\"value\", param2=\"va lue\"">>)),
     ?_assertEqual(<<"Digest username=\"Alice\", realm=\"atlanta.com\", ",
                     "nonce=\"84a4cc6f3082121f32b42a2187831a9e\", uri=\"sip:alice@atlanta.com\", ",
                     "response=\"7587245234b3434cc3412213e5f113a5\", algorithm=MD5, ",
                     "cnonce=\"0a4f113b\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\", ",
                     "qop=auth, nc=00000001, param=value, param2=\"va lue\"">>,
                   format('proxy-authorization',
                          auth('Digest',
                               [{username, <<"Alice">>}, {realm, <<"atlanta.com">>},
                                {nonce, <<"84a4cc6f3082121f32b42a2187831a9e">>}, {uri, <<"sip:alice@atlanta.com">>},
                                {response, <<117,135,36,82,52,179,67,76,195,65,34,19,229,241,19,165>>}, {algorithm, 'MD5'},
                                {cnonce, <<"0a4f113b">>}, {opaque, <<"5ccc069c403ebaf9f0171e9517f40e41">>},
                                {qop, auth}, {nc, 1}, {param, <<"value">>}, {param2, <<"va lue">>}]))),

     % Proxy-Require
     ?_assertEqual([foo, bar], parse('proxy-require', <<"foo, bar">>)),
     ?_assertEqual(<<"foo, bar">>, format('proxy-require', [foo, bar])),

     % Record-Route
     ?_assertEqual([address(<<>>, <<"sip:p1.example.com;lr">>, [])],
                   parse('record-route', <<"<sip:p1.example.com;lr>">>)),
     ?_assertEqual(<<"<sip:p1.example.com;lr>">>,
                   format('record-route', address(<<>>, <<"sip:p1.example.com;lr">>, []))),

     % Reply-To
     ?_assertEqual(address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{param, <<"value">>}]),
                   parse('reply-to', <<"Bob <sip:bob@biloxi.com>;param=value">>)),
     ?_assertEqual(<<"\"Bob\" <sip:bob@biloxi.com>;param=value">>,
                   format('reply-to', address(<<"Bob">>, <<"sip:bob@biloxi.com">>, [{param, <<"value">>}]))),

     % Require
     ?_assertEqual([foo, bar], parse('require', <<"foo, bar">>)),
     ?_assertEqual(<<"foo, bar">>, format('require', [foo, bar])),

     % Retry-After
     ?_assertEqual(retry(18000, <<>>, [{duration, 3600}, {param, <<"value">>}]),
                   parse('retry-after', <<"18000;duration=3600;param=value">>)),
     ?_assertEqual(retry(120, <<"(I'm in a meeting)">>, []),
                   parse('retry-after', <<"120 (I'm in a meeting)">>)),
     ?_assertEqual(<<"18000;duration=3600;param=value">>,
                   format('retry-after', retry(18000, <<>>, [{duration, 3600}, {param, <<"value">>}]))),
     ?_assertEqual(<<"120 (I'm in a meeting)">>,
                   format('retry-after', retry(120, <<"(I'm in a meeting)">>, []))),

     % Route
     ?_assertEqual([address(<<>>, <<"sip:p1.example.com;lr">>, [])],
                   parse('route', <<"<sip:p1.example.com;lr>">>)),
     ?_assertEqual(<<"<sip:p1.example.com;lr>">>,
                   format('route', address(<<>>, <<"sip:p1.example.com;lr">>, []))),

     % Server
     ?_assertEqual(<<"HomeServer v2">>, parse('server', <<"HomeServer v2">>)),
     ?_assertEqual(<<"HomeServer v2">>, format('server', <<"HomeServer v2">>)),

     % Subject
     ?_assertEqual(<<"Need more boxes">>, parse('subject', <<"Need more boxes">>)),
     ?_assertEqual(<<"Need more boxes">>, format('subject', <<"Need more boxes">>)),

     % Supported
     ?_assertEqual([foo, bar], parse('supported', <<"foo, bar">>)),
     ?_assertEqual(<<"foo, bar">>, format('supported', [foo, bar])),

     % Timestamp
     ?_assertEqual(timestamp(54.3, 2.6), parse('timestamp', <<"54.3 2.6">>)),
     ?_assertEqual(timestamp(54.3, 0.0), parse('timestamp', <<"54.3">>)),
     ?_assertEqual(<<"54.3 2.6">>, format('timestamp', timestamp(54.3, 2.6))),
     ?_assertEqual(<<"54.3">>, format('timestamp', timestamp(54.3, 0.0))),

     % To
     ?_assertEqual(address(<<"Bob Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('to', <<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>)),
     ?_assertEqual(address(<<"Bob \"Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]),
                   parse('to', <<"\"Bob \\\"Zert\" <sip:bob@biloxi.com>;tag=1928301774">>)),

     ?_assertEqual(<<"\"Bob Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('to', address(<<"Bob Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),
     ?_assertEqual(<<"\"Bob \\\"Zert\" <sip:bob@biloxi.com>;tag=1928301774">>,
                   format('to', address(<<"Bob \"Zert">>, <<"sip:bob@biloxi.com">>, [{'tag', <<"1928301774">>}]))),

     % Unsupported
     ?_assertEqual([foo, bar], parse('unsupported', <<"foo, bar">>)),
     ?_assertEqual(<<"foo, bar">>, format('unsupported', [foo, bar])),

     % User-Agent
     ?_assertEqual(<<"Softphone Beta1.5">>, parse('user-agent', <<"Softphone Beta1.5">>)),
     ?_assertEqual(<<"Softphone Beta1.5">>, format('user-agent', <<"Softphone Beta1.5">>)),

     % Via
     ?_assertEqual([via(udp, {{8193,3512,0,0,0,0,44577,44306}, undefined}, [{branch, <<"z9hG4bK776asdhds">>}])],
                   parse('via', <<"SIP/2.0/UDP [2001:0db8:0000:0000:0000:0000:ae21:ad12];branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}])],
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}])],
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com ; branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {{127, 0, 0, 1}, 15060}, [{param, <<"value">>}, flag]),
                    via(tcp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}])],
                   parse('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {{127, 0, 0, 1}, 15060}, [{param, <<"value">>}, flag]),
                    via(tcp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}]),
                    via(udp, {"pc22.atlanta.com", undefined}, [{branch, <<"z9hG4bK43nthoeu3">>}])],
                   parse('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag,SIP/2.0/TCP pc33.atlanta.com;branch=z9hG4bK776asdhds,SIP/2.0/UDP pc22.atlanta.com;branch=z9hG4bK43nthoeu3">>)),
     ?_assertEqual(<<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>,
                   format('via', [via(udp, {"pc33.atlanta.com", undefined}, [{branch, <<"z9hG4bK776asdhds">>}])])),
     ?_assertEqual(<<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag, SIP/2.0/TCP pc33.atlanta.com:5060;branch=z9hG4bK776asdhds">>,
                   format('via', [via(udp, {{127, 0, 0, 1}, 15060}, [{param, <<"value">>}, flag]),
                                         via(tcp, "pc33.atlanta.com", [{branch, <<"z9hG4bK776asdhds">>}])])),
     ?_assertEqual(<<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag">>,
                   format('via', <<"SIP/2.0/UDP 127.0.0.1:15060;param=value;flag">>)),
     ?_assertEqual(<<"SIP/2.0/UDP pc33.atlanta.com;extra=\"Hello world\"">>,
                   format('via', [via(udp, {"pc33.atlanta.com", undefined}, [{extra, <<"Hello world">>}])])),
     ?_assertEqual([via(udp, {"pc33.atlanta.com", undefined}, [{ttl, 3}, {maddr, {224, 0, 0, 1}}, {received, {127, 0, 0, 1}}, {branch, <<"z9hG4bK776asdhds">>}])],
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com;ttl=3;maddr=224.0.0.1;received=127.0.0.1;branch=z9hG4bK776asdhds">>)),
     ?_assertEqual([via(udp, {"pc33.atlanta.com", undefined}, [{ttl, 3}, {maddr, "sip.mcast.net"}, {received, {127, 0, 0, 1}}, {branch, <<"z9hG4bK776asdhds">>}])],
                   parse('via', <<"SIP/2.0/UDP pc33.atlanta.com;ttl=3;maddr=sip.mcast.net;received=127.0.0.1;branch=z9hG4bK776asdhds">>)),

     % Warning
     ?_assertEqual([warning(307, <<"isi.edu">>, <<"Session parameter 'foo' not understood">>),
                    warning(301, <<"isi.edu">>, <<"Incompatible network address type 'E.164'">>)],
                   parse('warning', <<"307 isi.edu \"Session parameter 'foo' not understood\", ",
                                      "301 isi.edu \"Incompatible network address type 'E.164'\"">>)),
     ?_assertEqual(<<"307 isi.edu \"Session parameter 'foo' not understood\", ",
                     "301 isi.edu \"Incompatible network address type 'E.164'\"">>,
                   format('warning',
                          [warning(307, <<"isi.edu">>, <<"Session parameter 'foo' not understood">>),
                           warning(301, <<"isi.edu">>, <<"Incompatible network address type 'E.164'">>)])),

     % WWW-Authenticate
     ?_assertEqual(auth('Digest',
                        [{realm, <<"atlanta.com">>}, {domain, <<"sip:ss1.carrier.com">>},
                         {nonce, <<"f84f1cec41e6cbe5aea9c8e88d359">>}, {opaque, <<>>},
                         {stale, false}, {algorithm, 'MD5'}, {qop, [auth, 'auth-int']},
                         {param, <<>>}]),
                   parse('www-authenticate',
                         <<"Digest realm=\"atlanta.com\", domain=\"sip:ss1.carrier.com\", ",
                           "nonce=\"f84f1cec41e6cbe5aea9c8e88d359\", opaque=\"\", ",
                           "stale=FALSE, algorithm=MD5, qop=\"auth, auth-int\", ",
                           "param=\"\"">>)),
     ?_assertEqual(<<"Digest realm=\"atlanta.com\", domain=\"sip:ss1.carrier.com\", ",
                     "nonce=\"f84f1cec41e6cbe5aea9c8e88d359\", opaque=\"\", ",
                     "stale=false, algorithm=MD5, qop=\"auth, auth-int\", ",
                     "param=\"\"">>,
                   format('www-authenticate',
                          auth('Digest',
                               [{realm, <<"atlanta.com">>}, {domain, <<"sip:ss1.carrier.com">>},
                                {nonce, <<"f84f1cec41e6cbe5aea9c8e88d359">>}, {opaque, <<>>},
                                {stale, false}, {algorithm, 'MD5'}, {qop, [auth, 'auth-int']},
                                {param, <<>>}]))),

     ?_assertEqual(auth('Digest', [{realm, <<"atlanta.com">>}, {stale, true}]),
                   parse('www-authenticate', <<"Digest realm=\"atlanta.com\", stale=true">>)),
     ?_assertEqual(<<"Digest realm=\"atlanta.com\", stale=true">>,
                   format('www-authenticate', auth('Digest', [{realm, <<"atlanta.com">>}, {stale, true}]))),

     % If the URI is not enclosed in angle brackets, any semicolon-delimited
     % parameters are header-parameters, not URI parameters, Section 20.
     ?_assertEqual({address(<<>>, sip_uri:parse(<<"sip:alice@atlanta.com">>), [{param, <<"value">>}]), <<>>},
                   parse_address(<<"sip:alice@atlanta.com;param=value">>, fun parse_generic_param/2)),

     % Check we support certain values
     ?_assertEqual(<<"some">>, format_value(some)),
     ?_assertEqual(<<"some">>, format_value(<<"some">>)),
     ?_assertEqual(<<"123">>, format_value(123)),
     ?_assertEqual(<<"123.35">>, format_value(123.35)),
     ?_assertEqual(<<"example.com">>, format_value("example.com")),
     ?_assertEqual(<<"10.0.0.1">>, format_value({10, 0, 0, 1})),
     ?_assertEqual(<<"[2001:0db8:11a3:09d7:1f34:8a2e:07a0:765d]">>, format_value({8193,3512,4515,2519,7988,35374,1952,30301}))
    ].

-spec utility_test_() -> list().
utility_test_() ->
    URI = sip_uri:parse(<<"sip:example.com">>),
    [
     ?_assertEqual(address(<<>>, URI, [{tag, <<"123456">>}]), add_tag('to', address(<<>>, URI, []), <<"123456">>))
     ].

-endif.
