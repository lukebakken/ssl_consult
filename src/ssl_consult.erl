%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(ssl_consult).

-export([consult_file/1,
         consult_string/1]).

%%
%% API
%%

consult_file(Path) ->
    case erl_prim_loader:get_file(Path) of
        {ok, Binary, _FullName} ->
            Encoding =
                case epp:read_encoding_from_binary(Binary) of
                    none ->
                        latin1;
                    Enc ->
                        Enc
                end,
            case unicode:characters_to_list(Binary, Encoding) of
                {error, _String, Rest} ->
                    error({badfile, {encoding_error, Rest}});
                {incomplete, _String, Rest} ->
                    error({badfile, {encoding_incomplete, Rest}});
                String when is_list(String) ->
                    consult_string(String)
            end;
        error ->
            error({badfile, Path})
    end.

consult_string(String) ->
    case erl_scan:string(String) of
        {error, Info, Location} ->
            error({badfile, {scan_error, Info, Location}});
        {ok, Tokens, _EndLocation} ->
            consult_tokens(Tokens)
    end.

%%
%% Implementation
%%

consult_tokens(Tokens) ->
    case erl_parse:parse_exprs(Tokens) of
        {error, Info} ->
            error({badfile, {parse_error, Info}});
        {ok, [Expr]} ->
            consult_expression(Expr);
        {ok, Other} ->
            error({badfile, {parse_error, Other}})
    end.

consult_expression(Expr) ->
    {value, Value, Bs} =
        try
            erl_eval:expr(Expr, erl_eval:new_bindings())
        catch
            Class:Error ->
                error({badfile, {Class, Error}})
        end,
    case erl_eval:bindings(Bs) of
        [] ->
            Value;
        Other ->
            error({badfile, {bindings, Other}})
    end.
