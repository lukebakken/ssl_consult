%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(ssl_consult_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-export([]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, tests}].

all_tests() ->
    [consult_file].

groups() ->
    [{tests, [], all_tests()}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

consult_file(Config) ->
    MatchFun = public_key:pkix_verify_hostname_match_fun(https),
    AdvancedConfigFile = ?config(data_dir, Config) ++ "advanced.config",
    AdvancedConfig = ssl_consult:consult(AdvancedConfigFile),
    ?assertMatch([{rabbit,
                   [{log, [{console, [{enabled, true}, {level, debug}]}]},
                    {loopback_users, []},
                    {ssl_listeners, [5671]},
                    {ssl_options,
                     [{cacertfile, "/path/to/certs/ca_certificate.pem"},
                      {certfile, "/path/to/certs/server_certificate.pem"},
                      {keyfile, "/path/to/certs/server_key.pem"},
                      {fail_if_no_peer_cert, true},
                      {verify, verify_peer},
                      {customize_hostname_check, [{match_fun, MatchFun}]}]},
                    {background_gc_enabled, true},
                    {background_gc_target_interval, 1000}]}],
                 AdvancedConfig).
