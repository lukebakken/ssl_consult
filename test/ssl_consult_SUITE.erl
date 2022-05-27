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
    ExpectedOkConfig =
        [{replication_transport, ssl},
         {replication_server_ssl_options,
          [{cacertfile, "/etc/rabbitmq/ca_certificate.pem"},
           {certfile, "/etc/rabbitmq/server_certificate.pem"},
           {keyfile, "/etc/rabbitmq/server_key.pem"},
           {secure_renegotiate, true},
           {verify, verify_peer},
           {fail_if_no_peer_cert, true}]},
         {replication_client_ssl_options,
          [{cacertfile, "/etc/rabbitmq/ca_certificate.pem"},
           {certfile, "/etc/rabbitmq/client_certificate.pem"},
           {keyfile, "/etc/rabbitmq/client_key.pem"},
           {secure_renegotiate, true},
           {verify, verify_peer},
           {fail_if_no_peer_cert, true}]}],
    AdvancedConfigFile = ?config(data_dir, Config) ++ "advanced.config",
    ?assertEqual(ExpectedOkConfig, ssl_consult:consult(AdvancedConfigFile)).
