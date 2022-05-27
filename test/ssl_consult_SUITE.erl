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
    [inet_tls_enabled,
     replication_over_tls_configuration_with_optfile,
     replication_over_tls_configuration_with_opt].

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

replication_over_tls_configuration_with_optfile(Config) ->
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
    [begin
         InitArgs =
             [{proto_dist, ["inet_tls"]},
              {ssl_dist_optfile, [?config(data_dir, Config) ++ File]}],
         ?assertEqual(ExpectedOkConfig,
                      replication_over_tls_configuration(InitArgs))
     end
     || File
            <- ["inter_node_tls_server_client_ok.config",
                "inter_node_tls_client_server_ok.config"]],

    FileBroken =
        ?config(data_dir, Config) ++ "inter_node_tls_broken.config",
    InitArgsBroken =
        [{proto_dist, ["inet_tls"]}, {ssl_dist_optfile, [FileBroken]}],
    ?assertEqual([], replication_over_tls_configuration(InitArgsBroken)),

    FileNotFound =
        ?config(data_dir, Config) ++ "inter_node_tls_not_found.config",
    InitArgsNotFound =
        [{proto_dist, ["inet_tls"]}, {ssl_dist_optfile, [FileNotFound]}],
    ?assertEqual([],
                 replication_over_tls_configuration(InitArgsNotFound)),

    ok.

replication_over_tls_configuration(Args) ->
    osiris_util:replication_over_tls_configuration(Args,
                                                   fun tls_replication_log/3).

tls_replication_log(_Level, Fmt, Args) ->
    ct:log(Fmt, Args).
