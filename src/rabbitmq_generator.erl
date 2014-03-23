%%%-------------------------------------------------------------------
%%% @author mike
%%% @copyright (C) 2014, QixSoft Limited
%%% @doc
%%%
%%% @end
%%% Created : 18. Mar 2014 13:46
%%%-------------------------------------------------------------------
-module(rabbitmq_generator).
-author("mike").

-behaviour(generator).

-include("../deps/amqp_client/include/amqp_client.hrl").

-record(rabbit_state, {channel, queue_name = <<"my_queue">>, exchange = <<"amq.topic">>,
  routing_key = <<"*">>, amqp_params}).

%% API
-export([init/1, run/2]).

-spec(init(Options :: [{atom(),term()}]) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init(Options) ->
  S = parse_options(Options),
  {ok, Connection} = amqp_connection:start(S#rabbit_state.amqp_params),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  NewState = S#rabbit_state{channel = Channel},
  {ok, NewState}.

parse_options(Options) ->
  Hostname = proplists:get_value(host, Options, "localhost"),
  Port = proplists:get_value(port, Options, 5672),
  VirtualHost = proplists:get_value(virtual_host, Options, <<"/">>),
  Username = proplists:get_value(username, Options, <<"guest">>),
  Password = proplists:get_value(password, Options, <<"guest">>),
  Heartbeat = proplists:get_value(heartbeat, Options, 0),
  AmqpParams = #amqp_params_network{host = Hostname, port = Port, virtual_host = VirtualHost,
    username = Username, password = Password, heartbeat = Heartbeat},
  QueueName = proplists:get_value(queue_name, Options, <<"my_queue">>),
  Exchange = proplists:get_value(exchange, Options, <<"amq.topic">>),
  RoutingKey = proplists:get_value(routing_key, Options, <<"*">>),
  RabbitState = #rabbit_state{queue_name = QueueName, exchange = Exchange, routing_key = RoutingKey,
    amqp_params = AmqpParams},
  RabbitState.

-spec(run(From :: pid(), State :: term()) -> no_return()).
run(_From,
    _S = #rabbit_state{channel = Channel, exchange = Exchange, routing_key = RoutingKey, queue_name = QueueName}) ->
  Declare = #'queue.declare'{queue = QueueName},
  #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, Declare),
  Binding = #'queue.bind'{queue = Queue,  exchange = Exchange, routing_key = RoutingKey},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
  Sub = #'basic.consume'{queue = Queue},
  #'basic.consume_ok'{consumer_tag = _Tag} = amqp_channel:call(Channel, Sub), %% the caller is the subscriber
  loop(Channel),
  ok.

loop(Channel) ->
  receive
  %% This is the first message received
    #'basic.consume_ok'{} ->
      loop(Channel);

  %% This is received when the subscription is cancelled
    #'basic.cancel_ok'{} ->
      ok;

  %% A delivery
    {#'basic.deliver'{delivery_tag = Tag}, Content} ->
      %% Do something with the message payload
      %% (some work here)
      #amqp_msg{payload = Payload} = Content,

      ApiLicenceActivated = enterprise_events_licensing_piqi:parse_api_licence_activated(Payload),
      io:format("Payload: ~p~n", [Payload]),

      %% Ack the message
      amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

      %% Loop
      loop(Channel)
  end.
