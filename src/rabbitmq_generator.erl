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

-include("../deps/amqp_client-3.2.4/include/amqp_client.hrl").

%% API
-export([init/1, run/2]).

-spec(init(Options :: list(term())) -> {ok, State :: term()} | tuple(error, Reason :: string())).
init([]) ->
  AmqpParams = #amqp_params_network{host = "192.168.0.133", virtual_host = <<"anapos-events">>},
  {ok, Connection} = amqp_connection:start(AmqpParams),
  amqp_connection:open_channel(Connection).

-spec(run(From :: pid(), State :: term()) -> no_return()).
run(_From, Channel) ->
  Declare = #'queue.declare'{queue = <<"my_queue">>},
  #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, Declare),
  Binding = #'queue.bind'{queue = Queue,  exchange = <<"anapos.topic">>, routing_key = <<"Anapos.AccessControl.UserLoggedIn">>},
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

      io:format("Payload: ~p~n", [Payload]),

      %% Ack the message
      amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

      %% Loop
      loop(Channel)
  end.
