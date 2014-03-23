-ifndef(__GOVERNANCE_PIQI_HRL__).
-define(__GOVERNANCE_PIQI_HRL__, 1).


-record(governance_message_handler_activated, {
    instruction :: governance_instruction(),
    message_handler_name :: string() | binary(),
    message_type_ids = [] :: [string() | binary()],
    replay_from :: integer()
}).

-record(governance_resource_saved, {
    instruction :: governance_instruction(),
    resource_type :: string() | binary(),
    resource_id :: string() | binary(),
    resource_version :: integer(),
    resource :: string() | binary()
}).

-record(governance_standard_response, {
    root_response :: governance_response()
}).

-record(governance_failure_response, {
    root_response :: governance_response(),
    error :: string() | binary()
}).

-record(governance_state_changed, {
    instruction :: governance_instruction(),
    state_changes = [] :: [governance_state_change()]
}).

-record(governance_resources_changed, {
    instruction :: governance_instruction(),
    resource_changes = [] :: [governance_resource_change()]
}).

-record(governance_command, {
    instruction :: governance_instruction(),
    aggregate_root_id :: string() | binary(),
    compensating_command :: governance_serialized_message()
}).

-record(governance_enterprise_event, {
    instruction :: governance_instruction(),
    started :: integer(),
    ended :: integer(),
    geographic_location :: governance_geographic_coordinates(),
    group :: string() | binary()
}).

-record(governance_instruction, {
    id :: string() | binary(),
    type :: string() | binary(),
    created :: integer(),
    source :: string() | binary(),
    source_machine_name :: string() | binary(),
    tenant_code :: string() | binary(),
    ttl = 0 :: integer() | 'undefined',
    priority = 0 :: non_neg_integer() | 'undefined',
    username :: string() | binary(),
    causality_vector = [] :: [string() | binary()],
    causal_request_id :: string() | binary(),
    support_id :: string() | binary(),
    why :: string() | binary()
}).

-record(governance_request, {
    instruction :: governance_instruction(),
    aggregate_root_id :: string() | binary()
}).

-record(governance_response, {
    instruction :: governance_instruction(),
    causality :: string() | binary(),
    failure :: string() | binary(),
    was_state_modified :: boolean()
}).

-record(governance_change, {
    action :: string() | binary(),
    path :: string() | binary(),
    type_id :: string() | binary(),
    value :: string() | binary()
}).

-record(governance_geographic_coordinates, {
    longitude :: number(),
    latitude :: number(),
    altitude :: number()
}).

-record(governance_input_endpoint_details, {
    name :: string() | binary(),
    message_handlers = [] :: [governance_message_handler_details()]
}).

-record(governance_message_handler_details, {
    type_name :: string() | binary(),
    group_name :: string() | binary(),
    message_type :: string() | binary()
}).

-record(governance_resource_change, {
    resource_type_id :: string() | binary(),
    resource_id :: string() | binary(),
    resource_version :: integer(),
    resource :: string() | binary()
}).

-record(governance_serialized_message, {
    type :: string() | binary(),
    data :: binary()
}).

-record(governance_state_change, {
    aggregate_type_id :: string() | binary(),
    aggregate_id :: string() | binary(),
    aggregate_version :: integer(),
    aggregate :: string() | binary(),
    when_changed :: integer()
}).

-type governance_message_handler_activated() :: #governance_message_handler_activated{}.

-type governance_resource_saved() :: #governance_resource_saved{}.

-type governance_standard_response() :: #governance_standard_response{}.

-type governance_failure_response() :: #governance_failure_response{}.

-type governance_state_changed() :: #governance_state_changed{}.

-type governance_resources_changed() :: #governance_resources_changed{}.

-type governance_command() :: #governance_command{}.

-type governance_enterprise_event() :: #governance_enterprise_event{}.

-type governance_instruction() :: #governance_instruction{}.

-type governance_request() :: #governance_request{}.

-type governance_response() :: #governance_response{}.

-type governance_change() :: #governance_change{}.

-type governance_geographic_coordinates() :: #governance_geographic_coordinates{}.

-type governance_input_endpoint_details() :: #governance_input_endpoint_details{}.

-type governance_message_handler_details() :: #governance_message_handler_details{}.

-type governance_resource_change() :: #governance_resource_change{}.

-type governance_serialized_message() :: #governance_serialized_message{}.

-type governance_state_change() :: #governance_state_change{}.


-endif.
