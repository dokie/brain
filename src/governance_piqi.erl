-module(governance_piqi).
-compile(export_all).

-include_lib("piqi/include/piqirun.hrl").
-include("governance_piqi.hrl").


-spec field_gen_binary/2 :: (Code :: piqirun_code(), X :: binary()) -> iolist().

field_gen_binary(Code, X) ->
    piqirun:binary_to_block(Code, X).


-spec field_gen_bool/2 :: (Code :: piqirun_code(), X :: boolean()) -> iolist().

field_gen_bool(Code, X) ->
    piqirun:boolean_to_varint(Code, X).


packed_field_gen_bool(X) ->
    piqirun:boolean_to_packed_varint(X).


-spec field_gen_float64/2 :: (Code :: piqirun_code(), X :: number()) -> iolist().

field_gen_float64(Code, X) ->
    piqirun:float_to_fixed64(Code, X).


packed_field_gen_float64(X) ->
    piqirun:float_to_packed_fixed64(X).


-spec field_gen_int32/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int32(Code, X) ->
    piqirun:integer_to_zigzag_varint(Code, X).


packed_field_gen_int32(X) ->
    piqirun:integer_to_packed_zigzag_varint(X).


-spec field_gen_int64/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int64(Code, X) ->
    piqirun:integer_to_zigzag_varint(Code, X).


packed_field_gen_int64(X) ->
    piqirun:integer_to_packed_zigzag_varint(X).


-spec field_gen_int64_fixed/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int64_fixed(Code, X) ->
    piqirun:integer_to_signed_fixed64(Code, X).


packed_field_gen_int64_fixed(X) ->
    piqirun:integer_to_packed_signed_fixed64(X).


-spec field_gen_protobuf_int32/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_protobuf_int32(Code, X) ->
    piqirun:integer_to_signed_varint(Code, X).


packed_field_gen_protobuf_int32(X) ->
    piqirun:integer_to_packed_signed_varint(X).


-spec field_gen_string/2 :: (Code :: piqirun_code(), X :: string() | binary()) -> iolist().

field_gen_string(Code, X) ->
    piqirun:string_to_block(Code, X).


-spec field_gen_uint32/2 :: (Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().

field_gen_uint32(Code, X) ->
    piqirun:non_neg_integer_to_varint(Code, X).


packed_field_gen_uint32(X) ->
    piqirun:non_neg_integer_to_packed_varint(X).


-spec field_gen_message_handler_activated/2 :: (Code :: piqirun_code(), X :: governance_message_handler_activated()) -> iolist().

field_gen_message_handler_activated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_message_handler_activated.instruction),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_message_handler_activated.message_handler_name),
        piqirun:gen_repeated_field(3, fun field_gen_string/2, X#governance_message_handler_activated.message_type_ids),
        piqirun:gen_required_field(4, fun field_gen_int64_fixed/2, X#governance_message_handler_activated.replay_from)
    ]).


-spec field_gen_resource_saved/2 :: (Code :: piqirun_code(), X :: governance_resource_saved()) -> iolist().

field_gen_resource_saved(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_resource_saved.instruction),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_resource_saved.resource_type),
        piqirun:gen_required_field(3, fun field_gen_string/2, X#governance_resource_saved.resource_id),
        piqirun:gen_required_field(4, fun field_gen_protobuf_int32/2, X#governance_resource_saved.resource_version),
        piqirun:gen_required_field(5, fun field_gen_string/2, X#governance_resource_saved.resource)
    ]).


-spec field_gen_standard_response/2 :: (Code :: piqirun_code(), X :: governance_standard_response()) -> iolist().

field_gen_standard_response(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_response/2, X#governance_standard_response.root_response)
    ]).


-spec field_gen_failure_response/2 :: (Code :: piqirun_code(), X :: governance_failure_response()) -> iolist().

field_gen_failure_response(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_response/2, X#governance_failure_response.root_response),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_failure_response.error)
    ]).


-spec field_gen_state_changed/2 :: (Code :: piqirun_code(), X :: governance_state_changed()) -> iolist().

field_gen_state_changed(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_state_changed.instruction),
        piqirun:gen_repeated_field(3, fun field_gen_state_change/2, X#governance_state_changed.state_changes)
    ]).


-spec field_gen_resources_changed/2 :: (Code :: piqirun_code(), X :: governance_resources_changed()) -> iolist().

field_gen_resources_changed(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_resources_changed.instruction),
        piqirun:gen_repeated_field(3, fun field_gen_resource_change/2, X#governance_resources_changed.resource_changes)
    ]).


-spec field_gen_command/2 :: (Code :: piqirun_code(), X :: governance_command()) -> iolist().

field_gen_command(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_command.instruction),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_command.aggregate_root_id),
        piqirun:gen_optional_field(3, fun field_gen_serialized_message/2, X#governance_command.compensating_command)
    ]).


-spec field_gen_enterprise_event/2 :: (Code :: piqirun_code(), X :: governance_enterprise_event()) -> iolist().

field_gen_enterprise_event(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_enterprise_event.instruction),
        piqirun:gen_optional_field(2, fun field_gen_int64_fixed/2, X#governance_enterprise_event.started),
        piqirun:gen_optional_field(3, fun field_gen_int64_fixed/2, X#governance_enterprise_event.ended),
        piqirun:gen_optional_field(4, fun field_gen_geographic_coordinates/2, X#governance_enterprise_event.geographic_location),
        piqirun:gen_optional_field(5, fun field_gen_string/2, X#governance_enterprise_event.group)
    ]).


-spec field_gen_instruction/2 :: (Code :: piqirun_code(), X :: governance_instruction()) -> iolist().

field_gen_instruction(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#governance_instruction.id),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_instruction.type),
        piqirun:gen_required_field(3, fun field_gen_int64_fixed/2, X#governance_instruction.created),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#governance_instruction.source),
        piqirun:gen_required_field(5, fun field_gen_string/2, X#governance_instruction.source_machine_name),
        piqirun:gen_required_field(6, fun field_gen_string/2, X#governance_instruction.tenant_code),
        piqirun:gen_optional_field(7, fun field_gen_int64_fixed/2, X#governance_instruction.ttl),
        piqirun:gen_optional_field(8, fun field_gen_uint32/2, X#governance_instruction.priority),
        piqirun:gen_optional_field(9, fun field_gen_string/2, X#governance_instruction.username),
        piqirun:gen_repeated_field(10, fun field_gen_string/2, X#governance_instruction.causality_vector),
        piqirun:gen_optional_field(11, fun field_gen_string/2, X#governance_instruction.causal_request_id),
        piqirun:gen_optional_field(12, fun field_gen_string/2, X#governance_instruction.support_id),
        piqirun:gen_optional_field(13, fun field_gen_string/2, X#governance_instruction.why)
    ]).


-spec field_gen_request/2 :: (Code :: piqirun_code(), X :: governance_request()) -> iolist().

field_gen_request(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_request.instruction),
        piqirun:gen_optional_field(2, fun field_gen_string/2, X#governance_request.aggregate_root_id)
    ]).


-spec field_gen_response/2 :: (Code :: piqirun_code(), X :: governance_response()) -> iolist().

field_gen_response(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_instruction/2, X#governance_response.instruction),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_response.causality),
        piqirun:gen_optional_field(3, fun field_gen_string/2, X#governance_response.failure),
        piqirun:gen_optional_field(4, fun field_gen_bool/2, X#governance_response.was_state_modified)
    ]).


-spec field_gen_change/2 :: (Code :: piqirun_code(), X :: governance_change()) -> iolist().

field_gen_change(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#governance_change.action),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_change.path),
        piqirun:gen_required_field(3, fun field_gen_string/2, X#governance_change.type_id),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#governance_change.value)
    ]).


-spec field_gen_geographic_coordinates/2 :: (Code :: piqirun_code(), X :: governance_geographic_coordinates()) -> iolist().

field_gen_geographic_coordinates(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_float64/2, X#governance_geographic_coordinates.longitude),
        piqirun:gen_required_field(2, fun field_gen_float64/2, X#governance_geographic_coordinates.latitude),
        piqirun:gen_optional_field(3, fun field_gen_float64/2, X#governance_geographic_coordinates.altitude)
    ]).


-spec field_gen_input_endpoint_details/2 :: (Code :: piqirun_code(), X :: governance_input_endpoint_details()) -> iolist().

field_gen_input_endpoint_details(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#governance_input_endpoint_details.name),
        piqirun:gen_repeated_field(2, fun field_gen_message_handler_details/2, X#governance_input_endpoint_details.message_handlers)
    ]).


-spec field_gen_message_handler_details/2 :: (Code :: piqirun_code(), X :: governance_message_handler_details()) -> iolist().

field_gen_message_handler_details(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#governance_message_handler_details.type_name),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_message_handler_details.group_name),
        piqirun:gen_required_field(3, fun field_gen_string/2, X#governance_message_handler_details.message_type)
    ]).


-spec field_gen_resource_change/2 :: (Code :: piqirun_code(), X :: governance_resource_change()) -> iolist().

field_gen_resource_change(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#governance_resource_change.resource_type_id),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_resource_change.resource_id),
        piqirun:gen_required_field(3, fun field_gen_protobuf_int32/2, X#governance_resource_change.resource_version),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#governance_resource_change.resource)
    ]).


-spec field_gen_serialized_message/2 :: (Code :: piqirun_code(), X :: governance_serialized_message()) -> iolist().

field_gen_serialized_message(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#governance_serialized_message.type),
        piqirun:gen_required_field(2, fun field_gen_binary/2, X#governance_serialized_message.data)
    ]).


-spec field_gen_state_change/2 :: (Code :: piqirun_code(), X :: governance_state_change()) -> iolist().

field_gen_state_change(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#governance_state_change.aggregate_type_id),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#governance_state_change.aggregate_id),
        piqirun:gen_required_field(3, fun field_gen_protobuf_int32/2, X#governance_state_change.aggregate_version),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#governance_state_change.aggregate),
        piqirun:gen_required_field(5, fun field_gen_int64_fixed/2, X#governance_state_change.when_changed)
    ]).


-spec gen_binary/1 :: (X :: binary()) -> iolist().

gen_binary(X) ->
    field_gen_binary('undefined', X).


-spec gen_bool/1 :: (X :: boolean()) -> iolist().

gen_bool(X) ->
    field_gen_bool('undefined', X).


-spec gen_float64/1 :: (X :: number()) -> iolist().

gen_float64(X) ->
    field_gen_float64('undefined', X).


-spec gen_int32/1 :: (X :: integer()) -> iolist().

gen_int32(X) ->
    field_gen_int32('undefined', X).


-spec gen_int64/1 :: (X :: integer()) -> iolist().

gen_int64(X) ->
    field_gen_int64('undefined', X).


-spec gen_int64_fixed/1 :: (X :: integer()) -> iolist().

gen_int64_fixed(X) ->
    field_gen_int64_fixed('undefined', X).


-spec gen_protobuf_int32/1 :: (X :: integer()) -> iolist().

gen_protobuf_int32(X) ->
    field_gen_protobuf_int32('undefined', X).


-spec gen_string/1 :: (X :: string() | binary()) -> iolist().

gen_string(X) ->
    field_gen_string('undefined', X).


-spec gen_uint32/1 :: (X :: non_neg_integer()) -> iolist().

gen_uint32(X) ->
    field_gen_uint32('undefined', X).


-spec gen_message_handler_activated/1 :: (X :: governance_message_handler_activated()) -> iolist().

gen_message_handler_activated(X) ->
    field_gen_message_handler_activated('undefined', X).


-spec gen_resource_saved/1 :: (X :: governance_resource_saved()) -> iolist().

gen_resource_saved(X) ->
    field_gen_resource_saved('undefined', X).


-spec gen_standard_response/1 :: (X :: governance_standard_response()) -> iolist().

gen_standard_response(X) ->
    field_gen_standard_response('undefined', X).


-spec gen_failure_response/1 :: (X :: governance_failure_response()) -> iolist().

gen_failure_response(X) ->
    field_gen_failure_response('undefined', X).


-spec gen_state_changed/1 :: (X :: governance_state_changed()) -> iolist().

gen_state_changed(X) ->
    field_gen_state_changed('undefined', X).


-spec gen_resources_changed/1 :: (X :: governance_resources_changed()) -> iolist().

gen_resources_changed(X) ->
    field_gen_resources_changed('undefined', X).


-spec gen_command/1 :: (X :: governance_command()) -> iolist().

gen_command(X) ->
    field_gen_command('undefined', X).


-spec gen_enterprise_event/1 :: (X :: governance_enterprise_event()) -> iolist().

gen_enterprise_event(X) ->
    field_gen_enterprise_event('undefined', X).


-spec gen_instruction/1 :: (X :: governance_instruction()) -> iolist().

gen_instruction(X) ->
    field_gen_instruction('undefined', X).


-spec gen_request/1 :: (X :: governance_request()) -> iolist().

gen_request(X) ->
    field_gen_request('undefined', X).


-spec gen_response/1 :: (X :: governance_response()) -> iolist().

gen_response(X) ->
    field_gen_response('undefined', X).


-spec gen_change/1 :: (X :: governance_change()) -> iolist().

gen_change(X) ->
    field_gen_change('undefined', X).


-spec gen_geographic_coordinates/1 :: (X :: governance_geographic_coordinates()) -> iolist().

gen_geographic_coordinates(X) ->
    field_gen_geographic_coordinates('undefined', X).


-spec gen_input_endpoint_details/1 :: (X :: governance_input_endpoint_details()) -> iolist().

gen_input_endpoint_details(X) ->
    field_gen_input_endpoint_details('undefined', X).


-spec gen_message_handler_details/1 :: (X :: governance_message_handler_details()) -> iolist().

gen_message_handler_details(X) ->
    field_gen_message_handler_details('undefined', X).


-spec gen_resource_change/1 :: (X :: governance_resource_change()) -> iolist().

gen_resource_change(X) ->
    field_gen_resource_change('undefined', X).


-spec gen_serialized_message/1 :: (X :: governance_serialized_message()) -> iolist().

gen_serialized_message(X) ->
    field_gen_serialized_message('undefined', X).


-spec gen_state_change/1 :: (X :: governance_state_change()) -> iolist().

gen_state_change(X) ->
    field_gen_state_change('undefined', X).


gen_binary(X, Format) ->
    gen_binary(X, Format, []).


gen_binary(X, Format, Options) ->
    Iolist = gen_binary(X),
    piqirun_ext:convert(?MODULE, <<"binary">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_bool(X, Format) ->
    gen_bool(X, Format, []).


gen_bool(X, Format, Options) ->
    Iolist = gen_bool(X),
    piqirun_ext:convert(?MODULE, <<"bool">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_float64(X, Format) ->
    gen_float64(X, Format, []).


gen_float64(X, Format, Options) ->
    Iolist = gen_float64(X),
    piqirun_ext:convert(?MODULE, <<"float64">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int32(X, Format) ->
    gen_int32(X, Format, []).


gen_int32(X, Format, Options) ->
    Iolist = gen_int32(X),
    piqirun_ext:convert(?MODULE, <<"int32">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int64(X, Format) ->
    gen_int64(X, Format, []).


gen_int64(X, Format, Options) ->
    Iolist = gen_int64(X),
    piqirun_ext:convert(?MODULE, <<"int64">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int64_fixed(X, Format) ->
    gen_int64_fixed(X, Format, []).


gen_int64_fixed(X, Format, Options) ->
    Iolist = gen_int64_fixed(X),
    piqirun_ext:convert(?MODULE, <<"int64-fixed">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_protobuf_int32(X, Format) ->
    gen_protobuf_int32(X, Format, []).


gen_protobuf_int32(X, Format, Options) ->
    Iolist = gen_protobuf_int32(X),
    piqirun_ext:convert(?MODULE, <<"protobuf-int32">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_string(X, Format) ->
    gen_string(X, Format, []).


gen_string(X, Format, Options) ->
    Iolist = gen_string(X),
    piqirun_ext:convert(?MODULE, <<"string">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_uint32(X, Format) ->
    gen_uint32(X, Format, []).


gen_uint32(X, Format, Options) ->
    Iolist = gen_uint32(X),
    piqirun_ext:convert(?MODULE, <<"uint32">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_message_handler_activated(X, Format) ->
    gen_message_handler_activated(X, Format, []).


gen_message_handler_activated(X, Format, Options) ->
    Iolist = gen_message_handler_activated(X),
    piqirun_ext:convert(?MODULE, <<"Governance/MessageHandlerActivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_resource_saved(X, Format) ->
    gen_resource_saved(X, Format, []).


gen_resource_saved(X, Format, Options) ->
    Iolist = gen_resource_saved(X),
    piqirun_ext:convert(?MODULE, <<"Governance/ResourceSaved">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_standard_response(X, Format) ->
    gen_standard_response(X, Format, []).


gen_standard_response(X, Format, Options) ->
    Iolist = gen_standard_response(X),
    piqirun_ext:convert(?MODULE, <<"Governance/StandardResponse">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_failure_response(X, Format) ->
    gen_failure_response(X, Format, []).


gen_failure_response(X, Format, Options) ->
    Iolist = gen_failure_response(X),
    piqirun_ext:convert(?MODULE, <<"Governance/FailureResponse">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_state_changed(X, Format) ->
    gen_state_changed(X, Format, []).


gen_state_changed(X, Format, Options) ->
    Iolist = gen_state_changed(X),
    piqirun_ext:convert(?MODULE, <<"Governance/StateChanged">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_resources_changed(X, Format) ->
    gen_resources_changed(X, Format, []).


gen_resources_changed(X, Format, Options) ->
    Iolist = gen_resources_changed(X),
    piqirun_ext:convert(?MODULE, <<"Governance/ResourcesChanged">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_command(X, Format) ->
    gen_command(X, Format, []).


gen_command(X, Format, Options) ->
    Iolist = gen_command(X),
    piqirun_ext:convert(?MODULE, <<"Governance/Command">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_enterprise_event(X, Format) ->
    gen_enterprise_event(X, Format, []).


gen_enterprise_event(X, Format, Options) ->
    Iolist = gen_enterprise_event(X),
    piqirun_ext:convert(?MODULE, <<"Governance/EnterpriseEvent">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_instruction(X, Format) ->
    gen_instruction(X, Format, []).


gen_instruction(X, Format, Options) ->
    Iolist = gen_instruction(X),
    piqirun_ext:convert(?MODULE, <<"Governance/Instruction">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_request(X, Format) ->
    gen_request(X, Format, []).


gen_request(X, Format, Options) ->
    Iolist = gen_request(X),
    piqirun_ext:convert(?MODULE, <<"Governance/Request">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_response(X, Format) ->
    gen_response(X, Format, []).


gen_response(X, Format, Options) ->
    Iolist = gen_response(X),
    piqirun_ext:convert(?MODULE, <<"Governance/Response">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_change(X, Format) ->
    gen_change(X, Format, []).


gen_change(X, Format, Options) ->
    Iolist = gen_change(X),
    piqirun_ext:convert(?MODULE, <<"Governance/Change">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_geographic_coordinates(X, Format) ->
    gen_geographic_coordinates(X, Format, []).


gen_geographic_coordinates(X, Format, Options) ->
    Iolist = gen_geographic_coordinates(X),
    piqirun_ext:convert(?MODULE, <<"Governance/GeographicCoordinates">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_input_endpoint_details(X, Format) ->
    gen_input_endpoint_details(X, Format, []).


gen_input_endpoint_details(X, Format, Options) ->
    Iolist = gen_input_endpoint_details(X),
    piqirun_ext:convert(?MODULE, <<"Governance/InputEndpointDetails">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_message_handler_details(X, Format) ->
    gen_message_handler_details(X, Format, []).


gen_message_handler_details(X, Format, Options) ->
    Iolist = gen_message_handler_details(X),
    piqirun_ext:convert(?MODULE, <<"Governance/MessageHandlerDetails">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_resource_change(X, Format) ->
    gen_resource_change(X, Format, []).


gen_resource_change(X, Format, Options) ->
    Iolist = gen_resource_change(X),
    piqirun_ext:convert(?MODULE, <<"Governance/ResourceChange">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_serialized_message(X, Format) ->
    gen_serialized_message(X, Format, []).


gen_serialized_message(X, Format, Options) ->
    Iolist = gen_serialized_message(X),
    piqirun_ext:convert(?MODULE, <<"Governance/SerializedMessage">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_state_change(X, Format) ->
    gen_state_change(X, Format, []).


gen_state_change(X, Format, Options) ->
    Iolist = gen_state_change(X),
    piqirun_ext:convert(?MODULE, <<"Governance/StateChange">>, 'pb', Format, iolist_to_binary(Iolist), Options).


-spec parse_binary/1 :: (X :: piqirun_buffer()) -> binary().

parse_binary(X) ->
    piqirun:binary_of_block(X).


-spec parse_bool/1 :: (X :: piqirun_buffer()) -> boolean().

parse_bool(X) ->
    piqirun:boolean_of_varint(X).


packed_parse_bool(X) ->
    {Res, Rest} = piqirun:boolean_of_packed_varint(X),
    {Res, Rest}.


-spec parse_float64/1 :: (X :: piqirun_buffer()) -> float().

parse_float64(X) ->
    piqirun:float_of_fixed64(X).


packed_parse_float64(X) ->
    {Res, Rest} = piqirun:float_of_packed_fixed64(X),
    {Res, Rest}.


-spec parse_int32/1 :: (X :: piqirun_buffer()) -> integer().

parse_int32(X) ->
    piqirun:integer_of_zigzag_varint(X).


packed_parse_int32(X) ->
    {Res, Rest} = piqirun:integer_of_packed_zigzag_varint(X),
    {Res, Rest}.


-spec parse_int64/1 :: (X :: piqirun_buffer()) -> integer().

parse_int64(X) ->
    piqirun:integer_of_zigzag_varint(X).


packed_parse_int64(X) ->
    {Res, Rest} = piqirun:integer_of_packed_zigzag_varint(X),
    {Res, Rest}.


-spec parse_int64_fixed/1 :: (X :: piqirun_buffer()) -> integer().

parse_int64_fixed(X) ->
    piqirun:integer_of_signed_fixed64(X).


packed_parse_int64_fixed(X) ->
    {Res, Rest} = piqirun:integer_of_packed_signed_fixed64(X),
    {Res, Rest}.


-spec parse_protobuf_int32/1 :: (X :: piqirun_buffer()) -> integer().

parse_protobuf_int32(X) ->
    piqirun:integer_of_signed_varint(X).


packed_parse_protobuf_int32(X) ->
    {Res, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {Res, Rest}.


-spec parse_string/1 :: (X :: piqirun_buffer()) -> binary().

parse_string(X) ->
    piqirun:binary_string_of_block(X).


-spec parse_uint32/1 :: (X :: piqirun_buffer()) -> non_neg_integer().

parse_uint32(X) ->
    piqirun:non_neg_integer_of_varint(X).


packed_parse_uint32(X) ->
    {Res, Rest} = piqirun:non_neg_integer_of_packed_varint(X),
    {Res, Rest}.


-spec parse_message_handler_activated/1 :: (X :: piqirun_buffer()) -> governance_message_handler_activated().

parse_message_handler_activated(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_Message_handler_name, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Message_type_ids, R3} = piqirun:parse_repeated_field(3, fun parse_string/1, R2),
    {_Replay_from, R4} = piqirun:parse_required_field(4, fun parse_int64_fixed/1, R3),
    piqirun:check_unparsed_fields(R4),
    #governance_message_handler_activated{
        instruction = _Instruction,
        message_handler_name = _Message_handler_name,
        message_type_ids = _Message_type_ids,
        replay_from = _Replay_from
    }.


-spec parse_resource_saved/1 :: (X :: piqirun_buffer()) -> governance_resource_saved().

parse_resource_saved(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_Resource_type, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Resource_id, R3} = piqirun:parse_required_field(3, fun parse_string/1, R2),
    {_Resource_version, R4} = piqirun:parse_required_field(4, fun parse_protobuf_int32/1, R3),
    {_Resource, R5} = piqirun:parse_required_field(5, fun parse_string/1, R4),
    piqirun:check_unparsed_fields(R5),
    #governance_resource_saved{
        instruction = _Instruction,
        resource_type = _Resource_type,
        resource_id = _Resource_id,
        resource_version = _Resource_version,
        resource = _Resource
    }.


-spec parse_standard_response/1 :: (X :: piqirun_buffer()) -> governance_standard_response().

parse_standard_response(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_response, R1} = piqirun:parse_required_field(1, fun parse_response/1, R0),
    piqirun:check_unparsed_fields(R1),
    #governance_standard_response{
        root_response = _Root_response
    }.


-spec parse_failure_response/1 :: (X :: piqirun_buffer()) -> governance_failure_response().

parse_failure_response(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_response, R1} = piqirun:parse_required_field(1, fun parse_response/1, R0),
    {_Error, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    piqirun:check_unparsed_fields(R2),
    #governance_failure_response{
        root_response = _Root_response,
        error = _Error
    }.


-spec parse_state_changed/1 :: (X :: piqirun_buffer()) -> governance_state_changed().

parse_state_changed(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_State_changes, R2} = piqirun:parse_repeated_field(3, fun parse_state_change/1, R1),
    piqirun:check_unparsed_fields(R2),
    #governance_state_changed{
        instruction = _Instruction,
        state_changes = _State_changes
    }.


-spec parse_resources_changed/1 :: (X :: piqirun_buffer()) -> governance_resources_changed().

parse_resources_changed(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_Resource_changes, R2} = piqirun:parse_repeated_field(3, fun parse_resource_change/1, R1),
    piqirun:check_unparsed_fields(R2),
    #governance_resources_changed{
        instruction = _Instruction,
        resource_changes = _Resource_changes
    }.


-spec parse_command/1 :: (X :: piqirun_buffer()) -> governance_command().

parse_command(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_Aggregate_root_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Compensating_command, R3} = piqirun:parse_optional_field(3, fun parse_serialized_message/1, R2),
    piqirun:check_unparsed_fields(R3),
    #governance_command{
        instruction = _Instruction,
        aggregate_root_id = _Aggregate_root_id,
        compensating_command = _Compensating_command
    }.


-spec parse_enterprise_event/1 :: (X :: piqirun_buffer()) -> governance_enterprise_event().

parse_enterprise_event(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_Started, R2} = piqirun:parse_optional_field(2, fun parse_int64_fixed/1, R1),
    {_Ended, R3} = piqirun:parse_optional_field(3, fun parse_int64_fixed/1, R2),
    {_Geographic_location, R4} = piqirun:parse_optional_field(4, fun parse_geographic_coordinates/1, R3),
    {_Group, R5} = piqirun:parse_optional_field(5, fun parse_string/1, R4),
    piqirun:check_unparsed_fields(R5),
    #governance_enterprise_event{
        instruction = _Instruction,
        started = _Started,
        ended = _Ended,
        geographic_location = _Geographic_location,
        group = _Group
    }.


-spec parse_instruction/1 :: (X :: piqirun_buffer()) -> governance_instruction().

parse_instruction(X) ->
    R0 = piqirun:parse_record(X),
    {_Id, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Type, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Created, R3} = piqirun:parse_required_field(3, fun parse_int64_fixed/1, R2),
    {_Source, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    {_Source_machine_name, R5} = piqirun:parse_required_field(5, fun parse_string/1, R4),
    {_Tenant_code, R6} = piqirun:parse_required_field(6, fun parse_string/1, R5),
    {_Ttl, R7} = piqirun:parse_optional_field(7, fun parse_int64_fixed/1, R6, <<9,0,0,0,0,0,0,0,0>>),
    {_Priority, R8} = piqirun:parse_optional_field(8, fun parse_uint32/1, R7, <<8,0>>),
    {_Username, R9} = piqirun:parse_optional_field(9, fun parse_string/1, R8),
    {_Causality_vector, R10} = piqirun:parse_repeated_field(10, fun parse_string/1, R9),
    {_Causal_request_id, R11} = piqirun:parse_optional_field(11, fun parse_string/1, R10),
    {_Support_id, R12} = piqirun:parse_optional_field(12, fun parse_string/1, R11),
    {_Why, R13} = piqirun:parse_optional_field(13, fun parse_string/1, R12),
    piqirun:check_unparsed_fields(R13),
    #governance_instruction{
        id = _Id,
        type = _Type,
        created = _Created,
        source = _Source,
        source_machine_name = _Source_machine_name,
        tenant_code = _Tenant_code,
        ttl = _Ttl,
        priority = _Priority,
        username = _Username,
        causality_vector = _Causality_vector,
        causal_request_id = _Causal_request_id,
        support_id = _Support_id,
        why = _Why
    }.


-spec parse_request/1 :: (X :: piqirun_buffer()) -> governance_request().

parse_request(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_Aggregate_root_id, R2} = piqirun:parse_optional_field(2, fun parse_string/1, R1),
    piqirun:check_unparsed_fields(R2),
    #governance_request{
        instruction = _Instruction,
        aggregate_root_id = _Aggregate_root_id
    }.


-spec parse_response/1 :: (X :: piqirun_buffer()) -> governance_response().

parse_response(X) ->
    R0 = piqirun:parse_record(X),
    {_Instruction, R1} = piqirun:parse_required_field(1, fun parse_instruction/1, R0),
    {_Causality, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Failure, R3} = piqirun:parse_optional_field(3, fun parse_string/1, R2),
    {_Was_state_modified, R4} = piqirun:parse_optional_field(4, fun parse_bool/1, R3),
    piqirun:check_unparsed_fields(R4),
    #governance_response{
        instruction = _Instruction,
        causality = _Causality,
        failure = _Failure,
        was_state_modified = _Was_state_modified
    }.


-spec parse_change/1 :: (X :: piqirun_buffer()) -> governance_change().

parse_change(X) ->
    R0 = piqirun:parse_record(X),
    {_Action, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Path, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Type_id, R3} = piqirun:parse_required_field(3, fun parse_string/1, R2),
    {_Value, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #governance_change{
        action = _Action,
        path = _Path,
        type_id = _Type_id,
        value = _Value
    }.


-spec parse_geographic_coordinates/1 :: (X :: piqirun_buffer()) -> governance_geographic_coordinates().

parse_geographic_coordinates(X) ->
    R0 = piqirun:parse_record(X),
    {_Longitude, R1} = piqirun:parse_required_field(1, fun parse_float64/1, R0),
    {_Latitude, R2} = piqirun:parse_required_field(2, fun parse_float64/1, R1),
    {_Altitude, R3} = piqirun:parse_optional_field(3, fun parse_float64/1, R2),
    piqirun:check_unparsed_fields(R3),
    #governance_geographic_coordinates{
        longitude = _Longitude,
        latitude = _Latitude,
        altitude = _Altitude
    }.


-spec parse_input_endpoint_details/1 :: (X :: piqirun_buffer()) -> governance_input_endpoint_details().

parse_input_endpoint_details(X) ->
    R0 = piqirun:parse_record(X),
    {_Name, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Message_handlers, R2} = piqirun:parse_repeated_field(2, fun parse_message_handler_details/1, R1),
    piqirun:check_unparsed_fields(R2),
    #governance_input_endpoint_details{
        name = _Name,
        message_handlers = _Message_handlers
    }.


-spec parse_message_handler_details/1 :: (X :: piqirun_buffer()) -> governance_message_handler_details().

parse_message_handler_details(X) ->
    R0 = piqirun:parse_record(X),
    {_Type_name, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Group_name, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Message_type, R3} = piqirun:parse_required_field(3, fun parse_string/1, R2),
    piqirun:check_unparsed_fields(R3),
    #governance_message_handler_details{
        type_name = _Type_name,
        group_name = _Group_name,
        message_type = _Message_type
    }.


-spec parse_resource_change/1 :: (X :: piqirun_buffer()) -> governance_resource_change().

parse_resource_change(X) ->
    R0 = piqirun:parse_record(X),
    {_Resource_type_id, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Resource_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Resource_version, R3} = piqirun:parse_required_field(3, fun parse_protobuf_int32/1, R2),
    {_Resource, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #governance_resource_change{
        resource_type_id = _Resource_type_id,
        resource_id = _Resource_id,
        resource_version = _Resource_version,
        resource = _Resource
    }.


-spec parse_serialized_message/1 :: (X :: piqirun_buffer()) -> governance_serialized_message().

parse_serialized_message(X) ->
    R0 = piqirun:parse_record(X),
    {_Type, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Data, R2} = piqirun:parse_required_field(2, fun parse_binary/1, R1),
    piqirun:check_unparsed_fields(R2),
    #governance_serialized_message{
        type = _Type,
        data = _Data
    }.


-spec parse_state_change/1 :: (X :: piqirun_buffer()) -> governance_state_change().

parse_state_change(X) ->
    R0 = piqirun:parse_record(X),
    {_Aggregate_type_id, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Aggregate_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Aggregate_version, R3} = piqirun:parse_required_field(3, fun parse_protobuf_int32/1, R2),
    {_Aggregate, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    {_When_changed, R5} = piqirun:parse_required_field(5, fun parse_int64_fixed/1, R4),
    piqirun:check_unparsed_fields(R5),
    #governance_state_change{
        aggregate_type_id = _Aggregate_type_id,
        aggregate_id = _Aggregate_id,
        aggregate_version = _Aggregate_version,
        aggregate = _Aggregate,
        when_changed = _When_changed
    }.


parse_binary(X, Format) ->
    parse_binary(X, Format, []).


parse_binary(X, Format, Options) ->
    parse_binary(
        piqirun_ext:convert(?MODULE, <<"binary">>, Format, 'pb', X, Options)).


parse_bool(X, Format) ->
    parse_bool(X, Format, []).


parse_bool(X, Format, Options) ->
    parse_bool(
        piqirun_ext:convert(?MODULE, <<"bool">>, Format, 'pb', X, Options)).


parse_float64(X, Format) ->
    parse_float64(X, Format, []).


parse_float64(X, Format, Options) ->
    parse_float64(
        piqirun_ext:convert(?MODULE, <<"float64">>, Format, 'pb', X, Options)).


parse_int32(X, Format) ->
    parse_int32(X, Format, []).


parse_int32(X, Format, Options) ->
    parse_int32(
        piqirun_ext:convert(?MODULE, <<"int32">>, Format, 'pb', X, Options)).


parse_int64(X, Format) ->
    parse_int64(X, Format, []).


parse_int64(X, Format, Options) ->
    parse_int64(
        piqirun_ext:convert(?MODULE, <<"int64">>, Format, 'pb', X, Options)).


parse_int64_fixed(X, Format) ->
    parse_int64_fixed(X, Format, []).


parse_int64_fixed(X, Format, Options) ->
    parse_int64_fixed(
        piqirun_ext:convert(?MODULE, <<"int64-fixed">>, Format, 'pb', X, Options)).


parse_protobuf_int32(X, Format) ->
    parse_protobuf_int32(X, Format, []).


parse_protobuf_int32(X, Format, Options) ->
    parse_protobuf_int32(
        piqirun_ext:convert(?MODULE, <<"protobuf-int32">>, Format, 'pb', X, Options)).


parse_string(X, Format) ->
    parse_string(X, Format, []).


parse_string(X, Format, Options) ->
    parse_string(
        piqirun_ext:convert(?MODULE, <<"string">>, Format, 'pb', X, Options)).


parse_uint32(X, Format) ->
    parse_uint32(X, Format, []).


parse_uint32(X, Format, Options) ->
    parse_uint32(
        piqirun_ext:convert(?MODULE, <<"uint32">>, Format, 'pb', X, Options)).


parse_message_handler_activated(X, Format) ->
    parse_message_handler_activated(X, Format, []).


parse_message_handler_activated(X, Format, Options) ->
    parse_message_handler_activated(
        piqirun_ext:convert(?MODULE, <<"Governance/MessageHandlerActivated">>, Format, 'pb', X, Options)).


parse_resource_saved(X, Format) ->
    parse_resource_saved(X, Format, []).


parse_resource_saved(X, Format, Options) ->
    parse_resource_saved(
        piqirun_ext:convert(?MODULE, <<"Governance/ResourceSaved">>, Format, 'pb', X, Options)).


parse_standard_response(X, Format) ->
    parse_standard_response(X, Format, []).


parse_standard_response(X, Format, Options) ->
    parse_standard_response(
        piqirun_ext:convert(?MODULE, <<"Governance/StandardResponse">>, Format, 'pb', X, Options)).


parse_failure_response(X, Format) ->
    parse_failure_response(X, Format, []).


parse_failure_response(X, Format, Options) ->
    parse_failure_response(
        piqirun_ext:convert(?MODULE, <<"Governance/FailureResponse">>, Format, 'pb', X, Options)).


parse_state_changed(X, Format) ->
    parse_state_changed(X, Format, []).


parse_state_changed(X, Format, Options) ->
    parse_state_changed(
        piqirun_ext:convert(?MODULE, <<"Governance/StateChanged">>, Format, 'pb', X, Options)).


parse_resources_changed(X, Format) ->
    parse_resources_changed(X, Format, []).


parse_resources_changed(X, Format, Options) ->
    parse_resources_changed(
        piqirun_ext:convert(?MODULE, <<"Governance/ResourcesChanged">>, Format, 'pb', X, Options)).


parse_command(X, Format) ->
    parse_command(X, Format, []).


parse_command(X, Format, Options) ->
    parse_command(
        piqirun_ext:convert(?MODULE, <<"Governance/Command">>, Format, 'pb', X, Options)).


parse_enterprise_event(X, Format) ->
    parse_enterprise_event(X, Format, []).


parse_enterprise_event(X, Format, Options) ->
    parse_enterprise_event(
        piqirun_ext:convert(?MODULE, <<"Governance/EnterpriseEvent">>, Format, 'pb', X, Options)).


parse_instruction(X, Format) ->
    parse_instruction(X, Format, []).


parse_instruction(X, Format, Options) ->
    parse_instruction(
        piqirun_ext:convert(?MODULE, <<"Governance/Instruction">>, Format, 'pb', X, Options)).


parse_request(X, Format) ->
    parse_request(X, Format, []).


parse_request(X, Format, Options) ->
    parse_request(
        piqirun_ext:convert(?MODULE, <<"Governance/Request">>, Format, 'pb', X, Options)).


parse_response(X, Format) ->
    parse_response(X, Format, []).


parse_response(X, Format, Options) ->
    parse_response(
        piqirun_ext:convert(?MODULE, <<"Governance/Response">>, Format, 'pb', X, Options)).


parse_change(X, Format) ->
    parse_change(X, Format, []).


parse_change(X, Format, Options) ->
    parse_change(
        piqirun_ext:convert(?MODULE, <<"Governance/Change">>, Format, 'pb', X, Options)).


parse_geographic_coordinates(X, Format) ->
    parse_geographic_coordinates(X, Format, []).


parse_geographic_coordinates(X, Format, Options) ->
    parse_geographic_coordinates(
        piqirun_ext:convert(?MODULE, <<"Governance/GeographicCoordinates">>, Format, 'pb', X, Options)).


parse_input_endpoint_details(X, Format) ->
    parse_input_endpoint_details(X, Format, []).


parse_input_endpoint_details(X, Format, Options) ->
    parse_input_endpoint_details(
        piqirun_ext:convert(?MODULE, <<"Governance/InputEndpointDetails">>, Format, 'pb', X, Options)).


parse_message_handler_details(X, Format) ->
    parse_message_handler_details(X, Format, []).


parse_message_handler_details(X, Format, Options) ->
    parse_message_handler_details(
        piqirun_ext:convert(?MODULE, <<"Governance/MessageHandlerDetails">>, Format, 'pb', X, Options)).


parse_resource_change(X, Format) ->
    parse_resource_change(X, Format, []).


parse_resource_change(X, Format, Options) ->
    parse_resource_change(
        piqirun_ext:convert(?MODULE, <<"Governance/ResourceChange">>, Format, 'pb', X, Options)).


parse_serialized_message(X, Format) ->
    parse_serialized_message(X, Format, []).


parse_serialized_message(X, Format, Options) ->
    parse_serialized_message(
        piqirun_ext:convert(?MODULE, <<"Governance/SerializedMessage">>, Format, 'pb', X, Options)).


parse_state_change(X, Format) ->
    parse_state_change(X, Format, []).


parse_state_change(X, Format, Options) ->
    parse_state_change(
        piqirun_ext:convert(?MODULE, <<"Governance/StateChange">>, Format, 'pb', X, Options)).


default_binary() -> <<>>.


default_bool() -> false.


default_float64() -> 0.0.


default_int32() -> 0.


default_int64() -> 0.


default_int64_fixed() -> default_int64().


default_protobuf_int32() -> default_int32().


default_string() -> <<>>.


default_uint32() -> 0.


default_message_handler_activated() ->
    #governance_message_handler_activated{
        instruction = default_instruction(),
        message_handler_name = default_string(),
        message_type_ids = [],
        replay_from = default_int64_fixed()
    }.


default_resource_saved() ->
    #governance_resource_saved{
        instruction = default_instruction(),
        resource_type = default_string(),
        resource_id = default_string(),
        resource_version = default_protobuf_int32(),
        resource = default_string()
    }.


default_standard_response() ->
    #governance_standard_response{
        root_response = default_response()
    }.


default_failure_response() ->
    #governance_failure_response{
        root_response = default_response(),
        error = default_string()
    }.


default_state_changed() ->
    #governance_state_changed{
        instruction = default_instruction(),
        state_changes = []
    }.


default_resources_changed() ->
    #governance_resources_changed{
        instruction = default_instruction(),
        resource_changes = []
    }.


default_command() ->
    #governance_command{
        instruction = default_instruction(),
        aggregate_root_id = default_string(),
        compensating_command = 'undefined'
    }.


default_enterprise_event() ->
    #governance_enterprise_event{
        instruction = default_instruction(),
        started = 'undefined',
        ended = 'undefined',
        geographic_location = 'undefined',
        group = 'undefined'
    }.


default_instruction() ->
    #governance_instruction{
        id = default_string(),
        type = default_string(),
        created = default_int64_fixed(),
        source = default_string(),
        source_machine_name = default_string(),
        tenant_code = default_string(),
        ttl = 'undefined',
        priority = 'undefined',
        username = 'undefined',
        causality_vector = [],
        causal_request_id = 'undefined',
        support_id = 'undefined',
        why = 'undefined'
    }.


default_request() ->
    #governance_request{
        instruction = default_instruction(),
        aggregate_root_id = 'undefined'
    }.


default_response() ->
    #governance_response{
        instruction = default_instruction(),
        causality = default_string(),
        failure = 'undefined',
        was_state_modified = 'undefined'
    }.


default_change() ->
    #governance_change{
        action = default_string(),
        path = default_string(),
        type_id = default_string(),
        value = default_string()
    }.


default_geographic_coordinates() ->
    #governance_geographic_coordinates{
        longitude = default_float64(),
        latitude = default_float64(),
        altitude = 'undefined'
    }.


default_input_endpoint_details() ->
    #governance_input_endpoint_details{
        name = default_string(),
        message_handlers = []
    }.


default_message_handler_details() ->
    #governance_message_handler_details{
        type_name = default_string(),
        group_name = default_string(),
        message_type = default_string()
    }.


default_resource_change() ->
    #governance_resource_change{
        resource_type_id = default_string(),
        resource_id = default_string(),
        resource_version = default_protobuf_int32(),
        resource = default_string()
    }.


default_serialized_message() ->
    #governance_serialized_message{
        type = default_string(),
        data = default_binary()
    }.


default_state_change() ->
    #governance_state_change{
        aggregate_type_id = default_string(),
        aggregate_id = default_string(),
        aggregate_version = default_protobuf_int32(),
        aggregate = default_string(),
        when_changed = default_int64_fixed()
    }.


piqi() ->
[<<226,202,230,52,10,71,111,118,101,114,110,97,110,99,101,160,148,209,72,129,
   248,174,104,226,231,249,238,1,29,46,46,47,100,97,116,97,47,71,111,118,101,
   114,110,97,110,99,101,46,112,114,111,116,111,46,112,105,113,105,162,244,
   146,155,11,26,105,50,79,87,97,116,101,114,46,65,110,97,112,111,115,46,71,
   111,118,101,114,110,97,110,99,101,218,244,134,182,12,128,2,138,233,142,251,
   14,249,1,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,
   171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,
   53,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   20,109,101,115,115,97,103,101,45,104,97,110,100,108,101,114,45,110,97,109,
   101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,49,232,146,
   150,113,6,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,16,109,101,
   115,115,97,103,101,45,116,121,112,101,45,105,100,115,210,171,158,194,6,6,
   115,116,114,105,110,103,210,203,242,36,49,232,146,150,113,8,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,11,114,101,112,108,97,121,45,102,
   114,111,109,210,171,158,194,6,11,105,110,116,54,52,45,102,105,120,101,100,
   218,164,238,191,4,23,77,101,115,115,97,103,101,72,97,110,100,108,101,114,
   65,99,116,105,118,97,116,101,100,218,244,134,182,12,160,2,138,233,142,251,
   14,153,2,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,
   171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,
   46,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   13,114,101,115,111,117,114,99,101,45,116,121,112,101,210,171,158,194,6,6,
   115,116,114,105,110,103,210,203,242,36,44,232,146,150,113,6,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,11,114,101,115,111,117,114,99,
   101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,
   57,232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   16,114,101,115,111,117,114,99,101,45,118,101,114,115,105,111,110,210,171,
   158,194,6,14,112,114,111,116,111,98,117,102,45,105,110,116,51,50,210,203,
   242,36,41,232,146,150,113,10,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,8,114,101,115,111,117,114,99,101,210,171,158,194,6,6,115,116,114,
   105,110,103,218,164,238,191,4,13,82,101,115,111,117,114,99,101,83,97,118,
   101,100,218,244,134,182,12,81,138,233,142,251,14,75,210,203,242,36,48,232,
   146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,13,114,
   111,111,116,45,114,101,115,112,111,110,115,101,210,171,158,194,6,8,82,101,
   115,112,111,110,115,101,218,164,238,191,4,16,83,116,97,110,100,97,114,100,
   82,101,115,112,111,110,115,101,218,244,134,182,12,123,138,233,142,251,14,
   117,210,203,242,36,48,232,146,150,113,2,152,182,154,152,4,223,162,138,147,
   1,218,164,238,191,4,13,114,111,111,116,45,114,101,115,112,111,110,115,101,
   210,171,158,194,6,8,82,101,115,112,111,110,115,101,210,203,242,36,38,232,
   146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,5,101,
   114,114,111,114,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,
   191,4,15,70,97,105,108,117,114,101,82,101,115,112,111,110,115,101,218,244,
   134,182,12,135,1,138,233,142,251,14,128,1,210,203,242,36,49,232,146,150,
   113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,11,105,110,115,
   116,114,117,99,116,105,111,110,210,171,158,194,6,11,73,110,115,116,114,117,
   99,116,105,111,110,210,203,242,36,51,232,146,150,113,6,152,182,154,152,4,
   250,248,214,130,1,218,164,238,191,4,13,115,116,97,116,101,45,99,104,97,110,
   103,101,115,210,171,158,194,6,11,83,116,97,116,101,67,104,97,110,103,101,
   218,164,238,191,4,12,83,116,97,116,101,67,104,97,110,103,101,100,218,244,
   134,182,12,145,1,138,233,142,251,14,138,1,210,203,242,36,49,232,146,150,
   113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,11,105,110,115,
   116,114,117,99,116,105,111,110,210,171,158,194,6,11,73,110,115,116,114,117,
   99,116,105,111,110,210,203,242,36,57,232,146,150,113,6,152,182,154,152,4,
   250,248,214,130,1,218,164,238,191,4,16,114,101,115,111,117,114,99,101,45,
   99,104,97,110,103,101,115,210,171,158,194,6,14,82,101,115,111,117,114,99,
   101,67,104,97,110,103,101,218,164,238,191,4,16,82,101,115,111,117,114,99,
   101,115,67,104,97,110,103,101,100,218,244,134,182,12,198,1,138,233,142,251,
   14,191,1,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,
   171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,
   50,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   17,97,103,103,114,101,103,97,116,101,45,114,111,111,116,45,105,100,210,171,
   158,194,6,6,115,116,114,105,110,103,210,203,242,36,64,232,146,150,113,6,
   152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,20,99,111,109,112,
   101,110,115,97,116,105,110,103,45,99,111,109,109,97,110,100,210,171,158,
   194,6,17,83,101,114,105,97,108,105,122,101,100,77,101,115,115,97,103,101,
   218,164,238,191,4,7,67,111,109,109,97,110,100,218,244,134,182,12,167,2,138,
   233,142,251,14,160,2,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,
   111,110,210,171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,
   203,242,36,45,232,146,150,113,4,152,182,154,152,4,160,223,186,243,1,218,
   164,238,191,4,7,115,116,97,114,116,101,100,210,171,158,194,6,11,105,110,
   116,54,52,45,102,105,120,101,100,210,203,242,36,43,232,146,150,113,6,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,5,101,110,100,101,100,
   210,171,158,194,6,11,105,110,116,54,52,45,102,105,120,101,100,210,203,242,
   36,67,232,146,150,113,8,152,182,154,152,4,160,223,186,243,1,218,164,238,
   191,4,19,103,101,111,103,114,97,112,104,105,99,45,108,111,99,97,116,105,
   111,110,210,171,158,194,6,21,71,101,111,103,114,97,112,104,105,99,67,111,
   111,114,100,105,110,97,116,101,115,210,203,242,36,38,232,146,150,113,10,
   152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,5,103,114,111,117,
   112,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,15,69,
   110,116,101,114,112,114,105,115,101,69,118,101,110,116,218,244,134,182,12,
   192,5,138,233,142,251,14,185,5,210,203,242,36,35,232,146,150,113,2,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,2,105,100,210,171,158,194,6,
   6,115,116,114,105,110,103,210,203,242,36,37,232,146,150,113,4,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,4,116,121,112,101,210,171,158,
   194,6,6,115,116,114,105,110,103,210,203,242,36,45,232,146,150,113,6,152,
   182,154,152,4,223,162,138,147,1,218,164,238,191,4,7,99,114,101,97,116,101,
   100,210,171,158,194,6,11,105,110,116,54,52,45,102,105,120,101,100,210,203,
   242,36,39,232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,6,115,111,117,114,99,101,210,171,158,194,6,6,115,116,114,105,110,
   103,210,203,242,36,52,232,146,150,113,10,152,182,154,152,4,223,162,138,147,
   1,218,164,238,191,4,19,115,111,117,114,99,101,45,109,97,99,104,105,110,101,
   45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,
   36,44,232,146,150,113,12,152,182,154,152,4,223,162,138,147,1,218,164,238,
   191,4,11,116,101,110,97,110,116,45,99,111,100,101,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,78,232,146,150,113,14,152,182,154,152,4,
   160,223,186,243,1,218,164,238,191,4,3,116,116,108,210,171,158,194,6,11,105,
   110,116,54,52,45,102,105,120,101,100,138,140,251,240,13,31,218,148,211,24,
   9,9,0,0,0,0,0,0,0,0,210,171,158,194,6,11,105,110,116,54,52,45,102,105,120,
   101,100,210,203,242,36,66,232,146,150,113,16,152,182,154,152,4,160,223,186,
   243,1,218,164,238,191,4,8,112,114,105,111,114,105,116,121,210,171,158,194,
   6,6,117,105,110,116,51,50,138,140,251,240,13,19,218,148,211,24,2,8,0,210,
   171,158,194,6,6,117,105,110,116,51,50,210,203,242,36,41,232,146,150,113,18,
   152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,8,117,115,101,114,
   110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,
   49,232,146,150,113,20,152,182,154,152,4,250,248,214,130,1,218,164,238,191,
   4,16,99,97,117,115,97,108,105,116,121,45,118,101,99,116,111,114,210,171,
   158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,22,
   152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,17,99,97,117,115,97,
   108,45,114,101,113,117,101,115,116,45,105,100,210,171,158,194,6,6,115,116,
   114,105,110,103,210,203,242,36,43,232,146,150,113,24,152,182,154,152,4,160,
   223,186,243,1,218,164,238,191,4,10,115,117,112,112,111,114,116,45,105,100,
   210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,36,232,146,150,
   113,26,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,3,119,104,121,
   210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,11,73,110,
   115,116,114,117,99,116,105,111,110,218,244,134,182,12,128,1,138,233,142,
   251,14,122,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,
   210,171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,
   36,50,232,146,150,113,4,152,182,154,152,4,160,223,186,243,1,218,164,238,
   191,4,17,97,103,103,114,101,103,97,116,101,45,114,111,111,116,45,105,100,
   210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,7,82,101,113,
   117,101,115,116,218,244,134,182,12,221,1,138,233,142,251,14,214,1,210,203,
   242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,171,158,194,6,
   11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,42,232,146,150,
   113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,99,97,117,
   115,97,108,105,116,121,210,171,158,194,6,6,115,116,114,105,110,103,210,203,
   242,36,40,232,146,150,113,6,152,182,154,152,4,160,223,186,243,1,218,164,
   238,191,4,7,102,97,105,108,117,114,101,210,171,158,194,6,6,115,116,114,105,
   110,103,210,203,242,36,49,232,146,150,113,8,152,182,154,152,4,160,223,186,
   243,1,218,164,238,191,4,18,119,97,115,45,115,116,97,116,101,45,109,111,100,
   105,102,105,101,100,210,171,158,194,6,4,98,111,111,108,218,164,238,191,4,8,
   82,101,115,112,111,110,115,101,218,244,134,182,12,193,1,138,233,142,251,14,
   186,1,210,203,242,36,39,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,6,97,99,116,105,111,110,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,37,232,146,150,113,4,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,4,112,97,116,104,210,171,158,194,6,6,
   115,116,114,105,110,103,210,203,242,36,40,232,146,150,113,6,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,7,116,121,112,101,45,105,100,210,
   171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,38,232,146,150,113,
   8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,5,118,97,108,117,
   101,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,6,67,104,
   97,110,103,101,218,244,134,182,12,176,1,138,233,142,251,14,169,1,210,203,
   242,36,43,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,9,108,111,110,103,105,116,117,100,101,210,171,158,194,6,7,102,
   108,111,97,116,54,52,210,203,242,36,42,232,146,150,113,4,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,8,108,97,116,105,116,117,100,101,210,
   171,158,194,6,7,102,108,111,97,116,54,52,210,203,242,36,42,232,146,150,113,
   6,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,8,97,108,116,105,
   116,117,100,101,210,171,158,194,6,7,102,108,111,97,116,54,52,218,164,238,
   191,4,21,71,101,111,103,114,97,112,104,105,99,67,111,111,114,100,105,110,
   97,116,101,115,218,244,134,182,12,144,1,138,233,142,251,14,137,1,210,203,
   242,36,37,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,4,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,
   203,242,36,64,232,146,150,113,4,152,182,154,152,4,250,248,214,130,1,218,
   164,238,191,4,16,109,101,115,115,97,103,101,45,104,97,110,100,108,101,114,
   115,210,171,158,194,6,21,77,101,115,115,97,103,101,72,97,110,100,108,101,
   114,68,101,116,97,105,108,115,218,164,238,191,4,20,73,110,112,117,116,69,
   110,100,112,111,105,110,116,68,101,116,97,105,108,115,218,244,134,182,12,
   179,1,138,233,142,251,14,172,1,210,203,242,36,42,232,146,150,113,2,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,9,116,121,112,101,45,110,97,
   109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,43,232,
   146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,103,
   114,111,117,112,45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,
   103,210,203,242,36,45,232,146,150,113,6,152,182,154,152,4,223,162,138,147,
   1,218,164,238,191,4,12,109,101,115,115,97,103,101,45,116,121,112,101,210,
   171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,21,77,101,115,
   115,97,103,101,72,97,110,100,108,101,114,68,101,116,97,105,108,115,218,244,
   134,182,12,238,1,138,233,142,251,14,231,1,210,203,242,36,49,232,146,150,
   113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,16,114,101,115,
   111,117,114,99,101,45,116,121,112,101,45,105,100,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,44,232,146,150,113,4,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,11,114,101,115,111,117,114,99,101,45,
   105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,57,232,
   146,150,113,6,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,16,114,
   101,115,111,117,114,99,101,45,118,101,114,115,105,111,110,210,171,158,194,
   6,14,112,114,111,116,111,98,117,102,45,105,110,116,51,50,210,203,242,36,41,
   232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,8,
   114,101,115,111,117,114,99,101,210,171,158,194,6,6,115,116,114,105,110,103,
   218,164,238,191,4,14,82,101,115,111,117,114,99,101,67,104,97,110,103,101,
   218,244,134,182,12,113,138,233,142,251,14,107,210,203,242,36,37,232,146,
   150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,4,116,121,
   112,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,37,232,
   146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,4,100,
   97,116,97,210,171,158,194,6,6,98,105,110,97,114,121,218,164,238,191,4,17,
   83,101,114,105,97,108,105,122,101,100,77,101,115,115,97,103,101,218,244,
   134,182,12,166,2,138,233,142,251,14,159,2,210,203,242,36,50,232,146,150,
   113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,17,97,103,103,
   114,101,103,97,116,101,45,116,121,112,101,45,105,100,210,171,158,194,6,6,
   115,116,114,105,110,103,210,203,242,36,45,232,146,150,113,4,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,12,97,103,103,114,101,103,97,116,
   101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,
   58,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   17,97,103,103,114,101,103,97,116,101,45,118,101,114,115,105,111,110,210,
   171,158,194,6,14,112,114,111,116,111,98,117,102,45,105,110,116,51,50,210,
   203,242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,9,97,103,103,114,101,103,97,116,101,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,50,232,146,150,113,10,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,12,119,104,101,110,45,99,104,97,110,
   103,101,100,210,171,158,194,6,11,105,110,116,54,52,45,102,105,120,101,100,
   218,164,238,191,4,11,83,116,97,116,101,67,104,97,110,103,101>>].
