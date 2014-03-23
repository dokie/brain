-module(enterprise_events_licensing_piqi).
-compile(export_all).

-include_lib("piqi/include/piqirun.hrl").
-include("enterprise_events_licensing_piqi.hrl").


-spec field_gen_string/2 :: (Code :: piqirun_code(), X :: string() | binary()) -> iolist().

field_gen_string(Code, X) ->
    piqirun:string_to_block(Code, X).


-spec field_gen_network_monitoring_licence_registered/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_network_monitoring_licence_registered()) -> iolist().

field_gen_network_monitoring_licence_registered(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_network_monitoring_licence_registered.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_registered.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_network_monitoring_licence_registered.licence_type)
    ]).


-spec field_gen_remote_control_licence_registered/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_remote_control_licence_registered()) -> iolist().

field_gen_remote_control_licence_registered(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_remote_control_licence_registered.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_registered.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_remote_control_licence_registered.licence_type)
    ]).


-spec field_gen_condition_monitoring_licence_registered/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_condition_monitoring_licence_registered()) -> iolist().

field_gen_condition_monitoring_licence_registered(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_condition_monitoring_licence_registered.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_registered.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_condition_monitoring_licence_registered.licence_type)
    ]).


-spec field_gen_api_licence_registered/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_api_licence_registered()) -> iolist().

field_gen_api_licence_registered(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_api_licence_registered.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_registered.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_api_licence_registered.licence_type)
    ]).


-spec field_gen_network_monitoring_licence_activated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_network_monitoring_licence_activated()) -> iolist().

field_gen_network_monitoring_licence_activated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_network_monitoring_licence_activated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_activated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_network_monitoring_licence_activated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_activated.target_id)
    ]).


-spec field_gen_remote_control_licence_activated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_remote_control_licence_activated()) -> iolist().

field_gen_remote_control_licence_activated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_remote_control_licence_activated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_activated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_remote_control_licence_activated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_activated.target_id)
    ]).


-spec field_gen_condition_monitoring_licence_activated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_condition_monitoring_licence_activated()) -> iolist().

field_gen_condition_monitoring_licence_activated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_condition_monitoring_licence_activated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_activated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_condition_monitoring_licence_activated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_activated.target_id)
    ]).


-spec field_gen_api_licence_activated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_api_licence_activated()) -> iolist().

field_gen_api_licence_activated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_api_licence_activated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_activated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_api_licence_activated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_activated.target_id)
    ]).


-spec field_gen_network_monitoring_licence_locked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_network_monitoring_licence_locked()) -> iolist().

field_gen_network_monitoring_licence_locked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_network_monitoring_licence_locked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_locked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_network_monitoring_licence_locked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_locked.target_id)
    ]).


-spec field_gen_remote_control_licence_locked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_remote_control_licence_locked()) -> iolist().

field_gen_remote_control_licence_locked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_remote_control_licence_locked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_locked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_remote_control_licence_locked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_locked.target_id)
    ]).


-spec field_gen_condition_monitoring_licence_locked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_condition_monitoring_licence_locked()) -> iolist().

field_gen_condition_monitoring_licence_locked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_condition_monitoring_licence_locked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_locked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_condition_monitoring_licence_locked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_locked.target_id)
    ]).


-spec field_gen_api_licence_locked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_api_licence_locked()) -> iolist().

field_gen_api_licence_locked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_api_licence_locked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_locked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_api_licence_locked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_locked.target_id)
    ]).


-spec field_gen_network_monitoring_licence_unlocked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_network_monitoring_licence_unlocked()) -> iolist().

field_gen_network_monitoring_licence_unlocked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_network_monitoring_licence_unlocked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_unlocked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_network_monitoring_licence_unlocked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_unlocked.target_id)
    ]).


-spec field_gen_remote_control_licence_unlocked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_remote_control_licence_unlocked()) -> iolist().

field_gen_remote_control_licence_unlocked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_remote_control_licence_unlocked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_unlocked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_remote_control_licence_unlocked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_unlocked.target_id)
    ]).


-spec field_gen_condition_monitoring_licence_unlocked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_condition_monitoring_licence_unlocked()) -> iolist().

field_gen_condition_monitoring_licence_unlocked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_condition_monitoring_licence_unlocked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_unlocked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_condition_monitoring_licence_unlocked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_unlocked.target_id)
    ]).


-spec field_gen_api_licence_unlocked/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_api_licence_unlocked()) -> iolist().

field_gen_api_licence_unlocked(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_api_licence_unlocked.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_unlocked.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_api_licence_unlocked.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_unlocked.target_id)
    ]).


-spec field_gen_network_monitoring_licence_deactivated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_network_monitoring_licence_deactivated()) -> iolist().

field_gen_network_monitoring_licence_deactivated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_network_monitoring_licence_deactivated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_deactivated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_network_monitoring_licence_deactivated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_network_monitoring_licence_deactivated.target_id)
    ]).


-spec field_gen_remote_control_licence_deactivated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_remote_control_licence_deactivated()) -> iolist().

field_gen_remote_control_licence_deactivated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_remote_control_licence_deactivated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_deactivated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_remote_control_licence_deactivated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_remote_control_licence_deactivated.target_id)
    ]).


-spec field_gen_condition_monitoring_licence_deactivated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_condition_monitoring_licence_deactivated()) -> iolist().

field_gen_condition_monitoring_licence_deactivated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_condition_monitoring_licence_deactivated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_deactivated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_condition_monitoring_licence_deactivated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_condition_monitoring_licence_deactivated.target_id)
    ]).


-spec field_gen_api_licence_deactivated/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_api_licence_deactivated()) -> iolist().

field_gen_api_licence_deactivated(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun governance_piqi:field_gen_enterprise_event/2, X#enterprise_events_licensing_api_licence_deactivated.root_event),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_deactivated.licence_id),
        piqirun:gen_required_field(3, fun field_gen_licence_type/2, X#enterprise_events_licensing_api_licence_deactivated.licence_type),
        piqirun:gen_required_field(4, fun field_gen_string/2, X#enterprise_events_licensing_api_licence_deactivated.target_id)
    ]).


-spec field_gen_licence_type/2 :: (Code :: piqirun_code(), X :: enterprise_events_licensing_licence_type()) -> iolist().

field_gen_licence_type(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#enterprise_events_licensing_licence_type.licence_type_id),
        piqirun:gen_required_field(2, fun field_gen_string/2, X#enterprise_events_licensing_licence_type.licence_type_name),
        piqirun:gen_required_field(3, fun field_gen_string/2, X#enterprise_events_licensing_licence_type.product_name),
        piqirun:gen_repeated_field(4, fun field_gen_string/2, X#enterprise_events_licensing_licence_type.licence_targets),
        piqirun:gen_repeated_field(5, fun field_gen_string/2, X#enterprise_events_licensing_licence_type.dependency_type_ids)
    ]).


-spec gen_string/1 :: (X :: string() | binary()) -> iolist().

gen_string(X) ->
    field_gen_string('undefined', X).


-spec gen_network_monitoring_licence_registered/1 :: (X :: enterprise_events_licensing_network_monitoring_licence_registered()) -> iolist().

gen_network_monitoring_licence_registered(X) ->
    field_gen_network_monitoring_licence_registered('undefined', X).


-spec gen_remote_control_licence_registered/1 :: (X :: enterprise_events_licensing_remote_control_licence_registered()) -> iolist().

gen_remote_control_licence_registered(X) ->
    field_gen_remote_control_licence_registered('undefined', X).


-spec gen_condition_monitoring_licence_registered/1 :: (X :: enterprise_events_licensing_condition_monitoring_licence_registered()) -> iolist().

gen_condition_monitoring_licence_registered(X) ->
    field_gen_condition_monitoring_licence_registered('undefined', X).


-spec gen_api_licence_registered/1 :: (X :: enterprise_events_licensing_api_licence_registered()) -> iolist().

gen_api_licence_registered(X) ->
    field_gen_api_licence_registered('undefined', X).


-spec gen_network_monitoring_licence_activated/1 :: (X :: enterprise_events_licensing_network_monitoring_licence_activated()) -> iolist().

gen_network_monitoring_licence_activated(X) ->
    field_gen_network_monitoring_licence_activated('undefined', X).


-spec gen_remote_control_licence_activated/1 :: (X :: enterprise_events_licensing_remote_control_licence_activated()) -> iolist().

gen_remote_control_licence_activated(X) ->
    field_gen_remote_control_licence_activated('undefined', X).


-spec gen_condition_monitoring_licence_activated/1 :: (X :: enterprise_events_licensing_condition_monitoring_licence_activated()) -> iolist().

gen_condition_monitoring_licence_activated(X) ->
    field_gen_condition_monitoring_licence_activated('undefined', X).


-spec gen_api_licence_activated/1 :: (X :: enterprise_events_licensing_api_licence_activated()) -> iolist().

gen_api_licence_activated(X) ->
    field_gen_api_licence_activated('undefined', X).


-spec gen_network_monitoring_licence_locked/1 :: (X :: enterprise_events_licensing_network_monitoring_licence_locked()) -> iolist().

gen_network_monitoring_licence_locked(X) ->
    field_gen_network_monitoring_licence_locked('undefined', X).


-spec gen_remote_control_licence_locked/1 :: (X :: enterprise_events_licensing_remote_control_licence_locked()) -> iolist().

gen_remote_control_licence_locked(X) ->
    field_gen_remote_control_licence_locked('undefined', X).


-spec gen_condition_monitoring_licence_locked/1 :: (X :: enterprise_events_licensing_condition_monitoring_licence_locked()) -> iolist().

gen_condition_monitoring_licence_locked(X) ->
    field_gen_condition_monitoring_licence_locked('undefined', X).


-spec gen_api_licence_locked/1 :: (X :: enterprise_events_licensing_api_licence_locked()) -> iolist().

gen_api_licence_locked(X) ->
    field_gen_api_licence_locked('undefined', X).


-spec gen_network_monitoring_licence_unlocked/1 :: (X :: enterprise_events_licensing_network_monitoring_licence_unlocked()) -> iolist().

gen_network_monitoring_licence_unlocked(X) ->
    field_gen_network_monitoring_licence_unlocked('undefined', X).


-spec gen_remote_control_licence_unlocked/1 :: (X :: enterprise_events_licensing_remote_control_licence_unlocked()) -> iolist().

gen_remote_control_licence_unlocked(X) ->
    field_gen_remote_control_licence_unlocked('undefined', X).


-spec gen_condition_monitoring_licence_unlocked/1 :: (X :: enterprise_events_licensing_condition_monitoring_licence_unlocked()) -> iolist().

gen_condition_monitoring_licence_unlocked(X) ->
    field_gen_condition_monitoring_licence_unlocked('undefined', X).


-spec gen_api_licence_unlocked/1 :: (X :: enterprise_events_licensing_api_licence_unlocked()) -> iolist().

gen_api_licence_unlocked(X) ->
    field_gen_api_licence_unlocked('undefined', X).


-spec gen_network_monitoring_licence_deactivated/1 :: (X :: enterprise_events_licensing_network_monitoring_licence_deactivated()) -> iolist().

gen_network_monitoring_licence_deactivated(X) ->
    field_gen_network_monitoring_licence_deactivated('undefined', X).


-spec gen_remote_control_licence_deactivated/1 :: (X :: enterprise_events_licensing_remote_control_licence_deactivated()) -> iolist().

gen_remote_control_licence_deactivated(X) ->
    field_gen_remote_control_licence_deactivated('undefined', X).


-spec gen_condition_monitoring_licence_deactivated/1 :: (X :: enterprise_events_licensing_condition_monitoring_licence_deactivated()) -> iolist().

gen_condition_monitoring_licence_deactivated(X) ->
    field_gen_condition_monitoring_licence_deactivated('undefined', X).


-spec gen_api_licence_deactivated/1 :: (X :: enterprise_events_licensing_api_licence_deactivated()) -> iolist().

gen_api_licence_deactivated(X) ->
    field_gen_api_licence_deactivated('undefined', X).


-spec gen_licence_type/1 :: (X :: enterprise_events_licensing_licence_type()) -> iolist().

gen_licence_type(X) ->
    field_gen_licence_type('undefined', X).


gen_string(X, Format) ->
    gen_string(X, Format, []).


gen_string(X, Format, Options) ->
    Iolist = gen_string(X),
    piqirun_ext:convert(?MODULE, <<"string">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_network_monitoring_licence_registered(X, Format) ->
    gen_network_monitoring_licence_registered(X, Format, []).


gen_network_monitoring_licence_registered(X, Format, Options) ->
    Iolist = gen_network_monitoring_licence_registered(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceRegistered">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_remote_control_licence_registered(X, Format) ->
    gen_remote_control_licence_registered(X, Format, []).


gen_remote_control_licence_registered(X, Format, Options) ->
    Iolist = gen_remote_control_licence_registered(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceRegistered">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_condition_monitoring_licence_registered(X, Format) ->
    gen_condition_monitoring_licence_registered(X, Format, []).


gen_condition_monitoring_licence_registered(X, Format, Options) ->
    Iolist = gen_condition_monitoring_licence_registered(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceRegistered">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_api_licence_registered(X, Format) ->
    gen_api_licence_registered(X, Format, []).


gen_api_licence_registered(X, Format, Options) ->
    Iolist = gen_api_licence_registered(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceRegistered">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_network_monitoring_licence_activated(X, Format) ->
    gen_network_monitoring_licence_activated(X, Format, []).


gen_network_monitoring_licence_activated(X, Format, Options) ->
    Iolist = gen_network_monitoring_licence_activated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceActivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_remote_control_licence_activated(X, Format) ->
    gen_remote_control_licence_activated(X, Format, []).


gen_remote_control_licence_activated(X, Format, Options) ->
    Iolist = gen_remote_control_licence_activated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceActivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_condition_monitoring_licence_activated(X, Format) ->
    gen_condition_monitoring_licence_activated(X, Format, []).


gen_condition_monitoring_licence_activated(X, Format, Options) ->
    Iolist = gen_condition_monitoring_licence_activated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceActivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_api_licence_activated(X, Format) ->
    gen_api_licence_activated(X, Format, []).


gen_api_licence_activated(X, Format, Options) ->
    Iolist = gen_api_licence_activated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceActivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_network_monitoring_licence_locked(X, Format) ->
    gen_network_monitoring_licence_locked(X, Format, []).


gen_network_monitoring_licence_locked(X, Format, Options) ->
    Iolist = gen_network_monitoring_licence_locked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceLocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_remote_control_licence_locked(X, Format) ->
    gen_remote_control_licence_locked(X, Format, []).


gen_remote_control_licence_locked(X, Format, Options) ->
    Iolist = gen_remote_control_licence_locked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceLocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_condition_monitoring_licence_locked(X, Format) ->
    gen_condition_monitoring_licence_locked(X, Format, []).


gen_condition_monitoring_licence_locked(X, Format, Options) ->
    Iolist = gen_condition_monitoring_licence_locked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceLocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_api_licence_locked(X, Format) ->
    gen_api_licence_locked(X, Format, []).


gen_api_licence_locked(X, Format, Options) ->
    Iolist = gen_api_licence_locked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceLocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_network_monitoring_licence_unlocked(X, Format) ->
    gen_network_monitoring_licence_unlocked(X, Format, []).


gen_network_monitoring_licence_unlocked(X, Format, Options) ->
    Iolist = gen_network_monitoring_licence_unlocked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceUnlocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_remote_control_licence_unlocked(X, Format) ->
    gen_remote_control_licence_unlocked(X, Format, []).


gen_remote_control_licence_unlocked(X, Format, Options) ->
    Iolist = gen_remote_control_licence_unlocked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceUnlocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_condition_monitoring_licence_unlocked(X, Format) ->
    gen_condition_monitoring_licence_unlocked(X, Format, []).


gen_condition_monitoring_licence_unlocked(X, Format, Options) ->
    Iolist = gen_condition_monitoring_licence_unlocked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceUnlocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_api_licence_unlocked(X, Format) ->
    gen_api_licence_unlocked(X, Format, []).


gen_api_licence_unlocked(X, Format, Options) ->
    Iolist = gen_api_licence_unlocked(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceUnlocked">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_network_monitoring_licence_deactivated(X, Format) ->
    gen_network_monitoring_licence_deactivated(X, Format, []).


gen_network_monitoring_licence_deactivated(X, Format, Options) ->
    Iolist = gen_network_monitoring_licence_deactivated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceDeactivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_remote_control_licence_deactivated(X, Format) ->
    gen_remote_control_licence_deactivated(X, Format, []).


gen_remote_control_licence_deactivated(X, Format, Options) ->
    Iolist = gen_remote_control_licence_deactivated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceDeactivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_condition_monitoring_licence_deactivated(X, Format) ->
    gen_condition_monitoring_licence_deactivated(X, Format, []).


gen_condition_monitoring_licence_deactivated(X, Format, Options) ->
    Iolist = gen_condition_monitoring_licence_deactivated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceDeactivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_api_licence_deactivated(X, Format) ->
    gen_api_licence_deactivated(X, Format, []).


gen_api_licence_deactivated(X, Format, Options) ->
    Iolist = gen_api_licence_deactivated(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceDeactivated">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_licence_type(X, Format) ->
    gen_licence_type(X, Format, []).


gen_licence_type(X, Format, Options) ->
    Iolist = gen_licence_type(X),
    piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/LicenceType">>, 'pb', Format, iolist_to_binary(Iolist), Options).


-spec parse_string/1 :: (X :: piqirun_buffer()) -> binary().

parse_string(X) ->
    piqirun:binary_string_of_block(X).


-spec parse_network_monitoring_licence_registered/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_network_monitoring_licence_registered().

parse_network_monitoring_licence_registered(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    piqirun:check_unparsed_fields(R3),
    #enterprise_events_licensing_network_monitoring_licence_registered{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type
    }.


-spec parse_remote_control_licence_registered/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_remote_control_licence_registered().

parse_remote_control_licence_registered(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    piqirun:check_unparsed_fields(R3),
    #enterprise_events_licensing_remote_control_licence_registered{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type
    }.


-spec parse_condition_monitoring_licence_registered/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_condition_monitoring_licence_registered().

parse_condition_monitoring_licence_registered(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    piqirun:check_unparsed_fields(R3),
    #enterprise_events_licensing_condition_monitoring_licence_registered{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type
    }.


-spec parse_api_licence_registered/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_api_licence_registered().

parse_api_licence_registered(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    piqirun:check_unparsed_fields(R3),
    #enterprise_events_licensing_api_licence_registered{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type
    }.


-spec parse_network_monitoring_licence_activated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_network_monitoring_licence_activated().

parse_network_monitoring_licence_activated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_network_monitoring_licence_activated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_remote_control_licence_activated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_remote_control_licence_activated().

parse_remote_control_licence_activated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_remote_control_licence_activated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_condition_monitoring_licence_activated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_condition_monitoring_licence_activated().

parse_condition_monitoring_licence_activated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_condition_monitoring_licence_activated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_api_licence_activated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_api_licence_activated().

parse_api_licence_activated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_api_licence_activated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_network_monitoring_licence_locked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_network_monitoring_licence_locked().

parse_network_monitoring_licence_locked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_network_monitoring_licence_locked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_remote_control_licence_locked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_remote_control_licence_locked().

parse_remote_control_licence_locked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_remote_control_licence_locked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_condition_monitoring_licence_locked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_condition_monitoring_licence_locked().

parse_condition_monitoring_licence_locked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_condition_monitoring_licence_locked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_api_licence_locked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_api_licence_locked().

parse_api_licence_locked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_api_licence_locked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_network_monitoring_licence_unlocked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_network_monitoring_licence_unlocked().

parse_network_monitoring_licence_unlocked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_network_monitoring_licence_unlocked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_remote_control_licence_unlocked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_remote_control_licence_unlocked().

parse_remote_control_licence_unlocked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_remote_control_licence_unlocked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_condition_monitoring_licence_unlocked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_condition_monitoring_licence_unlocked().

parse_condition_monitoring_licence_unlocked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_condition_monitoring_licence_unlocked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_api_licence_unlocked/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_api_licence_unlocked().

parse_api_licence_unlocked(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_api_licence_unlocked{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_network_monitoring_licence_deactivated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_network_monitoring_licence_deactivated().

parse_network_monitoring_licence_deactivated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_network_monitoring_licence_deactivated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_remote_control_licence_deactivated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_remote_control_licence_deactivated().

parse_remote_control_licence_deactivated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_remote_control_licence_deactivated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_condition_monitoring_licence_deactivated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_condition_monitoring_licence_deactivated().

parse_condition_monitoring_licence_deactivated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_condition_monitoring_licence_deactivated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_api_licence_deactivated/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_api_licence_deactivated().

parse_api_licence_deactivated(X) ->
    R0 = piqirun:parse_record(X),
    {_Root_event, R1} = piqirun:parse_required_field(1, fun governance_piqi:parse_enterprise_event/1, R0),
    {_Licence_id, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Licence_type, R3} = piqirun:parse_required_field(3, fun parse_licence_type/1, R2),
    {_Target_id, R4} = piqirun:parse_required_field(4, fun parse_string/1, R3),
    piqirun:check_unparsed_fields(R4),
    #enterprise_events_licensing_api_licence_deactivated{
        root_event = _Root_event,
        licence_id = _Licence_id,
        licence_type = _Licence_type,
        target_id = _Target_id
    }.


-spec parse_licence_type/1 :: (X :: piqirun_buffer()) -> enterprise_events_licensing_licence_type().

parse_licence_type(X) ->
    R0 = piqirun:parse_record(X),
    {_Licence_type_id, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Licence_type_name, R2} = piqirun:parse_required_field(2, fun parse_string/1, R1),
    {_Product_name, R3} = piqirun:parse_required_field(3, fun parse_string/1, R2),
    {_Licence_targets, R4} = piqirun:parse_repeated_field(4, fun parse_string/1, R3),
    {_Dependency_type_ids, R5} = piqirun:parse_repeated_field(5, fun parse_string/1, R4),
    piqirun:check_unparsed_fields(R5),
    #enterprise_events_licensing_licence_type{
        licence_type_id = _Licence_type_id,
        licence_type_name = _Licence_type_name,
        product_name = _Product_name,
        licence_targets = _Licence_targets,
        dependency_type_ids = _Dependency_type_ids
    }.


parse_string(X, Format) ->
    parse_string(X, Format, []).


parse_string(X, Format, Options) ->
    parse_string(
        piqirun_ext:convert(?MODULE, <<"string">>, Format, 'pb', X, Options)).


parse_network_monitoring_licence_registered(X, Format) ->
    parse_network_monitoring_licence_registered(X, Format, []).


parse_network_monitoring_licence_registered(X, Format, Options) ->
    parse_network_monitoring_licence_registered(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceRegistered">>, Format, 'pb', X, Options)).


parse_remote_control_licence_registered(X, Format) ->
    parse_remote_control_licence_registered(X, Format, []).


parse_remote_control_licence_registered(X, Format, Options) ->
    parse_remote_control_licence_registered(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceRegistered">>, Format, 'pb', X, Options)).


parse_condition_monitoring_licence_registered(X, Format) ->
    parse_condition_monitoring_licence_registered(X, Format, []).


parse_condition_monitoring_licence_registered(X, Format, Options) ->
    parse_condition_monitoring_licence_registered(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceRegistered">>, Format, 'pb', X, Options)).


parse_api_licence_registered(X, Format) ->
    parse_api_licence_registered(X, Format, []).


parse_api_licence_registered(X, Format, Options) ->
    parse_api_licence_registered(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceRegistered">>, Format, 'pb', X, Options)).


parse_network_monitoring_licence_activated(X, Format) ->
    parse_network_monitoring_licence_activated(X, Format, []).


parse_network_monitoring_licence_activated(X, Format, Options) ->
    parse_network_monitoring_licence_activated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceActivated">>, Format, 'pb', X, Options)).


parse_remote_control_licence_activated(X, Format) ->
    parse_remote_control_licence_activated(X, Format, []).


parse_remote_control_licence_activated(X, Format, Options) ->
    parse_remote_control_licence_activated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceActivated">>, Format, 'pb', X, Options)).


parse_condition_monitoring_licence_activated(X, Format) ->
    parse_condition_monitoring_licence_activated(X, Format, []).


parse_condition_monitoring_licence_activated(X, Format, Options) ->
    parse_condition_monitoring_licence_activated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceActivated">>, Format, 'pb', X, Options)).


parse_api_licence_activated(X, Format) ->
    parse_api_licence_activated(X, Format, []).


parse_api_licence_activated(X, Format, Options) ->
    parse_api_licence_activated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceActivated">>, Format, 'pb', X, Options)).


parse_network_monitoring_licence_locked(X, Format) ->
    parse_network_monitoring_licence_locked(X, Format, []).


parse_network_monitoring_licence_locked(X, Format, Options) ->
    parse_network_monitoring_licence_locked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceLocked">>, Format, 'pb', X, Options)).


parse_remote_control_licence_locked(X, Format) ->
    parse_remote_control_licence_locked(X, Format, []).


parse_remote_control_licence_locked(X, Format, Options) ->
    parse_remote_control_licence_locked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceLocked">>, Format, 'pb', X, Options)).


parse_condition_monitoring_licence_locked(X, Format) ->
    parse_condition_monitoring_licence_locked(X, Format, []).


parse_condition_monitoring_licence_locked(X, Format, Options) ->
    parse_condition_monitoring_licence_locked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceLocked">>, Format, 'pb', X, Options)).


parse_api_licence_locked(X, Format) ->
    parse_api_licence_locked(X, Format, []).


parse_api_licence_locked(X, Format, Options) ->
    parse_api_licence_locked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceLocked">>, Format, 'pb', X, Options)).


parse_network_monitoring_licence_unlocked(X, Format) ->
    parse_network_monitoring_licence_unlocked(X, Format, []).


parse_network_monitoring_licence_unlocked(X, Format, Options) ->
    parse_network_monitoring_licence_unlocked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceUnlocked">>, Format, 'pb', X, Options)).


parse_remote_control_licence_unlocked(X, Format) ->
    parse_remote_control_licence_unlocked(X, Format, []).


parse_remote_control_licence_unlocked(X, Format, Options) ->
    parse_remote_control_licence_unlocked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceUnlocked">>, Format, 'pb', X, Options)).


parse_condition_monitoring_licence_unlocked(X, Format) ->
    parse_condition_monitoring_licence_unlocked(X, Format, []).


parse_condition_monitoring_licence_unlocked(X, Format, Options) ->
    parse_condition_monitoring_licence_unlocked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceUnlocked">>, Format, 'pb', X, Options)).


parse_api_licence_unlocked(X, Format) ->
    parse_api_licence_unlocked(X, Format, []).


parse_api_licence_unlocked(X, Format, Options) ->
    parse_api_licence_unlocked(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceUnlocked">>, Format, 'pb', X, Options)).


parse_network_monitoring_licence_deactivated(X, Format) ->
    parse_network_monitoring_licence_deactivated(X, Format, []).


parse_network_monitoring_licence_deactivated(X, Format, Options) ->
    parse_network_monitoring_licence_deactivated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/NetworkMonitoringLicenceDeactivated">>, Format, 'pb', X, Options)).


parse_remote_control_licence_deactivated(X, Format) ->
    parse_remote_control_licence_deactivated(X, Format, []).


parse_remote_control_licence_deactivated(X, Format, Options) ->
    parse_remote_control_licence_deactivated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/RemoteControlLicenceDeactivated">>, Format, 'pb', X, Options)).


parse_condition_monitoring_licence_deactivated(X, Format) ->
    parse_condition_monitoring_licence_deactivated(X, Format, []).


parse_condition_monitoring_licence_deactivated(X, Format, Options) ->
    parse_condition_monitoring_licence_deactivated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ConditionMonitoringLicenceDeactivated">>, Format, 'pb', X, Options)).


parse_api_licence_deactivated(X, Format) ->
    parse_api_licence_deactivated(X, Format, []).


parse_api_licence_deactivated(X, Format, Options) ->
    parse_api_licence_deactivated(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/ApiLicenceDeactivated">>, Format, 'pb', X, Options)).


parse_licence_type(X, Format) ->
    parse_licence_type(X, Format, []).


parse_licence_type(X, Format, Options) ->
    parse_licence_type(
        piqirun_ext:convert(?MODULE, <<"EnterpriseEvents_Licensing/LicenceType">>, Format, 'pb', X, Options)).


default_string() -> <<>>.


default_network_monitoring_licence_registered() ->
    #enterprise_events_licensing_network_monitoring_licence_registered{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type()
    }.


default_remote_control_licence_registered() ->
    #enterprise_events_licensing_remote_control_licence_registered{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type()
    }.


default_condition_monitoring_licence_registered() ->
    #enterprise_events_licensing_condition_monitoring_licence_registered{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type()
    }.


default_api_licence_registered() ->
    #enterprise_events_licensing_api_licence_registered{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type()
    }.


default_network_monitoring_licence_activated() ->
    #enterprise_events_licensing_network_monitoring_licence_activated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_remote_control_licence_activated() ->
    #enterprise_events_licensing_remote_control_licence_activated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_condition_monitoring_licence_activated() ->
    #enterprise_events_licensing_condition_monitoring_licence_activated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_api_licence_activated() ->
    #enterprise_events_licensing_api_licence_activated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_network_monitoring_licence_locked() ->
    #enterprise_events_licensing_network_monitoring_licence_locked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_remote_control_licence_locked() ->
    #enterprise_events_licensing_remote_control_licence_locked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_condition_monitoring_licence_locked() ->
    #enterprise_events_licensing_condition_monitoring_licence_locked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_api_licence_locked() ->
    #enterprise_events_licensing_api_licence_locked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_network_monitoring_licence_unlocked() ->
    #enterprise_events_licensing_network_monitoring_licence_unlocked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_remote_control_licence_unlocked() ->
    #enterprise_events_licensing_remote_control_licence_unlocked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_condition_monitoring_licence_unlocked() ->
    #enterprise_events_licensing_condition_monitoring_licence_unlocked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_api_licence_unlocked() ->
    #enterprise_events_licensing_api_licence_unlocked{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_network_monitoring_licence_deactivated() ->
    #enterprise_events_licensing_network_monitoring_licence_deactivated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_remote_control_licence_deactivated() ->
    #enterprise_events_licensing_remote_control_licence_deactivated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_condition_monitoring_licence_deactivated() ->
    #enterprise_events_licensing_condition_monitoring_licence_deactivated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_api_licence_deactivated() ->
    #enterprise_events_licensing_api_licence_deactivated{
        root_event = governance_piqi:default_enterprise_event(),
        licence_id = default_string(),
        licence_type = default_licence_type(),
        target_id = default_string()
    }.


default_licence_type() ->
    #enterprise_events_licensing_licence_type{
        licence_type_id = default_string(),
        licence_type_name = default_string(),
        product_name = default_string(),
        licence_targets = [],
        dependency_type_ids = []
    }.


piqi() ->
[<<226,202,230,52,10,71,111,118,101,114,110,97,110,99,101,160,148,209,72,129,
   248,174,104,226,231,249,238,1,29,46,46,47,100,97,116,97,47,71,111,118,101,
   114,110,97,110,99,101,46,112,114,111,116,111,46,112,105,113,105,162,244,146,
   155,11,26,105,50,79,87,97,116,101,114,46,65,110,97,112,111,115,46,71,111,
   118,101,114,110,97,110,99,101,218,244,134,182,12,128,2,138,233,142,251,14,
   249,1,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,147,
   1,218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,171,
   158,194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,53,
   232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,20,
   109,101,115,115,97,103,101,45,104,97,110,100,108,101,114,45,110,97,109,101,
   210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,49,232,146,150,
   113,6,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,16,109,101,115,
   115,97,103,101,45,116,121,112,101,45,105,100,115,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,49,232,146,150,113,8,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,11,114,101,112,108,97,121,45,102,114,
   111,109,210,171,158,194,6,11,105,110,116,54,52,45,102,105,120,101,100,218,
   164,238,191,4,23,77,101,115,115,97,103,101,72,97,110,100,108,101,114,65,99,
   116,105,118,97,116,101,100,218,244,134,182,12,160,2,138,233,142,251,14,153,
   2,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,171,158,
   194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,46,232,
   146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,13,114,
   101,115,111,117,114,99,101,45,116,121,112,101,210,171,158,194,6,6,115,116,
   114,105,110,103,210,203,242,36,44,232,146,150,113,6,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,11,114,101,115,111,117,114,99,101,45,105,
   100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,57,232,146,
   150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,16,114,101,
   115,111,117,114,99,101,45,118,101,114,115,105,111,110,210,171,158,194,6,14,
   112,114,111,116,111,98,117,102,45,105,110,116,51,50,210,203,242,36,41,232,
   146,150,113,10,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,8,114,
   101,115,111,117,114,99,101,210,171,158,194,6,6,115,116,114,105,110,103,218,
   164,238,191,4,13,82,101,115,111,117,114,99,101,83,97,118,101,100,218,244,
   134,182,12,81,138,233,142,251,14,75,210,203,242,36,48,232,146,150,113,2,152,
   182,154,152,4,223,162,138,147,1,218,164,238,191,4,13,114,111,111,116,45,114,
   101,115,112,111,110,115,101,210,171,158,194,6,8,82,101,115,112,111,110,115,
   101,218,164,238,191,4,16,83,116,97,110,100,97,114,100,82,101,115,112,111,
   110,115,101,218,244,134,182,12,123,138,233,142,251,14,117,210,203,242,36,48,
   232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,13,
   114,111,111,116,45,114,101,115,112,111,110,115,101,210,171,158,194,6,8,82,
   101,115,112,111,110,115,101,210,203,242,36,38,232,146,150,113,4,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,5,101,114,114,111,114,210,171,158,
   194,6,6,115,116,114,105,110,103,218,164,238,191,4,15,70,97,105,108,117,114,
   101,82,101,115,112,111,110,115,101,218,244,134,182,12,135,1,138,233,142,251,
   14,128,1,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,
   171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,
   51,232,146,150,113,6,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,
   13,115,116,97,116,101,45,99,104,97,110,103,101,115,210,171,158,194,6,11,83,
   116,97,116,101,67,104,97,110,103,101,218,164,238,191,4,12,83,116,97,116,101,
   67,104,97,110,103,101,100,218,244,134,182,12,145,1,138,233,142,251,14,138,1,
   210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,171,158,194,
   6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,57,232,146,
   150,113,6,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,16,114,101,
   115,111,117,114,99,101,45,99,104,97,110,103,101,115,210,171,158,194,6,14,82,
   101,115,111,117,114,99,101,67,104,97,110,103,101,218,164,238,191,4,16,82,
   101,115,111,117,114,99,101,115,67,104,97,110,103,101,100,218,244,134,182,12,
   198,1,138,233,142,251,14,191,1,210,203,242,36,49,232,146,150,113,2,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,
   116,105,111,110,210,171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,
   110,210,203,242,36,50,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,17,97,103,103,114,101,103,97,116,101,45,114,111,111,116,
   45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,64,
   232,146,150,113,6,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,20,
   99,111,109,112,101,110,115,97,116,105,110,103,45,99,111,109,109,97,110,100,
   210,171,158,194,6,17,83,101,114,105,97,108,105,122,101,100,77,101,115,115,
   97,103,101,218,164,238,191,4,7,67,111,109,109,97,110,100,218,244,134,182,12,
   167,2,138,233,142,251,14,160,2,210,203,242,36,49,232,146,150,113,2,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,
   116,105,111,110,210,171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,
   110,210,203,242,36,45,232,146,150,113,4,152,182,154,152,4,160,223,186,243,1,
   218,164,238,191,4,7,115,116,97,114,116,101,100,210,171,158,194,6,11,105,110,
   116,54,52,45,102,105,120,101,100,210,203,242,36,43,232,146,150,113,6,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,5,101,110,100,101,100,210,
   171,158,194,6,11,105,110,116,54,52,45,102,105,120,101,100,210,203,242,36,67,
   232,146,150,113,8,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,19,
   103,101,111,103,114,97,112,104,105,99,45,108,111,99,97,116,105,111,110,210,
   171,158,194,6,21,71,101,111,103,114,97,112,104,105,99,67,111,111,114,100,
   105,110,97,116,101,115,210,203,242,36,38,232,146,150,113,10,152,182,154,152,
   4,160,223,186,243,1,218,164,238,191,4,5,103,114,111,117,112,210,171,158,194,
   6,6,115,116,114,105,110,103,218,164,238,191,4,15,69,110,116,101,114,112,114,
   105,115,101,69,118,101,110,116,218,244,134,182,12,192,5,138,233,142,251,14,
   185,5,210,203,242,36,35,232,146,150,113,2,152,182,154,152,4,223,162,138,147,
   1,218,164,238,191,4,2,105,100,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,37,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,4,116,121,112,101,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,45,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,7,99,114,101,97,116,101,100,210,171,158,194,6,11,105,110,116,
   54,52,45,102,105,120,101,100,210,203,242,36,39,232,146,150,113,8,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,6,115,111,117,114,99,101,210,
   171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,52,232,146,150,113,
   10,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,19,115,111,117,114,
   99,101,45,109,97,99,104,105,110,101,45,110,97,109,101,210,171,158,194,6,6,
   115,116,114,105,110,103,210,203,242,36,44,232,146,150,113,12,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,11,116,101,110,97,110,116,45,99,
   111,100,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,78,
   232,146,150,113,14,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,3,
   116,116,108,210,171,158,194,6,11,105,110,116,54,52,45,102,105,120,101,100,
   138,140,251,240,13,31,218,148,211,24,9,9,0,0,0,0,0,0,0,0,210,171,158,194,6,
   11,105,110,116,54,52,45,102,105,120,101,100,210,203,242,36,66,232,146,150,
   113,16,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,8,112,114,105,
   111,114,105,116,121,210,171,158,194,6,6,117,105,110,116,51,50,138,140,251,
   240,13,19,218,148,211,24,2,8,0,210,171,158,194,6,6,117,105,110,116,51,50,
   210,203,242,36,41,232,146,150,113,18,152,182,154,152,4,160,223,186,243,1,
   218,164,238,191,4,8,117,115,101,114,110,97,109,101,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,49,232,146,150,113,20,152,182,154,152,4,
   250,248,214,130,1,218,164,238,191,4,16,99,97,117,115,97,108,105,116,121,45,
   118,101,99,116,111,114,210,171,158,194,6,6,115,116,114,105,110,103,210,203,
   242,36,50,232,146,150,113,22,152,182,154,152,4,160,223,186,243,1,218,164,
   238,191,4,17,99,97,117,115,97,108,45,114,101,113,117,101,115,116,45,105,100,
   210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,43,232,146,150,
   113,24,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,10,115,117,112,
   112,111,114,116,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,
   203,242,36,36,232,146,150,113,26,152,182,154,152,4,160,223,186,243,1,218,
   164,238,191,4,3,119,104,121,210,171,158,194,6,6,115,116,114,105,110,103,218,
   164,238,191,4,11,73,110,115,116,114,117,99,116,105,111,110,218,244,134,182,
   12,128,1,138,233,142,251,14,122,210,203,242,36,49,232,146,150,113,2,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,11,105,110,115,116,114,117,99,
   116,105,111,110,210,171,158,194,6,11,73,110,115,116,114,117,99,116,105,111,
   110,210,203,242,36,50,232,146,150,113,4,152,182,154,152,4,160,223,186,243,1,
   218,164,238,191,4,17,97,103,103,114,101,103,97,116,101,45,114,111,111,116,
   45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,7,
   82,101,113,117,101,115,116,218,244,134,182,12,221,1,138,233,142,251,14,214,
   1,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,11,105,110,115,116,114,117,99,116,105,111,110,210,171,158,
   194,6,11,73,110,115,116,114,117,99,116,105,111,110,210,203,242,36,42,232,
   146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,99,97,
   117,115,97,108,105,116,121,210,171,158,194,6,6,115,116,114,105,110,103,210,
   203,242,36,40,232,146,150,113,6,152,182,154,152,4,160,223,186,243,1,218,164,
   238,191,4,7,102,97,105,108,117,114,101,210,171,158,194,6,6,115,116,114,105,
   110,103,210,203,242,36,49,232,146,150,113,8,152,182,154,152,4,160,223,186,
   243,1,218,164,238,191,4,18,119,97,115,45,115,116,97,116,101,45,109,111,100,
   105,102,105,101,100,210,171,158,194,6,4,98,111,111,108,218,164,238,191,4,8,
   82,101,115,112,111,110,115,101,218,244,134,182,12,193,1,138,233,142,251,14,
   186,1,210,203,242,36,39,232,146,150,113,2,152,182,154,152,4,223,162,138,147,
   1,218,164,238,191,4,6,97,99,116,105,111,110,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,37,232,146,150,113,4,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,4,112,97,116,104,210,171,158,194,6,6,115,116,
   114,105,110,103,210,203,242,36,40,232,146,150,113,6,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,7,116,121,112,101,45,105,100,210,171,158,
   194,6,6,115,116,114,105,110,103,210,203,242,36,38,232,146,150,113,8,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,5,118,97,108,117,101,210,171,
   158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,6,67,104,97,110,103,
   101,218,244,134,182,12,176,1,138,233,142,251,14,169,1,210,203,242,36,43,232,
   146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,108,
   111,110,103,105,116,117,100,101,210,171,158,194,6,7,102,108,111,97,116,54,
   52,210,203,242,36,42,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,8,108,97,116,105,116,117,100,101,210,171,158,194,6,7,102,
   108,111,97,116,54,52,210,203,242,36,42,232,146,150,113,6,152,182,154,152,4,
   160,223,186,243,1,218,164,238,191,4,8,97,108,116,105,116,117,100,101,210,
   171,158,194,6,7,102,108,111,97,116,54,52,218,164,238,191,4,21,71,101,111,
   103,114,97,112,104,105,99,67,111,111,114,100,105,110,97,116,101,115,218,244,
   134,182,12,144,1,138,233,142,251,14,137,1,210,203,242,36,37,232,146,150,113,
   2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,4,110,97,109,101,
   210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,64,232,146,150,
   113,4,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,16,109,101,115,
   115,97,103,101,45,104,97,110,100,108,101,114,115,210,171,158,194,6,21,77,
   101,115,115,97,103,101,72,97,110,100,108,101,114,68,101,116,97,105,108,115,
   218,164,238,191,4,20,73,110,112,117,116,69,110,100,112,111,105,110,116,68,
   101,116,97,105,108,115,218,244,134,182,12,179,1,138,233,142,251,14,172,1,
   210,203,242,36,42,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,9,116,121,112,101,45,110,97,109,101,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,43,232,146,150,113,4,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,10,103,114,111,117,112,45,110,97,109,
   101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,45,232,146,
   150,113,6,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,109,101,
   115,115,97,103,101,45,116,121,112,101,210,171,158,194,6,6,115,116,114,105,
   110,103,218,164,238,191,4,21,77,101,115,115,97,103,101,72,97,110,100,108,
   101,114,68,101,116,97,105,108,115,218,244,134,182,12,238,1,138,233,142,251,
   14,231,1,210,203,242,36,49,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,16,114,101,115,111,117,114,99,101,45,116,121,112,
   101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,
   44,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   11,114,101,115,111,117,114,99,101,45,105,100,210,171,158,194,6,6,115,116,
   114,105,110,103,210,203,242,36,57,232,146,150,113,6,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,16,114,101,115,111,117,114,99,101,45,118,
   101,114,115,105,111,110,210,171,158,194,6,14,112,114,111,116,111,98,117,102,
   45,105,110,116,51,50,210,203,242,36,41,232,146,150,113,8,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,8,114,101,115,111,117,114,99,101,210,
   171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,14,82,101,115,111,
   117,114,99,101,67,104,97,110,103,101,218,244,134,182,12,113,138,233,142,251,
   14,107,210,203,242,36,37,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,4,116,121,112,101,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,37,232,146,150,113,4,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,4,100,97,116,97,210,171,158,194,6,6,98,105,110,
   97,114,121,218,164,238,191,4,17,83,101,114,105,97,108,105,122,101,100,77,
   101,115,115,97,103,101,218,244,134,182,12,166,2,138,233,142,251,14,159,2,
   210,203,242,36,50,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,17,97,103,103,114,101,103,97,116,101,45,116,121,112,101,45,
   105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,45,232,
   146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,97,
   103,103,114,101,103,97,116,101,45,105,100,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,58,232,146,150,113,6,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,17,97,103,103,114,101,103,97,116,101,45,118,101,
   114,115,105,111,110,210,171,158,194,6,14,112,114,111,116,111,98,117,102,45,
   105,110,116,51,50,210,203,242,36,42,232,146,150,113,8,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,9,97,103,103,114,101,103,97,116,101,210,171,
   158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,10,
   152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,119,104,101,110,45,
   99,104,97,110,103,101,100,210,171,158,194,6,11,105,110,116,54,52,45,102,105,
   120,101,100,218,164,238,191,4,11,83,116,97,116,101,67,104,97,110,103,101>>,
 <<226,202,230,52,26,69,110,116,101,114,112,114,105,115,101,69,118,101,110,
   116,115,95,76,105,99,101,110,115,105,110,103,160,148,209,72,129,248,174,
   104,226,231,249,238,1,45,46,46,47,100,97,116,97,47,69,110,116,101,114,112,
   114,105,115,101,69,118,101,110,116,115,95,76,105,99,101,110,115,105,110,
   103,46,112,114,111,116,111,46,112,105,113,105,170,150,212,160,4,15,226,202,
   230,52,10,71,111,118,101,114,110,97,110,99,101,162,244,146,155,11,53,105,
   50,79,87,97,116,101,114,46,65,110,97,112,111,115,46,71,111,118,101,114,110,
   97,110,99,101,46,69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,
   115,46,76,105,99,101,110,115,105,110,103,218,244,134,182,12,218,1,138,233,
   142,251,14,211,1,210,203,242,36,63,232,146,150,113,2,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,101,110,116,
   210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,69,110,116,
   101,114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,43,232,146,
   150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,108,105,
   99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,101,210,171,
   158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,218,164,238,191,4,34,
   78,101,116,119,111,114,107,77,111,110,105,116,111,114,105,110,103,76,105,
   99,101,110,99,101,82,101,103,105,115,116,101,114,101,100,218,244,134,182,
   12,214,1,138,233,142,251,14,207,1,210,203,242,36,63,232,146,150,113,2,152,
   182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,
   101,118,101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,
   101,47,69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,210,203,
   242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,
   121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,
   218,164,238,191,4,30,82,101,109,111,116,101,67,111,110,116,114,111,108,76,
   105,99,101,110,99,101,82,101,103,105,115,116,101,114,101,100,218,244,134,
   182,12,220,1,138,233,142,251,14,213,1,210,203,242,36,63,232,146,150,113,2,
   152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,111,116,
   45,101,118,101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,97,110,
   99,101,47,69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,210,
   203,242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,6,6,
   115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,45,
   116,121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,112,
   101,218,164,238,191,4,36,67,111,110,100,105,116,105,111,110,77,111,110,105,
   116,111,114,105,110,103,76,105,99,101,110,99,101,82,101,103,105,115,116,
   101,114,101,100,218,244,134,182,12,204,1,138,233,142,251,14,197,1,210,203,
   242,36,63,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,10,114,111,111,116,45,101,118,101,110,116,210,171,158,194,6,26,
   71,111,118,101,114,110,97,110,99,101,47,69,110,116,101,114,112,114,105,115,
   101,69,118,101,110,116,210,203,242,36,43,232,146,150,113,4,152,182,154,152,
   4,223,162,138,147,1,218,164,238,191,4,10,108,105,99,101,110,99,101,45,105,
   100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,
   150,113,6,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,
   99,101,110,99,101,45,116,121,112,101,210,171,158,194,6,11,76,105,99,101,
   110,99,101,84,121,112,101,218,164,238,191,4,20,65,112,105,76,105,99,101,
   110,99,101,82,101,103,105,115,116,101,114,101,100,218,244,134,182,12,136,2,
   138,233,142,251,14,129,2,210,203,242,36,63,232,146,150,113,2,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,
   101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,
   69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,
   43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,
   101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,
   242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,9,116,97,114,103,101,116,45,105,100,210,171,158,194,6,6,115,116,
   114,105,110,103,218,164,238,191,4,33,78,101,116,119,111,114,107,77,111,110,
   105,116,111,114,105,110,103,76,105,99,101,110,99,101,65,99,116,105,118,97,
   116,101,100,218,244,134,182,12,132,2,138,233,142,251,14,253,1,210,203,242,
   36,63,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,
   191,4,10,114,111,111,116,45,101,118,101,110,116,210,171,158,194,6,26,71,
   111,118,101,114,110,97,110,99,101,47,69,110,116,101,114,112,114,105,115,
   101,69,118,101,110,116,210,203,242,36,43,232,146,150,113,4,152,182,154,152,
   4,223,162,138,147,1,218,164,238,191,4,10,108,105,99,101,110,99,101,45,105,
   100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,
   150,113,6,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,
   99,101,110,99,101,45,116,121,112,101,210,171,158,194,6,11,76,105,99,101,
   110,99,101,84,121,112,101,210,203,242,36,42,232,146,150,113,8,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,9,116,97,114,103,101,116,45,105,
   100,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,29,82,
   101,109,111,116,101,67,111,110,116,114,111,108,76,105,99,101,110,99,101,65,
   99,116,105,118,97,116,101,100,218,244,134,182,12,138,2,138,233,142,251,14,
   131,2,210,203,242,36,63,232,146,150,113,2,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,101,110,116,210,171,
   158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,69,110,116,101,114,
   112,114,105,115,101,69,118,101,110,116,210,203,242,36,43,232,146,150,113,4,
   152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,108,105,99,101,
   110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,
   242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,101,210,171,158,194,
   6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,242,36,42,232,146,150,
   113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,116,97,114,
   103,101,116,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,218,164,
   238,191,4,35,67,111,110,100,105,116,105,111,110,77,111,110,105,116,111,114,
   105,110,103,76,105,99,101,110,99,101,65,99,116,105,118,97,116,101,100,218,
   244,134,182,12,250,1,138,233,142,251,14,243,1,210,203,242,36,63,232,146,
   150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,
   111,116,45,101,118,101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,
   97,110,99,101,47,69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,
   210,203,242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,
   6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,
   45,116,121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,
   112,101,210,203,242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,9,116,97,114,103,101,116,45,105,100,210,171,158,
   194,6,6,115,116,114,105,110,103,218,164,238,191,4,19,65,112,105,76,105,99,
   101,110,99,101,65,99,116,105,118,97,116,101,100,218,244,134,182,12,133,2,
   138,233,142,251,14,254,1,210,203,242,36,63,232,146,150,113,2,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,
   101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,
   69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,
   43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,
   101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,
   242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,9,116,97,114,103,101,116,45,105,100,210,171,158,194,6,6,115,116,
   114,105,110,103,218,164,238,191,4,30,78,101,116,119,111,114,107,77,111,110,
   105,116,111,114,105,110,103,76,105,99,101,110,99,101,76,111,99,107,101,100,
   218,244,134,182,12,129,2,138,233,142,251,14,250,1,210,203,242,36,63,232,
   146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,114,
   111,111,116,45,101,118,101,110,116,210,171,158,194,6,26,71,111,118,101,114,
   110,97,110,99,101,47,69,110,116,101,114,112,114,105,115,101,69,118,101,110,
   116,210,203,242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,
   1,218,164,238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,171,158,
   194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,6,152,
   182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,
   101,45,116,121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,
   121,112,101,210,203,242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,9,116,97,114,103,101,116,45,105,100,210,171,
   158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,26,82,101,109,111,
   116,101,67,111,110,116,114,111,108,76,105,99,101,110,99,101,76,111,99,107,
   101,100,218,244,134,182,12,135,2,138,233,142,251,14,128,2,210,203,242,36,
   63,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   10,114,111,111,116,45,101,118,101,110,116,210,171,158,194,6,26,71,111,118,
   101,114,110,97,110,99,101,47,69,110,116,101,114,112,114,105,115,101,69,118,
   101,110,116,210,203,242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,
   171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,
   6,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,
   110,99,101,45,116,121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,
   101,84,121,112,101,210,203,242,36,42,232,146,150,113,8,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,9,116,97,114,103,101,116,45,105,100,
   210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,32,67,111,
   110,100,105,116,105,111,110,77,111,110,105,116,111,114,105,110,103,76,105,
   99,101,110,99,101,76,111,99,107,101,100,218,244,134,182,12,247,1,138,233,
   142,251,14,240,1,210,203,242,36,63,232,146,150,113,2,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,101,110,116,
   210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,69,110,116,
   101,114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,43,232,146,
   150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,108,105,
   99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,101,210,171,
   158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,242,36,42,232,
   146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,116,
   97,114,103,101,116,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,
   218,164,238,191,4,16,65,112,105,76,105,99,101,110,99,101,76,111,99,107,101,
   100,218,244,134,182,12,135,2,138,233,142,251,14,128,2,210,203,242,36,63,
   232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,
   114,111,111,116,45,101,118,101,110,116,210,171,158,194,6,26,71,111,118,101,
   114,110,97,110,99,101,47,69,110,116,101,114,112,114,105,115,101,69,118,101,
   110,116,210,203,242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,171,
   158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,6,
   152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,
   110,99,101,45,116,121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,
   101,84,121,112,101,210,203,242,36,42,232,146,150,113,8,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,9,116,97,114,103,101,116,45,105,100,
   210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,32,78,101,
   116,119,111,114,107,77,111,110,105,116,111,114,105,110,103,76,105,99,101,
   110,99,101,85,110,108,111,99,107,101,100,218,244,134,182,12,131,2,138,233,
   142,251,14,252,1,210,203,242,36,63,232,146,150,113,2,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,101,110,116,
   210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,69,110,116,
   101,114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,43,232,146,
   150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,108,105,
   99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,101,210,171,
   158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,242,36,42,232,
   146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,116,
   97,114,103,101,116,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,
   218,164,238,191,4,28,82,101,109,111,116,101,67,111,110,116,114,111,108,76,
   105,99,101,110,99,101,85,110,108,111,99,107,101,100,218,244,134,182,12,137,
   2,138,233,142,251,14,130,2,210,203,242,36,63,232,146,150,113,2,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,
   101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,
   69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,
   43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,
   10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,
   101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,
   242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,9,116,97,114,103,101,116,45,105,100,210,171,158,194,6,6,115,116,
   114,105,110,103,218,164,238,191,4,34,67,111,110,100,105,116,105,111,110,77,
   111,110,105,116,111,114,105,110,103,76,105,99,101,110,99,101,85,110,108,
   111,99,107,101,100,218,244,134,182,12,249,1,138,233,142,251,14,242,1,210,
   203,242,36,63,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,10,114,111,111,116,45,101,118,101,110,116,210,171,158,194,6,
   26,71,111,118,101,114,110,97,110,99,101,47,69,110,116,101,114,112,114,105,
   115,101,69,118,101,110,116,210,203,242,36,43,232,146,150,113,4,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,10,108,105,99,101,110,99,101,45,
   105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,
   146,150,113,6,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,
   105,99,101,110,99,101,45,116,121,112,101,210,171,158,194,6,11,76,105,99,
   101,110,99,101,84,121,112,101,210,203,242,36,42,232,146,150,113,8,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,9,116,97,114,103,101,116,45,
   105,100,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,18,
   65,112,105,76,105,99,101,110,99,101,85,110,108,111,99,107,101,100,218,244,
   134,182,12,138,2,138,233,142,251,14,131,2,210,203,242,36,63,232,146,150,
   113,2,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,111,
   116,45,101,118,101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,97,
   110,99,101,47,69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,
   210,203,242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,
   6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,
   154,152,4,223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,
   45,116,121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,
   112,101,210,203,242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,138,
   147,1,218,164,238,191,4,9,116,97,114,103,101,116,45,105,100,210,171,158,
   194,6,6,115,116,114,105,110,103,218,164,238,191,4,35,78,101,116,119,111,
   114,107,77,111,110,105,116,111,114,105,110,103,76,105,99,101,110,99,101,68,
   101,97,99,116,105,118,97,116,101,100,218,244,134,182,12,134,2,138,233,142,
   251,14,255,1,210,203,242,36,63,232,146,150,113,2,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,101,110,116,210,
   171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,69,110,116,101,
   114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,43,232,146,150,
   113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,108,105,99,
   101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,
   203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,101,210,171,158,
   194,6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,242,36,42,232,146,
   150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,116,97,
   114,103,101,116,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,218,
   164,238,191,4,31,82,101,109,111,116,101,67,111,110,116,114,111,108,76,105,
   99,101,110,99,101,68,101,97,99,116,105,118,97,116,101,100,218,244,134,182,
   12,140,2,138,233,142,251,14,133,2,210,203,242,36,63,232,146,150,113,2,152,
   182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,114,111,111,116,45,
   101,118,101,110,116,210,171,158,194,6,26,71,111,118,101,114,110,97,110,99,
   101,47,69,110,116,101,114,112,114,105,115,101,69,118,101,110,116,210,203,
   242,36,43,232,146,150,113,4,152,182,154,152,4,223,162,138,147,1,218,164,
   238,191,4,10,108,105,99,101,110,99,101,45,105,100,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,50,232,146,150,113,6,152,182,154,152,4,
   223,162,138,147,1,218,164,238,191,4,12,108,105,99,101,110,99,101,45,116,
   121,112,101,210,171,158,194,6,11,76,105,99,101,110,99,101,84,121,112,101,
   210,203,242,36,42,232,146,150,113,8,152,182,154,152,4,223,162,138,147,1,
   218,164,238,191,4,9,116,97,114,103,101,116,45,105,100,210,171,158,194,6,6,
   115,116,114,105,110,103,218,164,238,191,4,37,67,111,110,100,105,116,105,
   111,110,77,111,110,105,116,111,114,105,110,103,76,105,99,101,110,99,101,68,
   101,97,99,116,105,118,97,116,101,100,218,244,134,182,12,252,1,138,233,142,
   251,14,245,1,210,203,242,36,63,232,146,150,113,2,152,182,154,152,4,223,162,
   138,147,1,218,164,238,191,4,10,114,111,111,116,45,101,118,101,110,116,210,
   171,158,194,6,26,71,111,118,101,114,110,97,110,99,101,47,69,110,116,101,
   114,112,114,105,115,101,69,118,101,110,116,210,203,242,36,43,232,146,150,
   113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,10,108,105,99,
   101,110,99,101,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,210,
   203,242,36,50,232,146,150,113,6,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,12,108,105,99,101,110,99,101,45,116,121,112,101,210,171,158,
   194,6,11,76,105,99,101,110,99,101,84,121,112,101,210,203,242,36,42,232,146,
   150,113,8,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,9,116,97,
   114,103,101,116,45,105,100,210,171,158,194,6,6,115,116,114,105,110,103,218,
   164,238,191,4,21,65,112,105,76,105,99,101,110,99,101,68,101,97,99,116,105,
   118,97,116,101,100,218,244,134,182,12,164,2,138,233,142,251,14,157,2,210,
   203,242,36,48,232,146,150,113,2,152,182,154,152,4,223,162,138,147,1,218,
   164,238,191,4,15,108,105,99,101,110,99,101,45,116,121,112,101,45,105,100,
   210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,
   113,4,152,182,154,152,4,223,162,138,147,1,218,164,238,191,4,17,108,105,99,
   101,110,99,101,45,116,121,112,101,45,110,97,109,101,210,171,158,194,6,6,
   115,116,114,105,110,103,210,203,242,36,45,232,146,150,113,6,152,182,154,
   152,4,223,162,138,147,1,218,164,238,191,4,12,112,114,111,100,117,99,116,45,
   110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,
   48,232,146,150,113,8,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,
   15,108,105,99,101,110,99,101,45,116,97,114,103,101,116,115,210,171,158,194,
   6,6,115,116,114,105,110,103,210,203,242,36,52,232,146,150,113,10,152,182,
   154,152,4,250,248,214,130,1,218,164,238,191,4,19,100,101,112,101,110,100,
   101,110,99,121,45,116,121,112,101,45,105,100,115,210,171,158,194,6,6,115,
   116,114,105,110,103,218,164,238,191,4,11,76,105,99,101,110,99,101,84,121,
   112,101>>].
