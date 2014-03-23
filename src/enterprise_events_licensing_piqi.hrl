-ifndef(__ENTERPRISE_EVENTS_LICENSING_PIQI_HRL__).
-define(__ENTERPRISE_EVENTS_LICENSING_PIQI_HRL__, 1).


-include("governance_piqi.hrl").

-record(enterprise_events_licensing_network_monitoring_licence_registered, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type()
}).

-record(enterprise_events_licensing_remote_control_licence_registered, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type()
}).

-record(enterprise_events_licensing_condition_monitoring_licence_registered, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type()
}).

-record(enterprise_events_licensing_api_licence_registered, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type()
}).

-record(enterprise_events_licensing_network_monitoring_licence_activated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_remote_control_licence_activated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_condition_monitoring_licence_activated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_api_licence_activated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_network_monitoring_licence_locked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_remote_control_licence_locked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_condition_monitoring_licence_locked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_api_licence_locked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_network_monitoring_licence_unlocked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_remote_control_licence_unlocked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_condition_monitoring_licence_unlocked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_api_licence_unlocked, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_network_monitoring_licence_deactivated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_remote_control_licence_deactivated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_condition_monitoring_licence_deactivated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_api_licence_deactivated, {
    root_event :: governance_enterprise_event(),
    licence_id :: string() | binary(),
    licence_type :: enterprise_events_licensing_licence_type(),
    target_id :: string() | binary()
}).

-record(enterprise_events_licensing_licence_type, {
    licence_type_id :: string() | binary(),
    licence_type_name :: string() | binary(),
    product_name :: string() | binary(),
    licence_targets = [] :: [string() | binary()],
    dependency_type_ids = [] :: [string() | binary()]
}).

-type enterprise_events_licensing_network_monitoring_licence_registered() :: #enterprise_events_licensing_network_monitoring_licence_registered{}.

-type enterprise_events_licensing_remote_control_licence_registered() :: #enterprise_events_licensing_remote_control_licence_registered{}.

-type enterprise_events_licensing_condition_monitoring_licence_registered() :: #enterprise_events_licensing_condition_monitoring_licence_registered{}.

-type enterprise_events_licensing_api_licence_registered() :: #enterprise_events_licensing_api_licence_registered{}.

-type enterprise_events_licensing_network_monitoring_licence_activated() :: #enterprise_events_licensing_network_monitoring_licence_activated{}.

-type enterprise_events_licensing_remote_control_licence_activated() :: #enterprise_events_licensing_remote_control_licence_activated{}.

-type enterprise_events_licensing_condition_monitoring_licence_activated() :: #enterprise_events_licensing_condition_monitoring_licence_activated{}.

-type enterprise_events_licensing_api_licence_activated() :: #enterprise_events_licensing_api_licence_activated{}.

-type enterprise_events_licensing_network_monitoring_licence_locked() :: #enterprise_events_licensing_network_monitoring_licence_locked{}.

-type enterprise_events_licensing_remote_control_licence_locked() :: #enterprise_events_licensing_remote_control_licence_locked{}.

-type enterprise_events_licensing_condition_monitoring_licence_locked() :: #enterprise_events_licensing_condition_monitoring_licence_locked{}.

-type enterprise_events_licensing_api_licence_locked() :: #enterprise_events_licensing_api_licence_locked{}.

-type enterprise_events_licensing_network_monitoring_licence_unlocked() :: #enterprise_events_licensing_network_monitoring_licence_unlocked{}.

-type enterprise_events_licensing_remote_control_licence_unlocked() :: #enterprise_events_licensing_remote_control_licence_unlocked{}.

-type enterprise_events_licensing_condition_monitoring_licence_unlocked() :: #enterprise_events_licensing_condition_monitoring_licence_unlocked{}.

-type enterprise_events_licensing_api_licence_unlocked() :: #enterprise_events_licensing_api_licence_unlocked{}.

-type enterprise_events_licensing_network_monitoring_licence_deactivated() :: #enterprise_events_licensing_network_monitoring_licence_deactivated{}.

-type enterprise_events_licensing_remote_control_licence_deactivated() :: #enterprise_events_licensing_remote_control_licence_deactivated{}.

-type enterprise_events_licensing_condition_monitoring_licence_deactivated() :: #enterprise_events_licensing_condition_monitoring_licence_deactivated{}.

-type enterprise_events_licensing_api_licence_deactivated() :: #enterprise_events_licensing_api_licence_deactivated{}.

-type enterprise_events_licensing_licence_type() :: #enterprise_events_licensing_licence_type{}.


-endif.
