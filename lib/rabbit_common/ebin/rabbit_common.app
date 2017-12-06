% vim:ft=erlang:

{application, rabbit_common, [
	{description, ""},
	{vsn, "3.6.6"},
	{id, ""},
	{modules, ['app_utils','code_version','credit_flow','ec_semver','ec_semver_parser','gen_server2','mirrored_supervisor','mochijson2','mochinum','pmon','priority_queue','rabbit_amqqueue','rabbit_auth_backend_dummy','rabbit_auth_backend_internal','rabbit_auth_mechanism','rabbit_authn_backend','rabbit_authz_backend','rabbit_backing_queue','rabbit_basic','rabbit_binary_generator','rabbit_binary_parser','rabbit_channel','rabbit_channel_interceptor','rabbit_command_assembler','rabbit_control_misc','rabbit_data_coercion','rabbit_error_logger_handler','rabbit_event','rabbit_exchange_decorator','rabbit_exchange_type','rabbit_framing_amqp_0_8','rabbit_framing_amqp_0_9_1','rabbit_health_check','rabbit_heartbeat','rabbit_misc','rabbit_msg_store_index','rabbit_net','rabbit_networking','rabbit_nodes','rabbit_password_hashing','rabbit_policy_validator','rabbit_queue_collector','rabbit_queue_decorator','rabbit_queue_master_locator','rabbit_reader','rabbit_runtime_parameter','rabbit_types','rabbit_writer','rand_compat','ssl_compat','supervisor2','time_compat']},
	{registered, []},
	{applications, [
		kernel,
                stdlib,
                xmerl
	]}
]}.
