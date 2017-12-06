
%% Lager
%% ===========================================
-define(ERROR(M, Arg), lager:error(M, Arg)).
-define(WARN(M, Arg), lager:warning(M, Arg)).
-define(INFO(M, Arg), lager:info(M, Arg)).

-define(LOGGER_OUTPUT_MAX_LENGTH, 100).

-ifdef(debug).
-warning("Debug mode is ON!").
-define(DEBUG(M, Arg), lager:debug(M, Arg)).
-else.
-define(DEBUG(M, Arg), ok).
-endif.