-ifndef(jd_utils_include_logging_hrl).
-define(jd_utils_include_logging_hrl, true).

-compile({parse_transform, lager_transform}).

-define( log_common(Lvl, Report),
		jd_log:report(Lvl, Report)
	).

% info
-define( log_debug(Report), ?log_common(debug, Report) ).
-define( log_info(Report), ?log_common(info, Report) ).
-define( log_notice(Report), ?log_common(notice, Report) ).

% warning
-define( log_warn(Report), ?log_common(warning, Report) ).

% error
-define( log_error(Report), ?log_common(error, Report) ).
-define( log_crit(Report), ?log_common(critical, Report) ).
-define( log_alert(Report), ?log_common(alert, Report) ).
-define( log_fatal(Report), ?log_common(emergency, Report) ).

-endif. % jd_utils_include_logging_hrl
