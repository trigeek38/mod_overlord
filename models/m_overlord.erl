-module(m_overlord).
-author("Jeff Bell jeff@5nineshq.com").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    is_subscribed/1
]).

-include_lib("zotonic.hrl").


m_find_value(is_subscribed, #m{value=undefined}, Context) ->
    is_subscribed(Context);

m_find_value(subscribers, #m{value=undefined}, Context) ->
   get_subscribers(Context);

m_find_value(_Key, #m{value=subscribers}, _Context) ->
    undefined;

m_find_value(_Key, #m{value=undefined}, _Context) ->
   undefined.

m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.

get_subscribers(Context) ->
    Users = z_notifier:first(get_user_pids, Context),
    Users1 = [[{pid, pid_to_list(Pid)},{user, User}] || {Pid, User} <- Users],
    Users1.

is_subscribed(Context) ->
    SessionPid = Context#context.session_pid,
    Users = z_notifier:first(get_user_pids, Context),
    Result = lists:keymember(SessionPid, 1, Users),
    Result.
