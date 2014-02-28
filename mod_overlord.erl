-module(mod_overlord).
-author("Jeff Bell <jeff@5nineshq.com>").

-behaviour(gen_server).

-mod_title("Overlord server").
-mod_description("View and interact with logged on users.").
-mod_prio(300).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
         start_link/1, 
         pid_observe_auth_logon_done/3,
         pid_observe_auth_logoff_done/3,
         pid_observe_subscriber_put/3,
         pid_observe_subscriber_delete/3,
         pid_observe_get_user_pids/3,
         pid_observe_subscriber_get/3,
         event/2
        ]).

-include_lib("zotonic.hrl").
-include_lib("overlord.hrl").
-record(state, {context, subscribers=[]}).

%% Module API

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

pid_observe_auth_logon_done(Pid, auth_logon_done, Context) ->
    gen_server:cast(Pid, {update_pids, Context}).

pid_observe_auth_logoff_done(Pid, auth_logoff_done, Context) ->
    gen_server:cast(Pid, {unsubscribe, Context}).

pid_observe_subscriber_put(Pid, #subscriber_put{} = Subscriber, _Context) ->
    gen_server:cast(Pid, Subscriber).

pid_observe_subscriber_delete(Pid, #subscriber_delete{} = Subscriber, _Context) ->
    gen_server:cast(Pid, Subscriber).

pid_observe_get_user_pids(Pid, get_user_pids, _Context) ->
    gen_server:call(Pid, get_user_pids).

pid_observe_subscriber_get(Pid, #subscriber_get{} = Subscriber, _Context) ->
    gen_server:call(Pid, Subscriber).

%% Events

event({postback,{subscriber_put, []}, _TriggerId, _TargetId}, Context) ->
    Pid = Context#context.session_pid,
    User = z_acl:user(Context),
    Subscriber = #subscriber_put{pid=Pid, userid=User},
    z_notifier:first(Subscriber, Context),
    mod_signal:emit({refresh_subscriber_list, []}, Context),
    z_render:growl("Added subscriber entry", notice, false, Context);

event({postback,{subscriber_delete, []}, _TriggerId, _TargetId}, Context) ->
    Pid = Context#context.session_pid,
    UserId = z_acl:user(Context),
    Subscriber = #subscriber_delete{pid=Pid, userid=UserId},
    z_notifier:first(Subscriber, Context),
    mod_signal:emit({refresh_subscriber_list, []}, Context),
    z_render:growl("Deleted subscriber entry", notice, false, Context);
   

event({postback,{subscriber_delete, Args}, _TriggerId, _TargetId}, Context) ->
    Pid = list_to_pid(proplists:get_value(pid, Args)),
    UserId = z_convert:to_integer(proplists:get_value(userid, Args)),
    Subscriber = #subscriber_delete{pid=Pid, userid=UserId},
    z_notifier:first(Subscriber, Context),
    mod_signal:emit({refresh_subscriber_list, []}, Context),
    z_render:growl("Deleted subscriber entry", notice, false, Context);

event({submit, {subscriber_message, Args}, _TriggerId, _TargetId}, Context) ->
    Pid = list_to_pid(proplists:get_value(pid, Args)),
    Msg = z_context:get_q(message, Context, "No message sent"),
    Script = z_script:get_script(z_render:wire([ 
                                                 {dialog, [{title,"Proctor Message"}, {text, Msg}]}
                                               ], Context)),
    z_session:add_script(Script, Pid),
    Context;

event({postback, {update_slide, Args}, _TriggerId, _Target_id}, Context) ->
    SlideDiv = proplists:get_value(target, Args, "slide_content"),
    SlideTemplate = proplists:get_value(template, Args, "_slide_content.tpl"),
    Id = proplists:get_value(id, Args),
    Script = z_script:get_script(z_render:wire([ {update,[{target, SlideDiv},{template,"_slide_content.tpl"}, {id, Id}]},
                                                 {script,[{script, "window.scrollTo(0,0)"}]}
                                               ], Context)),
    Pids = z_notifier:first(get_user_pids, Context), 
    [z_session:add_script(Script, Pid) || {Pid, _UserId} <- Pids],
    Context.


%% gen_server callbacks

init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    {ok, #state{context=z_context:new(Context)}}.

handle_call(get_user_pids, _From, State) ->
    UserPids = [{Pid, UserId} || {Pid, UserId} <- State#state.subscribers, is_process_alive(Pid)], 
    {reply, UserPids, State#state{subscribers=UserPids}};

handle_call(#subscriber_get{userid=UserId}, _From, State) ->
    SubscriberPids = [Pid || {Pid, User} <- State#state.subscribers, User == UserId],
    {reply, SubscriberPids, State}; 

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast({update_pids, Context}, State) ->
    Pid = Context#context.session_pid,
    User = z_acl:user(Context),
    Subscribers = [{Pid2, UserId} || {Pid2, UserId} <- State#state.subscribers, Pid2 =/= Pid],
    NewState = State#state{subscribers=[{Pid, User}|Subscribers]},
    {noreply, NewState};

handle_cast(#subscriber_put{pid=Pid, userid=UserId}, State) ->
    UserPids = [{Pid, UserId} | [{Pid2, UserId2} || {Pid2, UserId2} <- State#state.subscribers, is_process_alive(Pid2), Pid2 =/= Pid]], 
    {noreply, State#state{subscribers=UserPids}};

handle_cast({unsubscribe, Context}, State) ->
    Pid = Context#context.session_pid,
    User = z_acl:user(Context),
    NewSubscribers = [X || X <- State#state.subscribers, X =/= {Pid, User}],
    NewState = State#state{subscribers=NewSubscribers},
    {noreply, NewState};

handle_cast(#subscriber_delete{pid=PidId, userid=UserId}, State) ->
    Subscribers = [{Pid, User} || {Pid, User} <- State#state.subscribers, {Pid,User} =/= {PidId, UserId}],
    {noreply, State#state{subscribers=Subscribers}}; 

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Msg, _Reason, _State) ->
    ok.

%% Other functions
