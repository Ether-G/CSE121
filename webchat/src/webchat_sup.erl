-module(webchat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_one,
            intensity => 5,
            period => 10},
          [#{id => webchat_server,
             start => {webchat_server, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [webchat_server]}
          ]}}.