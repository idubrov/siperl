%%% @author  Ivan Dubrov <dubrov.ivan@gmail.com>
%%% @doc Common supervisor children specs
%%% @end
%%% @copyright 2011 Ivan Dubrov. See LICENSE file.


-define(SUPERVISOR(Module, Args),
        {Module, {Module, start_link, Args},
         permanent, infinity, supervisor, [Module]}).

-define(SERVER(Name, Module, Args),
        {Name, {Module, start_link, Args},
         permanent, 2000, worker, [Module]}).

-define(SERVER(Name, Module, Args, Modules),
        {Name, {Module, start_link, Args},
         permanent, 2000, worker, Modules}).
