%% This file is part of Twoorl.
%% 
%% Twoorl is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% Twoorl is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with Twoorl.  If not, see <http://www.gnu.org/licenses/>.
%%
%% Copyright Nick Gerakines, 2008
%%
%% @author Nick Gerakines <nick@gerakinesnet> [http://blog.socklabs.com/]
%% @copyright Nick Gerakines, 2008

-module(stats_controller).
-compile(export_all).

%% disabled for now
private() ->
    true.

index(_A) ->
    Registration = twoorl_stats:call({graph, registration, 7}),
    Updates = twoorl_stats:call({graph, twoot, 7}),
    {data, {Registration, Updates}}.
