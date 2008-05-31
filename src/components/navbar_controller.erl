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
%% Copyright Yariv Sadan, 2008
%%
%% @author Yariv Sadan <yarivsblog@gmail.com> [http://yarivsblog.com]
%% @copyright Yariv Sadan, 2008

-module(navbar_controller).
-compile(export_all).
-include("twoorl.hrl").

private() ->
    true.

index(A) ->
    Appmod = tl(yaws_arg:appmoddata(A)),
    Usr = twoorl_util:get_usr(A),
    Username = Usr:username(),
    Tabs = 
	[{"home", <<"home">>},
	 {"replies", <<"replies">>},
	 {"users/" ++ binary_to_list(Username), <<"me">>},
	 {"main", <<"everyone">>}],
    Links = 
	lists:map(
	  fun({Tab, Title}) when Tab == Appmod ->
		  Title;
	     ({Tab, Title}) ->
		  erlyweb_html:a(["", Tab], Title)
	  end, Tabs),
    {data, Links}.
	      
