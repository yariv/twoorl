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
-module(grid_controller).
-export([private/0, index/2, index/3]).

private() ->
    true.

index(A, Cells) ->
    index(A, Cells, 3).

index(_A, Cells, NumCols) ->
    {_, Rows1} =
	lists:foldl(
	  fun(Cell, {N, [Hd | Tl]}) when N == NumCols ->
		  {1, [[Cell] |
		       [lists:reverse(Hd) | Tl]]};
	     (Cell, {N, [Hd | Tl]}) ->
		  {N+1, [[Cell | Hd] | Tl]}
	  end, {0, [[]]}, Cells),
    lists:reverse(Rows1).
