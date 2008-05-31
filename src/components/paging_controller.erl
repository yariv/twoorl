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

-module(paging_controller).
-compile(export_all).
-include("twoorl.hrl").

private() ->
    true.

index(A, Fun) ->
    index(A, Fun, undefined).

index(A, Fun, ResFun) ->
    index(A, Fun, ResFun, []).


index(A, Fun, ResFun, Opts) ->
    {Page, Params} = get_page(A, true),
	  
    PageSize = case proplists:get_value(page_size, Opts) of
		   undefined ->
		       ?MAX_PAGE_SIZE;
		   Other ->
		       Other
	       end,

    Ewcs = Fun({limit, (Page - 1)*PageSize, PageSize}),
	  
    Len = length(Ewcs),
    Total = proplists:get_value(total, Opts),
    NumPages = if Total == undefined ->
		       if Len < PageSize ->
			       Page;
			  true ->
			       undefined
		       end;
		  true ->
		       ceil(Total/PageSize)
	       end,
    From = (Page-1) * PageSize,
    To = From + Len,
    From1 = if Len > 0 -> From + 1; true -> From end,
    [{data, {Len,
	     if Total =/= undefined ->
		    integer_to_list(Total);
	       true ->
		    Total
	     end,
	     integer_to_list(From1),
	     integer_to_list(To),
	     lists:member(show_stats, Opts),
	     case proplists:get_value(desc, Opts) of
		undefined -> <<"item">>;
		Other3 -> Other3
	     end,
	     get_links(A, NumPages, Page, Params, Opts)}},
      if ResFun == undefined ->
	     Ewcs;
	true ->
	     ResFun(Ewcs)
      end].

get_page(A) ->
    get_page(A, false).
get_page(A, ReturnParams) ->
    Params = if is_tuple(A) ->
		     yaws_api:parse_query(A);
		true ->
		     A
	     end,
    Page = case proplists:get_value("page", Params) of
	       undefined ->
		   1;
	       Val ->
		   case catch list_to_integer(Val) of
		       {'EXIT', _} ->
			   1;
		       Num ->
			   lists:max([1, Num])
		   end
	   end,
    if ReturnParams ->
	    {Page, Params};
       true ->
	    Page
    end.

get_links(A, NumPages, Page, Params, Opts) ->
    Page1 = lists:max([lists:min([Page, NumPages]), 1]),
    Params1 = proplists:delete("page", Params),

    BaseUrl1 = erlyweb_util:get_url_prefix(A),

    BaseUrl2 = 
	[BaseUrl1, "?", lists:map(fun({Key, Val}) -> [Key,$=,Val,$&] end,
				  Params1)],
    MakeLink =
	fun(PageNum) ->
		make_link(BaseUrl2, PageNum,
			  integer_to_list(PageNum))
	end,
    MakeWindow =
	fun(Min, Max, Tail) ->
		Seq = if Max >= Min ->
			      lists:reverse(lists:seq(Min, Max));
			 true ->
			      []
		      end,
		lists:foldl(
		  fun(PageNum, Acc) ->
			  [MakeLink(PageNum) | Acc]
		  end, Tail, Seq)
	end,

    NextLinks = if NumPages == undefined ->
		       [make_link(BaseUrl2, Page1 + 1, <<"next >">>)];
		  Page1 < NumPages ->
		       FirstLinks = 
			   if Page1 + ?PAGING_WINDOW < NumPages - 1 ->
				   [{nolink, <<"...">>}, MakeLink(NumPages)];
			      Page1 + ?PAGING_WINDOW < NumPages ->
				   [MakeLink(NumPages)];
			      true ->
				   []
			   end,
			MakeWindow(
			  Page+1,
			  lists:min([Page1 + ?PAGING_WINDOW,
				     NumPages]),
			  FirstLinks);
		  true ->
		       []
	       end,
    NextLinks1 = [{nolink, integer_to_list(Page1)} | NextLinks],
    AllLinks =
	begin
	    Links2 =
		MakeWindow(
		  lists:max([1, Page1 - ?PAGING_WINDOW]), Page1 - 1,
		  NextLinks1),
	    if Page1 - ?PAGING_WINDOW > 2 ->
		    [MakeLink(1), {nolink, <<"...">>} | Links2];
	       Page1 - ?PAGING_WINDOW > 1 ->
		    [MakeLink(1) | Links2];
	       true ->
		    Links2
	    end
	end,
    AllLinks.

make_link(BaseUrl, Page, Text) ->
    {link, [[BaseUrl, [<<"page=">>, integer_to_list(Page)]]],
     Text}.

limit(Page, PageSize) ->
    {limit, (Page - 1)*PageSize, PageSize}.


ceil(Num) ->
    R = round(Num),
    if R < Num ->
	    R + 1;
       true ->
	    R
    end.
