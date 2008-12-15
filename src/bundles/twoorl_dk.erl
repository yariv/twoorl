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

-module(twoorl_dk).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	
	language -> <<"dansk">>;
	
	%% layout
	login -> <<"log ind">>;
	register -> <<"ny bruger">>;
	logged_in_as -> <<"logget ind som">>;
	settings -> <<"indstillinger">>;
	logout -> <<"log ud">>;
	get_source ->
	    <<"Få <a href=\"http://code.google.com/p/twoorl\">"
	     "kildekoden</a>">>;
	
	%% navbar
	home -> <<"hjem">>;
	replies -> <<"svarer">>;
	me -> <<"jeg">>;
	everyone -> <<"alle">>;

	%% login page
	login_cap -> <<"Log ind">>;
	username -> <<"brugernavn">>;
	password -> <<"adgangskode">>;
	not_member -> <<"Ikke medlem?">>;
	login_submit -> <<"log ind">>;

	%% register page
	% note: 'username', 'password' and 'Login_cap' are taken from
	% login page section
	register_cap -> <<"Ny bruger">>;
	email -> <<"email">>;
	password2 -> <<"gentag adgangskode">>;
	already_member -> <<"Allerede medlem?">>;

	%% home page
	upto -> <<"Hvad sidder du og gør?">>;
	twitter_msg -> <<"Automatisk sending til Twitter aktiv for "
			"ikke-svarer">>;
	send -> <<"send">>;

	%% main page
	public_timeline -> <<"Offentlig tidlinie">>;

	%% users page
	{no_user, Username} ->
	    [<<"Brugeren '">>, Username, <<"' findes ikke">>];
	{timeline_of, Username} ->
	    [Username, <<"s tidlinie">>];
	following -> <<"følgende">>;
	followers -> <<"følgendsvender">>;
	follow -> <<"følge">>;
	unfollow -> <<"følge ikke">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Folk ">>, Userlink, <<" følges">>];
	{followers_of, Userlink} ->
	    [Userlink, <<"s følgendsvender">>];
	{no_friends, Username} ->
	    [Username, <<" følges ingen">>];
	{no_followers, Username} ->
	    [Username, <<" har ingen følgendsvender">>];


	%% settings page
	settings_cap -> <<"Indstillinger">>;
	use_gravatar -> <<"Brug <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"Profil baggrund">>;
	profile_bg_help ->
	    <<"Indføre urlen for dit profilbaggrundbilede"
	     "(led tom efter for at bruge normal baggrunden):">>;
	twitter_help ->
	    <<"Du kan give dine twitterkonto-detaljer for at have dine twoorls sendt automatisk til Twitter.<br/><br/>"
	     "Kun disse twoorls uden svarer (e.g."
	     "\"@sergey\") vil blive sendt til Twitter.">>;
	twitter_username -> <<"Twitter brugernavn:">>;
	twitter_password -> <<"Twitter adgangskode:">>;
	twitter_auto -> <<"Sende mine Twoorls til Twitter automatisk?">>;
	submit -> <<"send frem">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"Felden ">>, Field, <<" er obligatorisk">>];
	{username_taken, Val} ->
	    [<<"Brugeren '">>, Val, <<"' er allerede besat">>];
	{invalid_username, Val} ->
	    [<<"Brugeren '">>, Val,
	     <<"' gælder ikke. Accepteres kun bogstaver, ciffer og understreger ('_') ">>];
	invalid_login ->
	    <<"Ukendt brugernavn eller adgangskode">>;
	{too_short, Field, Min} ->
	    [<<"Felden ">>, Field, <<" er for kort (">>, Min,
	     <<" bogstaver som minimum)">>];
	password_mismatch ->
	    <<"Den valgte adgangskode stemmer ikke overens med gentagelsen">>;
	twitter_unauthorized ->
	    <<"Twitter afslog din brugernavn/adgangskode kombination">>;
	twitter_authorization_error ->
	    <<"Kunne forbinde til Twitter ikke. Prøv igen senere.">>;
	{invalid_url, Field} ->
	    [<<"Felden ">>, Field, <<" URL skal begynde med 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Dine indstillinger blev opdateret">>];

	%% miscellaneous
	{seconds_ago, Val} -> [Val, <<" sekunder siden">>];
	{minutes_ago, Val} -> [Val, <<" minutter siden">>];
	{hours_ago, Val} -> [Val, <<" timer siden">>];
	{days_ago, Val} -> [Val, <<" dage siden">>]
    end.

