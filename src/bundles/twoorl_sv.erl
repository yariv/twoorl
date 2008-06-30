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

-module(twoorl_sv).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	
	language -> <<"svenska">>;
	
	%% layout
	login -> <<"logga in">>;
	register -> <<"registrera">>;
	logged_in_as -> <<"inloggad som">>;
	settings -> <<"inställningar">>;
	logout -> <<"logga ut">>;
	get_source ->
	    <<"Hämta <a href=\"http://code.google.com/p/twoorl\">"
	     "källkoden</a>">>;
	
	%% navbar
	home -> <<"hem">>;
	replies -> <<"svar">>;
	me -> <<"jag">>;
	everyone -> <<"alla">>;

	%% login page
	login_cap -> <<"Logga in">>;
	username -> <<"användarnamn">>;
	password -> <<"lösenord">>;
	not_member -> <<"Inte medlem?">>;
	login_submit -> <<"logga in">>;

	%% register page
	% note: 'username', 'password' and 'Login_cap' are taken from
	% login page section
	register_cap -> <<"Registrera">>;
	email -> <<"e-post">>;
	password2 -> <<"upprepa lösenord">>;
	already_member -> <<"Redan medlem?">>;

	%% home page
	upto -> <<"Vad pysslar du med?">>;
	twitter_msg -> <<"Automatisk postning till Twitter aktiverat för "
			"icke-svar">>;
	send -> <<"skicka">>;

	%% main page
	public_timeline -> <<"Publik tidslinje">>;

	%% users page
	{no_user, Username} ->
	    [<<"Användaren '">>, Username, <<"' finns inte">>];
	{timeline_of, Username} ->
	    [Username, <<" tidslinje">>];
	following -> <<"följer">>;
	followers -> <<"förföljare">>;
	follow -> <<"följ">>;
	unfollow -> <<"ofölj">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Människor ">>, Userlink, <<" följer">>];
	{followers_of, Userlink} ->
	    [Userlink, <<" förföljare">>];
	{no_friends, Username} ->
	    [Username, <<" följer ingen">>];
	{no_followers, Username} ->
	    [Username, <<" har inga förföljare">>];


	%% settings page
	settings_cap -> <<"Inställningar">>;
	use_gravatar -> <<"Använd <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"Profilbakgrund">>;
	profile_bg_help ->
	    <<"Skriv in url.en för din profilbakgrundsbild "
	     "(lämna blankt för att använda standardbakgrunden):">>;
	twitter_help ->
	    <<"Du kan tillhandahålla dina Twitterkontouppgifter för att få"
	     "dina twoorls automatiskt postade till Twitter.<br/><br/>"
	     "Bara twoorls som inte innehåller svar (ex."
	     "\"@sergey\") kommer att postas till Twitter">>;
	twitter_username -> <<"Twitter användarnamn:">>;
	twitter_password -> <<"Twitter lösenord:">>;
	twitter_auto -> <<"Posta automatiskt mina Twoorls till Twitter?">>;
	submit -> <<"skicka">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"">>, Field, <<" fältet är obligatoriskt">>];
	{username_taken, Val} ->
	    [<<"Användarnamnet '">>, Val, <<"' är taget">>];
	{invalid_username, Val} ->
	    [<<"Användarnamnet '">>, Val,
	     <<"' är ogiltigt. Enbart bokstäver, nummer och understreck ('_') "
	      " är tillåtna">>];
	invalid_login ->
	    <<"Ogiltigt användarnamn eller lösenord">>;
	{too_short, Field, Min} ->
	    [<<"">>, Field, <<" är för kort (">>, Min,
	     <<" tecken minst)">>];
	password_mismatch ->
	    <<"Lösenorden matchade inte">>;
	twitter_unauthorized ->
	    <<"Twitter ratade användarnamn/lösenords kombinationen du "
	     "tillhandahöll">>;
	twitter_authorization_error ->
	    <<"Kund einte koppla upp mot Twitter. Var vänlig försök igen senare.">>;
	{invalid_url, Field} ->
	    [<<"">>, Field, <<" URL:en måste börja med 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Dina inställningar har uppdaterats framgångsrikt">>];

	%% miscellaneous
	{seconds_ago, Val} -> [Val, <<" sekunder sedan">>];
	{minutes_ago, Val} -> [Val, <<" minuter sedan">>];
	{hours_ago, Val} -> [Val, <<" timmar sedan">>];
	{days_ago, Val} -> [Val, <<" dagar sedan">>]
    end.
