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

-module(twoorl_deu).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	%% layout
	login -> <<"anmelden">>;
	register -> <<"registrieren">>;
	logged_in_as -> <<"angemeldet als">>;
	settings -> <<"Einstellungen">>;
	logout -> <<"abmelden">>;
	get_source ->
	    <<"Hol dir den <a href=\"http://code.google.com/p/twoorl\">"
	     "Quelltext</a>">>;

	%% navbar
	home -> <<"homepage">>;
	replies -> <<"antworten">>;
	me -> <<"ich">>;
	everyone -> <<"alle">>;

	%% login page
	login_cap -> <<"Anmeldung">>;
	username -> <<"Benutzername">>;
	password -> <<"Passwort">>;
	not_member -> <<"Noch kein Mitglied?">>;
	login_submit -> <<"anmelden">>;

	%% register page
	% note: 'username', 'password' and 'Login_cap' are taken from
	% login page section
	register_cap -> <<"Registrierung">>;
	email -> <<"Email">>;
	password2 -> <<"Passwort erneut eingeben">>;
	already_member -> <<"Bereits registriert?">>;

	%% home page
	upto -> <<"Was machst du gerade?">>;
	twitter_msg -> <<"Automatisches Posten bei Twitter angestellt für "
			"nicht-Antworten">>;

	%% main page
	public_timeline -> <<"öffentliche Zeitleiste">>;

	%% users page
	{no_user, Username} ->
	    [<<"Der Benutzer '">>, Username, <<"' existiert nicht">>];
	{timeline_of, Username} ->
	    [Username, <<"s Zeitleiste">>];
	following -> <<"verfolgt">>;
	followers -> <<"Fans">>;
	follow -> <<"verfolgen">>;
	unfollow -> <<"nicht mehr verfolgen">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Leute die ">>, Userlink, <<" verfolgt">>];
	{followers_of, Userlink} ->
	    [Userlink, <<"s Fans">>];
	{no_friends, Username} ->
	    [Username, <<" folgt Niemandem">>];
	{no_followers, Username} ->
	    [Username, <<" hat keine Fans">>];


	%% settings page
	settings_cap -> <<"Einstellungen">>;
	use_gravatar -> <<"<a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a> benutzen?">>;
	profile_bg -> <<"Profilhintergrundbild">>;
	profile_bg_help ->
	    <<"Bitte gebe die Url für dein Profilhintergrundbild ein "
	     "(Url freilassen um das voreingestellte Bild zu verwenden):">>;
	twitter_help ->
	    <<"Hier kannst du deine Twitter-Profileinstellungen eingeben "
	     "damit deine Twoorls automatisch auf Twitter gepostet werden.<br/><br/>"
	     "Nur twoorls die keine Antworten enthalten (z.B."
	     "\"@sergey\") werden auf Twitter gepostet.">>;
	twitter_username -> <<"Twitter Benutzername:">>;
	twitter_password -> <<"Twitter Passwort:">>;
	twitter_auto -> <<"Meine Twoorls automatisch auf Twitter posten?">>;
	submit -> <<"Abschicken">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"Das Feld ">>, Field, <<" ist erforderlich">>];
	{username_taken, Val} ->
	    [<<"Der Benutzername '">>, Val, <<"' ist bereits vergeben">>];
	{invalid_username, Val} ->
	    [<<"Der Benutzername '">>, Val,
	     <<"' ist ungültig. Bitte nur Buchstaben, Zahlen und den Unterstrich ('_') "
	      "verwenden">>];
	invalid_login ->
	    <<"Ungültiger Benutzername or Passwort">>;
	{too_short, Field, Min} ->
	    [<<"Das Feld ">>, Field, <<" ist zu kurz (mindestens ">>, Min,
	     <<" Zeichen)">>];
	password_mismatch ->
	    <<"Die Passwörter passen nicht zusammen">>;
	twitter_unauthorized ->
	    <<"Twitter hat die Kombination Benutzername/Passwort die du "
	     "eingegeben hast abgelehnt">>;
	twitter_authorization_error ->
	    <<"Konnte mich nicht mit Twitter verbinden. Bitte versuche es nochmal später.">>;
	{invalid_url, Field} ->
	    [<<"Das Feld ">>, Field, <<" URL muss mit 'http://' anfangen">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Ihre Einstellungen wurden erfolgreich aktualisiert">>]
    end.
