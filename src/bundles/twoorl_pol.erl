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
 
-module(twoorl_pol).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	%% layout
	login -> <<"zaloguj">>;
	register -> <<"zajrejstruj">>;
	logged_in_as -> <<"zalogowany jako">>;
	settings -> <<"ustawienia">>;
	logout -> <<"wyloguj">>;
	get_source ->
	    <<"Pobierz <a href=\"http://code.google.com/p/twoorl\">"
	     "kod źródłowy</a>">>;

	%% navbar
	home -> <<"początek">>;
	replies -> <<"odpowiedzi">>;
	me -> <<"ja">>;
	everyone -> <<"wszyscy">>;

	%% login page
	login_cap -> <<"Login">>;
	username -> <<"nazwa użytkownika">>;
	password -> <<"hasło">>;
	not_member -> <<"Nie jesteś członkiem?">>;
	login_submit -> <<"zaloguj">>;

	%% register page
	register_cap -> <<"Zarejstruj">>;
	username -> <<"nazwa użytkownika">>;
	email -> <<"adres poczty elektronicznej">>;
	password -> <<"hasło">>;
	password2 -> <<"powtórz hasło">>;
	already_member -> <<"Jesteś już członkiem?">>;
%%	login_cap -> <<"Zaloguj">>;

	%% home page
	upto -> <<"Co teraz robisz?">>;
	twitter_msg -> <<"Automatyczne wysyłanie do Twittera włączone dla "
			"'nie-odpowiedzi'">>;

	%% main page
	public_timeline -> <<"Publiczna historia">>;

	%% users page
	{no_user, Username} ->
	    [<<"Użytkownik '">>, Username, <<"' nie istnieje">>];
	{timeline_of, Username} ->
	    [<<"Historia użytkownika ">>, Username];
	following -> <<"śledzi">>;
	followers -> <<"śledzący">>;
	follow -> <<"śledź">>;
	unfollow -> <<"przestań śledzić">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Użytkownicy których ">>, Userlink, <<" śledzi">>];
	{followers_of, Userlink} ->
	    [<<"Śledzący użytkownika ">>, Userlink];
	{no_friends, Username} ->
	    [Username, <<" nie śledzi nikogo">>];
	{no_followers, Username} ->
	    [<<"Nikt ie śledzi ">>, Username];


	%% settings page
	settings_cap -> <<"Ustawienia">>;
	use_gravatar -> <<"Użyć <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatara</a>?">>;
	profile_bg -> <<"Tło profilu">>;
	profile_bg_help ->
	    <<"Wprowadź URL dla obrazu tła Twojego profilu "
	     "(zostaw puste dla domyślnego tła):">>;
	twitter_help ->
	    <<"Możesz podać szczegóły Twojego konta Twitter aby automatycznie "
	     "wysyłać również do Twittera.<br/><br/>"
	     "Jedynie twoorle które nie zawierają odpowiedzi (np."
	     "\"@tomek\") będą wysyłane do Twittera.">>;
	twitter_username -> <<"Nazwa użytkownika Twitter:">>;
	twitter_password -> <<"Hasło Twitter:">>;
	twitter_auto -> <<"Automatycznie wysyłaj moje Twoorle do Twittera?">>;
	submit -> <<"wyślij">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"Pole ">>, Field, <<" jest wymagane">>];
	{username, Val} ->
	    [<<"Nazwa użytkownika '">>, Val, <<"' jest zajęta">>];
	{invalid_username, Val} ->
	    [<<"Nazwa użytkownika '">>, Val,
	     <<"' jest nieprawidłowa. Jedynie litery, cyfry oraz znak podkreślenia ('_') "
	      "są dozwolone">>];
	invalid_login ->
	    <<"Niepoprawna nazwa użytkownika lub hasło">>;
	{too_short, Field, Min} ->
	    [<<"Pole ">>, Field, <<" jest zbyt krótkie (minimum ">>, Min,
	     <<" znaków)">>];
	password_mismatch ->
	    <<"Podane hasła nie są identyczne">>;
	twitter_unauthorized ->
	    <<"Twitter odrzucił kombinację nazwy użytkownika oraz hasła "
	     "które podałeś/podałaś">>;
	twitter_authorization_error ->
	    <<"Nie mogę połączyć się z Twitter. Spróbuj ponownie później.">>;
	{invalid_url, Field} ->
	    [<<"Pole ">>, Field, <<" z URL musi zaczynać się od 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Twoje ustawienia zostały poprawnie zaktualizowane">>]
    end.
