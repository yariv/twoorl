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

-module(twoorl_ita).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	
	language -> <<"italiano">>;
	
	%% layout
	login -> <<"accedi">>;
	register -> <<"iscriviti">>;
	logged_in_as -> <<"sei entrato come">>;
	settings -> <<"configura">>;
	logout -> <<"esci">>;
	get_source ->
	    <<"Scarica il <a href=\"http://code.google.com/p/twoorl\">"
	     "codice sorgente</a>">>;
	
	%% navbar
	home -> <<"home">>;
	replies -> <<"risposte">>;
	me -> <<"miei">>;
	everyone -> <<"tutti">>;

	%% login page
	login_cap -> <<"Accesso">>;
	username -> <<"utente">>;
	password -> <<"password">>;
	not_member -> <<"Non sei iscritto?">>;
	login_submit -> <<"accedi">>;

	%% register page
	% note: 'username', 'password' and 'Login_cap' are taken from
	% login page section
	register_cap -> <<"Iscriviti">>;
	email -> <<"email">>;
	password2 -> <<"Inserisci di nuovo la password">>;
	already_member -> <<"Sei già iscritto?">>;

	%% home page
	upto -> <<"Che cosa stai facendo ora?">>;
	twitter_msg -> <<"Posta automaticamente su Twitter i "
			"messaggi senza risposta">>;
	send -> <<"invia">>;

	%% main page
	public_timeline -> <<"Cronologia pubblica">>;

	%% users page
	{no_user, Username} ->
	    [<<"L'utente '">>, Username, <<"' non esiste">>];
	{timeline_of, Username} ->
	    [<<"Cronologia di">>, Username];
	following -> <<"segue">>;
	followers -> <<"seguito da">>;
	follow -> <<"segui">>;
	unfollow -> <<"non seguire più">>;

	%% friends page
	{friends_of, Userlink} ->
	    [Userlink, <<" segue">>];
	{followers_of, Userlink} ->
	    [Userlink, <<" è seguito da">>];
	{no_friends, Username} ->
	    [Username, <<" non sta seguendo nessuno">>];
	{no_followers, Username} ->
	    [Username, <<" non è seguito da nessuno">>];


	%% settings page
	settings_cap -> <<"Impostazioni">>;
	use_gravatar -> <<"Vuoi usare <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"Immagine di sfondo del profilo">>;
	profile_bg_help ->
	    <<"Inserisci la url per l'immagine di sfondo del tuo profilo "
	     "(non riempire se vuoi usare lo sfondo di default):">>;
	twitter_help ->
		<<"Inserisci i dati del tuo account Twitter in modo "
		"che i tuoi twoorls vengano postati automaticamente su Twitter.<br/><br/>"
	     "Saranno postati su Twitter solo i twoorls senza "
	     "risposta (ad esempio\"@sergey\").">>;
	twitter_username -> <<"Utente twitter:">>;
	twitter_password -> <<"Password twitter:">>;
	twitter_auto -> <<"Posta automaticamente i miei twoorls su Twitter">>;
	submit -> <<"invia">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"Il campo ">>, Field, <<" è obbligatorio">>];
	{username_taken, Val} ->
	    [<<"Il nome utente '">>, Val, <<"' esiste già">>];
	{invalid_username, Val} ->
	    [<<"Il nome utente '">>, Val,
	     <<"' non è valido. Sono ammessi solo caratteri, numeri e sottolineato ('_')">>];
	invalid_login ->
	    <<"Nome utente o password non validi">>;
	{too_short, Field, Min} ->
	    [<<"Il campo ">>, Field, <<" è troppo corto (">>, Min,
	     <<" caratteri almeno)">>];
	password_mismatch ->
	    <<"Le password non corrispondono">>;
	twitter_unauthorized ->
		<<"Il nome utente/password inseriti non sono stati accettati "
		"da Twitter!">>;
	twitter_authorization_error ->
		<<"Non sono riuscito a contattare Twitter. Per favore "
		" riprova più tardi.">>;
	{invalid_url, Field} ->
	    [<<"Il campo URL ">>, Field, <<" deve iniziare con 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Le nuove impostazioni sono state salvate">>];

	%% miscellaneous
	{seconds_ago, Val} -> [Val, <<" secondi fa">>];
	{minutes_ago, Val} -> [Val, <<" minuti fa">>];
	{hours_ago, Val} -> [Val, <<" ore fa">>];
	{days_ago, Val} -> [Val, <<" giorni fa">>]
    end.
