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

-module(twoorl_fra).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	%% layout
	login -> <<"Se connecter">>;
	register -> <<"créer mon compte Twoorl">>;
	logged_in_as -> <<"connecté comme">>;
	settings -> <<"configuration">>;
	logout -> <<"quitter">>;
	get_source ->
	    <<"Obtenir le <a href=\"http://code.google.com/p/twoorl\">"
	     "code source</a>">>;

	%% navbar
	home -> <<"accueuil">>;
	replies -> <<"réponses">>;
	me -> <<"moi">>;
	everyone -> <<"tous">>;

	%% login page
	login_cap -> <<"Se connecter">>;
	username -> <<"nom d'utilisateur">>;
	password -> <<"mot de passe">>;
	not_member -> <<"Vous n'avez pas encore votre compte Twoorl?">>;
	login_submit -> <<"se connecter">>;

	%% register page
	register_cap -> <<"Créer mon compte Twoorl">>;
	email -> <<"adresse électronique">>;
	password2 -> <<"veuillez confirmer votre mot de passe">>;
	already_member -> <<"Vous avez déjà un compte Twoorl?">>;

	%% home page
	upto -> <<"Que faîtes-vous?">>;
	twitter_msg -> <<"L'envoi automatique à Twitter des messages "
			"sans réponses est activé">>;

	%% main page
	public_timeline -> <<"Chronologie publique">>;

	%% users page
	{no_user, Username} ->
	    [<<"Le nom d'utilisateur '">>, Username, <<"' n'existe pas">>];
	{timeline_of, Username} ->
	    [<<"Chronologie de ">>, Username];
	following -> <<"suivis">>;
	followers -> <<"suiveurs">>;
	follow -> <<"suivre">>;
	unfollow -> <<"ne plus suivre">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Gens suivis par ">>, Userlink];
	{followers_of, Userlink} ->
	    [<<"Suiveurs de ">>, Userlink];
	{no_friends, Username} ->
	    [Username, <<" ne suit personne">>];
	{no_followers, Username} ->
	    [Username, <<" n'est suivi par personne">>];


	%% settings page
	settings_cap -> <<"Configuration">>;
	use_gravatar -> <<"Utiliser un <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"Fond d'écran associé à mon profil">>;
	profile_bg_help ->
	    <<"Veuillez fournir l'URL de l'image à utiliser comme fond d'écran "
	     "(vous pouvez laisser ce champ vide pour utiliser l'image par défaut):">>;
	twitter_help ->
	    <<"Vous pouvez fournir votre information de connexion à Twitter pour que vos "
	     "Twoorls soient automatiquement envoyés à Twitter.<br/><br/>"
	     "Seuls les Twoorls sans réponses (par exemple, sans "
	     "\"@sergey\") seront envoyés à Twitter.">>;
	twitter_username -> <<"Nom d'utilisateur Twitter:">>;
	twitter_password -> <<"Mot de passe Twitter:">>;
	twitter_auto -> <<"Voulez-vous automatiquement envoyer vos Twoorls à Twitter?">>;
	submit -> <<"envoyer">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"Le champ ">>, Field, <<" est requis.">>];
	{username_taken, Val} ->
	    [<<"Le nom d'utilisateur '">>, Val, <<"' n'est pas disponible.">>];
	{invalid_username, Val} ->
	    [<<"Le nom d'utilisateur '">>, Val,
	     <<"' n'est pas valide. Seulement les lettres, numéros et le symbole de souligné ('_') sont acceptés.">>];
	invalid_login ->
	    <<"Nom d'utilisateur ou mot de passe invalides.">>;
	{too_short, Field, Min} ->
	    [<<"Le champ ">>, Field, <<" est trop court (">>, Min,
	     <<" charactères minimum).">>];
	password_mismatch ->
	    <<"Les mots de passe ne coïncident pas.">>;
	twitter_unauthorized ->
	    <<"Twitter n'a pas accepté le nom d'usager ou le mot de passe fournis.">>;
	twitter_authorization_error ->
	    <<"Échec lors de la connexion à Twitter. Veuillez essayer plus tard.">>;
	{invalid_url, Field} ->
	    [<<"Le champ URL ">>, Field, <<" doit commencer par 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Votre configuration a été mise à jour.">>]
    end.

