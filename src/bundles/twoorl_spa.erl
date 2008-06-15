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

-module(twoorl_spa).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	%% layout
	login -> <<"acceder">>;
	register -> <<"crear una cuenta">>;
	logged_in_as -> <<"conectado como">>;
	settings -> <<"configuración">>;
	logout -> <<"salir">>;
	get_source ->
	    <<"Obtener el <a href=\"http://code.google.com/p/twoorl\">"
	     "código fuente</a>">>;

	%% navbar
	home -> <<"inicio">>;
	replies -> <<"respuestas">>;
	me -> <<"yo">>;
	everyone -> <<"todos">>;

	%% login page
	login_cap -> <<"Acceder">>;
	username -> <<"nombre de usuario">>;
	password -> <<"contraseña">>;
	not_member -> <<"No dispone de una cuenta?">>;
	login_submit -> <<"acceder">>;

	%% register page
	register_cap -> <<"Crear una cuenta">>;
	email -> <<"dirección de correo electrónico">>;
	password2 -> <<"vuelve a introducir la contraseña">>;
	already_member -> <<"Ya dispone de una cuenta?">>;

	%% home page
	upto -> <<"Qué estás haciendo?">>;
	twitter_msg -> <<"Envío automático a Twitter activado para "
			"mensajes sin respuestas">>;

	%% main page
	public_timeline -> <<"Cronograma público">>;

	%% users page
	{no_user, Username} ->
	    [<<"El usuario '">>, Username, <<"' no existe">>];
	{timeline_of, Username} ->
	    [<<"Cronograma de">>, Username];
	following -> <<"siguiendo">>;
	followers -> <<"seguidores">>;
	follow -> <<"seguir">>;
	unfollow -> <<"no seguir más">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Gente que ">>, Userlink, <<" sigue">>];
	{followers_of, Userlink} ->
	    [<<"Seguidores de">>, Userlink];
	{no_friends, Username} ->
	    [Username, <<" no sigue a nadie">>];
	{no_followers, Username} ->
	    [Username, <<" no es seguido por nadie">>];


	%% settings page
	settings_cap -> <<"Configuración">>; %% jdilelle says: can also be "Parámetros".
	use_gravatar -> <<"Usar un <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"Fondo de pantalla de tu perfil">>;
	profile_bg_help ->
	    <<"Proporcione el URL de la imágen de fondo de pantalla "
	     "(dejar vacío para usar el fondo de pantalla por defecto):">>;
	twitter_help ->
	    <<"Puedes proporcionar los detalles de tu cuenta Twitter para que tus "
	     "twoorls sean enviados automáticamente a Twitter.<br/><br/>"
	     "Sólo los twoorls sin respuestas (por ejemplo"
	     "\"@sergey\") serán enviados a Twitter.">>;
	twitter_username -> <<"Nombre de usuario Twitter:">>;
	twitter_password -> <<"Contraseña Twitter:">>;
	twitter_auto -> <<"Enviar automáticamente mis Twoorls a Twitter?">>; %% jdilelle says: 'Twools' vs 'twools' ?
	submit -> <<"enviar">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"El campo ">>, Field, <<" es requerido">>];
	{username, Val} ->
	    [<<"En nombre de usuario '">>, Val, <<"' no está disponible">>];
	{invalid_username, Val} ->
	    [<<"El nombre de usuatio '">>, Val,
	     <<"' no es válido. Se aceptan solo letras, números y rayita de subrayado ('_') ">>];
	invalid_login ->
	    <<"Nombre de usuario o contraseña inválidas">>;
	{too_short, Field, Min} ->
	    [<<"El campo ">>, Field, <<" es demasiado corto (">>, Min,
	     <<" chars minimum)">>];
	password_mismatch ->
	    <<"Las contraseñas no coinciden">>;
	twitter_unauthorized ->
	    <<"Twitter no ha aceptado el nombre de usuario y contraseña que "
	     "has proporcionado">>;
	twitter_authorization_error ->
	    <<"Fallo conectandose a Twitter. Por favor vuelva a intentar más tarde.">>;
	{invalid_url, Field} ->
	    [<<"El campo URL ">>, Field, <<" debe comenzar con 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Su configuración ha sido aplicada">>]
    end.
