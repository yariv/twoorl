-module(twoorl_eng).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	%% layout
	login -> <<"conectar-se">>;
	register -> <<"registrar-se">>;
	logged_in_as -> <<"conectado como">>;
	settings -> <<"configurações">>;
	logout -> <<"sair">>;
	get_source ->
	    <<"Obtenha o <a href=\"http://code.google.com/p/twoorl\">"
	     "código fonte</a>">>;

	%% navbar
	home -> <<"home">>;
	replies -> <<"respostas">>;
	me -> <<"eu">>;
	everyone -> <<"todo mundo">>;

	%% login page
	login_cap -> <<"Login">>;
	username -> <<"nome de usuário">>;
	password -> <<"senha">>;
	not_member -> <<"Ainda não cadastrado?">>;
	login_submit -> <<"login">>;

	%% register page
	register_cap -> <<"Criar sua conta">>;
	username -> <<"nome de usuário">>;
	email -> <<"email">>;
	password -> <<"senha">>;
	password2 -> <<"repita a senha">>;
	already_member -> <<"Já possui sua conta?">>;
	login_cap -> <<"Login">>;

	%% home page
	upto -> <<"O que você está fazendo agora?">>;
	twitter_msg -> <<"Postagem automática no Twitter habilitada para "
			"mensagens que não sejam respostas">>;

	%% main page
	public_timeline -> <<"Linha do tempo pública">>;

	%% users page
	{no_user, Username} ->
	    [<<"O usuário '">>, Username, <<" não existe">>];
	{timeline_of, Username} ->
	    [<<"Linha do tempo de ">>, Username];
	following -> <<"está seguindo">>;
	followers -> <<"seus seguidores">>;
	follow -> <<"seguir">>;
	unfollow -> <<"deixar de seguir">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Pessoas que ">>, Userlink, <<" segue">>];
	{followers_of, Userlink} ->
	    [<<"Seguidore de ">>, Userlink];
	{no_friends, Username} ->
	    [Username, <<" ainda não está seguindo ninguem">>];
	{no_followers, Username} ->
	    [Username, <<" não tem seguidores">>];


	%% settings page
	settings_cap -> <<"Configurações">>;
	use_gravatar -> <<"Use <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"Imagem de fundo do seu perfil">>;
	profile_bg_help ->
	    <<"Informe a url para a imagem de fundo do seu perfil "
	     "(você pode deixar este campo em branco para usar o fundo default):">>;
	twitter_help ->
	    <<"Você também pode informar os detalhes de sua conta no twitter "
	     "para que seus twoorls sejam automaticamente postados no Twitter.<br/><br/>"
	     "Apenas os twoorls que não tenham respostas (e.g."
	     "\"@sergey\") serão postados no Twitter.">>;
	twitter_username -> <<"Nome de usuário no Twitter:">>;
	twitter_password -> <<"Senha do twitter:">>;
	twitter_auto -> <<"Postar meus Twoorls automaticamente no Twitter?">>;
	submit -> <<"submeter alterações">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"O ">>, Field, <<" campo é obrigatório">>];
	{username, Val} ->
	    [<<"O nome de usuário '">>, Val, <<"' já está sendo utilizado">>];
	{invalid_username, Val} ->
	    [<<"O nome de usuário '">>, Val,
	     <<"' é inválido. Apenas letras, números e underscores ('_') "
	      "são permitidos">>];
	invalid_login ->
	    <<"Nome de usuário ou senha inválidos">>;
	{too_short, Field, Min} ->
	    [<<"O campo ">>, Field, <<" é muito curto (">>, Min,
	     <<" caracteres é o mínimo)">>];
	password_mismatch ->
	    <<"Os campos de senhas não são iguais.">>;
	twitter_unauthorized ->
	    <<"O Twitter rejeitou a combinação de usuário e senha que você  "
	     "informou">>;
	twitter_authorization_error ->
	    <<"Não foi possível conectar-se ao Twitter. Por favor tente novamente mais tarde.">>;
	{invalid_url, Field} ->
	    [<<"O campo ">>, Field, <<"  precisa começar com 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Suas configurações foram atualizadas com sucesso">>]
    end.
