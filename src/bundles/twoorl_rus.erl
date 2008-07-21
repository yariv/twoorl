-module(twoorl_rus).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	%% layout
	login -> <<"вход">>;
	register -> <<"регистрация">>;
	logged_in_as -> <<"ваше имя">>; 
	settings -> <<"настройки">>;
	logout -> <<"выход">>;
	get_source ->
	    <<"Получить <a href=\"http://code.google.com/p/twoorl\">"
	     "исходный код</a>">>;

	%% navbar
	home -> <<"домой">>;
	replies -> <<"ответ">>;
	me -> <<"я">>;
	everyone -> <<"все">>;

	%% login page
	login_cap -> <<"Вход">>;
	username -> <<"имя">>;
	password -> <<"пароль">>;
	not_member -> <<"Не зарегистрированы?">>;
	login_submit -> <<"Войти">>;

	%% register page
	register_cap -> <<"Регистрация">>;
	username -> <<"имя">>;
	email -> <<"email">>;
	password -> <<"пароль">>;
	password2 -> <<"пароль (подтверждение)">>;
	already_member -> <<"Уже зарегистрировались?">>;
	login_cap -> <<"Вход">>;

	%% home page
	upto -> <<"Чем занимаешься?">>;
	twitter_msg -> <<"Автоматическое копирование на Twitter работает для обычных сообщений,"
			" не ответов">>;

	%% main page
	public_timeline -> <<"Общая лента">>;

	%% users page
	{no_user, Username} ->
	    [<<"Пользователь '">>, Username, <<" не существует">>];
	{timeline_of, Username} ->
	    [<<"Лента ">>, Username];
	following -> <<"друзей">>;
	followers -> <<"наблюдающих">>;
	follow -> <<"следить">>;
	unfollow -> <<"не следить">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"Друзья ">>, Userlink];
	{followers_of, Userlink} ->
	    [<<"Наблюдатели ">>, Userlink];
	{no_friends, Username} ->
	    [Username, <<" не следит ни за кем">>];
	{no_followers, Username} ->
	    [<<"У ">>, Username, <<" нет наблюдателей">>];


	%% settings page
	settings_cap -> <<"Настройки">>;
	use_gravatar -> <<"Использовать <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"Фоновое изображение">>;
	profile_bg_help ->
	    <<"Введите адрес фонового изображения"
	     "(оставьте пустым для изображения по-умолчанию):">>;
	twitter_help ->
	    <<"Вы можете предоставить информацию о учетной записи в Twitter, чтобы"
	     "ваши посты автоматически копировались в Twitter.<br/><br/>"
	     "Только посты не содержащие ответы (например "
	     "\"@edbond\") будут отправлены на Twitter.">>;
	twitter_username -> <<"Twitter имя:">>;
	twitter_password -> <<"Twitter пароль:">>;
	twitter_auto -> <<"Автоматически постить на Twitter?">>;
	submit -> <<"отправить">>;
	
	%% error messages
	{missing_field, Field} ->
	    [Field, <<" обязательное поле">>];
	{username, Val} ->
	    [<<"Имя '">>, Val, <<"' уже занято">>];
	{invalid_username, Val} ->
	    [<<"Имя '">>, Val,
	     <<"' некорректно. Используйте только буквы, цифры и подчеркивание ('_') ">>];
	invalid_login ->
	    <<"Неправильное имя или пароль">>;
	{too_short, Field, Min} ->
	    [<<"Поле ">>, Field, <<" очень короткое (">>, Min,
	     <<" букв минимум)">>];
	password_mismatch ->
	    <<"Пароли не совпадают">>;
	twitter_unauthorized ->
	    <<"Twitter отказал в доступе по имени и паролю, который вы указали">>;
	twitter_authorization_error ->
	    <<"Не могу соединиться с Twitter. Пожалуйста, попробуйте позже.">>;
	{invalid_url, Field} ->
	    [<<"Ссылка ">>, Field, <<" должна начинаться с 'http://'">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"Ваши настройки были успешно сохранены">>]
    end.
