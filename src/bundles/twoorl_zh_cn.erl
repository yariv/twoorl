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

-module(twoorl_zh_cn).
-export([bundle/1]).

bundle(Tag) ->
    case Tag of
	
	language -> <<"简体中文">>;
	
	%% layout
	login -> <<"登录">>;
	register -> <<"注册">>;
	logged_in_as -> <<"登录为">>;
	settings -> <<"设置">>;
	logout -> <<"登出">>;
	get_source ->
	    <<"获取 <a href=\"http://code.google.com/p/twoorl\">"
	     "源代码</a>">>;
	
	%% navbar
	home -> <<"首页">>;
	replies -> <<"回复">>;
	me -> <<"我">>;
	everyone -> <<"所有人">>;

	%% login page
	login_cap -> <<"登录">>;
	username -> <<"用户名">>;
	password -> <<"密码">>;
	not_member -> <<"不是注册用户?">>;
	login_submit -> <<"登录">>;

	%% register page
	% note: 'username', 'password' and 'Login_cap' are taken from
	% login page section
	register_cap -> <<"注册">>;
	email -> <<"邮箱">>;
	password2 -> <<"再次输入密码">>;
	already_member -> <<"已经注册?">>;

	%% home page
	upto -> <<"我在做什么?">>;
	twitter_msg -> <<"自动发表到twitter并启用非回复">>;
	send -> <<"发表">>;

	%% main page
	public_timeline -> <<"公共时间表">>;

	%% users page
	{no_user, Username} ->
	    [<<"用户名 '">>, Username, <<"' 不存在">>];
	{timeline_of, Username} ->
	    [Username, <<"的时间表">>];
	following -> <<"我关注的人">>;
	followers -> <<"关注我的人">>;
	follow -> <<"关注">>;
	unfollow -> <<"取消关注">>;

	%% friends page
	{friends_of, Userlink} ->
	    [<<"用户 ">>, Userlink, <<" 关注的人">>];
	{followers_of, Userlink} ->
	    [<<"关注 ">>, Userlink, <<" 的人">>];
	{no_friends, Username} ->
	    [Username, <<" 没有关注任何人">>];
	{no_followers, Username} ->
	    [Username, <<" 没有被任何人关注">>];


	%% settings page
	settings_cap -> <<"配置">>;
	use_gravatar -> <<"使用 <a href=\"http://gravatar.com\" "
			 "target=\"_blank\">Gravatar</a>?">>;
	profile_bg -> <<"介绍的背景图">>;
	profile_bg_help ->
	    <<"请输入您的介绍页面背景图的网址 "
	     "(留空时使用默认背景图):">>;
	twitter_help ->
	    <<"您可以提供您的twitter帐户的详细资料，"
	     "让您的消息自动发表到twitter。<br/><br/>"
	     "只有不为回复的消息 (例如 "
	     "\"@sergey\") 才会发表到 Twitter.">>;
	twitter_username -> <<"Twitter 用户名:">>;
	twitter_password -> <<"Twitter 密码:">>;
	twitter_auto -> <<"自动发表我的消息到Twitter?">>;
	submit -> <<"提交">>;
	
	%% error messages
	{missing_field, Field} ->
	    [<<"请输入 ">>, Field, <<" 的内容">>];
	{username_taken, Val} ->
	    [<<"帐号 '">>, Val, <<"' 已经被使用">>];
	{invalid_username, Val} ->
	    [<<"帐号 '">>, Val,
	     <<"' 不合法。只能输入英文字母、数字和下划线 ('_') ">>];
	invalid_login ->
	    <<"错误的用户名或密码">>;
	{too_short, Field, Min} ->
	    [<<"输入框 ">>, Field, <<" 内容太短 (至少 ">>, Min,
	     <<" 个英文字母)">>];
	password_mismatch ->
	    <<"两次输入的密码不匹配">>;
	twitter_unauthorized ->
	    <<"您输入的Twitter帐号和密码组合错误">>;
	twitter_authorization_error ->
	    <<"连接不上Twitter，请重试">>;
	{invalid_url, Field} ->
	    [<<"网址 ">>, Field, <<" 必须以 'http://' 开头">>];
	
	%% confirmation messages
	settings_updated ->
	    [<<"您的设置已经成功更新了">>];

	%% miscellaneous
	{seconds_ago, Val} -> [Val, <<" 秒之前">>];
	{minutes_ago, Val} -> [Val, <<" 分钟之前">>];
	{hours_ago, Val} -> [Val, <<" 小时之前">>];
	{days_ago, Val} -> [Val, <<" 天之前">>]
    end.
