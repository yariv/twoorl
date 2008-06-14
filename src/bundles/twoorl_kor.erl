-module(twoorl_kor).
-export([bundle/1]).

bundle(Tag) ->
   case Tag of
     %% layout
     login -> <<"로그인">>;
     register -> <<"회원가입">>;
     logged_in_as -> <<"logged in as">>;
     settings -> <<"settings">>;
     logout -> <<"로그 오프">>;
     get_source ->
         <<"<a href=\"http://code.google.com/p/twoorl\">소스</a> 잡으세요">>;
    
     %% navbar
     home -> <<"홈">>;
     replies -> <<"답장">>;
     me -> <<"나">>;
     everyone -> <<"사람들 모두">>;
    
     %% login page
     login_cap -> <<"로그인">>;
     username -> <<"아이디">>;
     password -> <<"비밀번호">>;
     not_member -> <<"회원 아니세요?">>;
     login_submit -> <<"로그인">>;
    
     %% register page
     register_cap -> <<"회원가입">>;
     username -> <<"아이디">>;
     email -> <<"이메일">>;
     password -> <<"비밀번호">>;
     password2 -> <<"비밀번호 다시 입력해보세요">>;
     already_member -> <<"벌써 회원 이세요?">>;
     login_cap -> <<"로그인">>;
    
     %% home page
     upto -> <<"뭘 하세요?">>;
     twitter_msg -> <<"Automatic posting to Twitter enabled for "
         "non-replies">>;
    
     %% main page
     public_timeline -> <<"Public timeline">>;
    
     %% users page
     {no_user, Username} ->
         [Username,<<" 아이디를 못 찾았습니다. '">>];
     {timeline_of, Username} ->
         [Username, <<"'s timeline">>];
     following -> <<"following">>;
     followers -> <<"followers">>;
     follow -> <<"follow">>;
     unfollow -> <<"unfollow">>;
    
     %% friends page
     {friends_of, Userlink} ->
         [<<"People ">>, Userlink, <<" follows">>];
     {followers_of, Userlink} ->
         [Userlink, <<"'s followers">>];
     {no_friends, Username} ->
         [Username, <<" isn't following anyone">>];
     {no_followers, Username} ->
         [Username, <<" has no followers">>];
    
    
     %% settings page
     settings_cap -> <<"Settings">>;
     use_gravatar -> <<"Use <a href=\"http://gravatar.com\" "
          "target=\"_blank\">Gravatar</a>?">>;
     profile_bg -> <<"Profile background">>;
     profile_bg_help ->
         <<"Enter the url for your profile background image "
          "(leave blank to use the default background):">>;
     twitter_help ->
         <<"You may provide your Twitter account details to have "
          "your twoorls automatically posted to Twitter.<br/><br/>"
          "Only twoorls that don't contain replies (e.g."
          "\"@sergey\") will be posted to Twitter.">>;
     twitter_username -> <<"Twitter username:">>;
     twitter_password -> <<"Twitter password:">>;
     twitter_auto -> <<"Automatically post my Twoorls to Twitter?">>;
     submit -> <<"submit">>;
    
     %% error messages
     {missing_field, Field} ->
         [<<"The ">>, Field, <<" field is required">>];
     {username, Val} ->
         [<<"The username '">>, Val, <<"' is taken">>];
     {invalid_username, Val} ->
         [<<"The username '">>, Val,
          <<"' is invalid. Only letters, numbers and underscore ('_') "
           "are accepted">>];
     invalid_login ->
         <<"Invalid username or password">>;
     {too_short, Field, Min} ->
         [<<"The ">>, Field, <<" is too short (">>, Min,
          <<" chars minimum)">>];
     password_mismatch ->
         <<"The password values didn't match">>;
     twitter_unauthorized ->
         <<"Twitter rejected the username/password combination you "
          "provided">>;
     twitter_authorization_error ->
         <<"Couldn't connect to Twitter. Please try again later.">>;
     {invalid_url, Field} ->
         [<<"The ">>, Field, <<" URL must start with 'http://'">>];
    
     %% confirmation messages
     settings_updated ->
         [<<"Your settings have been updated successfully">>]
   end.
