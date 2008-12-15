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
 
-module(twoorl_greek).
-export([bundle/1]).
 
bundle(Tag) ->
    case Tag of
  
  language -> <<"ελληνικά">>;
  
  %% layout
  login -> <<"Είσοδος">>;
  register -> <<"Εγγραφή">>;
  logged_in_as -> <<"Εγγεγραμμένος ως">>;
  settings -> <<"Ρυθμίσεις">>;
  logout -> <<"Έξοδος">>;
  get_source ->
      <<"Κατεβάστε <a href=\"http://code.google.com/p/twoorl\">"
       "τον πηγαίο κώδικα</a>">>;
  
  %% navbar
  home -> <<"Αρχική Σελίδα">>;
  replies -> <<"Απαντήσεις">>;
  me -> <<"εγώ">>;
  everyone -> <<"όλοι">>;
 
  %% login page
  login_cap -> <<"Είσοδος">>;
  username -> <<"Όνομα Χρήστη">>;
  password -> <<"Κωδικός">>;
  not_member -> <<"Όχι μέλος">>;
  login_submit -> <<"Είσοδος">>;
 
  %% register page
  % note: 'username', 'password' and 'Login_cap' are taken from
  % login page section
  register_cap -> <<"Εγγραφή">>;
  email -> <<"email">>;
  password2 -> <<"Ξαναεισάγετε τον κωδικό">>;
  already_member -> <<"Ήδη μέλος;">>;
 
  %% home page
  upto -> <<"Με τί ασχολείσαι;">>;
  twitter_msg -> <<"Αυτόματη αποστολή στο Twitter ενεργοποιημένη για "
      "μη-απαντήσεις">>;
  send -> <<"Αποστολή">>;
 
  %% main page
  public_timeline -> <<"Δημόσιο χρονοδιάγραμμα">>;
 
  %% users page
  {no_user, Username} ->
      [<<"Ο χρήστης '">>, Username, <<"' δεν υπάρχει">>];
  {timeline_of, Username} ->
      [<<"Χρονοδιάγραμμα του ">>, Username];
  following -> <<"ακολουθεί">>;
  followers -> <<"ακόλουθοι">>;
  follow -> <<"ακολούθησε">>;
  unfollow -> <<"μην ακολουθείς">>;
 
  %% friends page
  {friends_of, Userlink} ->
      [<<"Άνθρωποι που ο ">>, Userlink, <<" ακολουθεί">>];
  {followers_of, Userlink} ->
      [<<"Ακόλουθοι του">>, Userlink];
  {no_friends, Username} ->
      [Username, <<" δεν ακολουθεί κανέναν">>];
  {no_followers, Username} ->
      [Username, <<" δεν έχει ακολούθους">>];
 
 
  %% settings page
  settings_cap -> <<"Ρυθμίσεις">>;
  use_gravatar -> <<"Χρησιμοποιήστε το<a href=\"http://gravatar.com\" "
       "target=\"_blank\">Gravatar</a>?">>;
  profile_bg -> <<"Φόντο προφίλ">>;
  profile_bg_help ->
      <<"Εισάγετε την διεύθυνση για το αρχείο φόντου "
       "(αφήστε κενό για να χρησιμοποιηθεί το προεπιλεγμένο φόντο):">>;
  twitter_help ->
      <<"Μπορείτε να παρέχετε τα στοιχεία του λογαριασμού σας στο Twitter ώστε "
       "τα twoorls σας να αποστέλονται αυτόματα στο Twitter.<br/><br/>"
       "Μόνο twoorls, τα οποία δεν περιέχουν απαντήσεις (π.χ."
       "\"@sergey\") θα αποστέλονται στο Twitter.">>;
  twitter_username -> <<"Όνομα χρήστη Twitter:">>;
  twitter_password -> <<"Κωδικός χρήστη Twitter:">>;
  twitter_auto -> <<"Να αποστέλονται αυτόματα τα Twoorls μου στο Twitter;">>;
  submit -> <<"Αποδοχή">>;
  
  %% error messages
  {missing_field, Field} ->
      [<<"Το πεδίο ">>, Field, <<" είναι απαραίτητο">>];
  {username_taken, Val} ->
      [<<"Το όνομα χρήστη '">>, Val, <<"' είναι κατειλημένο">>];
  {invalid_username, Val} ->
      [<<"Το όνομα χρήστη '">>, Val,
       <<"' είναι άκυρο. Μόνο γράμματα, αριθμοί και κάτω παύλα ('_') "
        "γίνονται αποδεκτά">>];
  invalid_login ->
      <<"Εσφαλμένο όνομα χρήστη ή κωδικός">>;
  {too_short, Field, Min} ->
      [<<"Το πεδίο ">>, Field, <<" είναι πολύ μικρό (">>, Min,
       <<" χαρακτήρες τουλάχιστον)">>];
  password_mismatch ->
      <<"Οι δύο κωδικοί δεν ταυτίζονται">>;
  twitter_unauthorized ->
      <<"Το Twitter απέρριψε τον συνδιασμό όνοματος και κωδικού "
       "που δώσατε">>;
  twitter_authorization_error ->
      <<"Αδύνατη η σύνδεση με το Twitter. Παρακαλούμε προσπαθείστε ξανα σε λίγο.">>;
  {invalid_url, Field} ->
      [<<"Το πεδίο διεύθυνσης ">>, Field, <<" πρέπει να ξεκινά με 'http://'">>];
  
  %% confirmation messages
  settings_updated ->
      [<<"Οι ρυθμίσεις σας ανανεώθηκαν επιτυχώς">>];
 
  %% miscellaneous
  {seconds_ago, Val} -> [Val, <<" δευτερόλεπτα πριν">>];
  {minutes_ago, Val} -> [Val, <<" λεπτά πριν">>];
  {hours_ago, Val} -> [Val, <<" ώρες πριν">>];
  {days_ago, Val} -> [Val, <<" μέρες πριν">>]
    end.
