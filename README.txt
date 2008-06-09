To run Twoorl follow the following steps:

- Get the latest versiona of ErlyWeb (prior to 0.7.1, this would be from trunk) and Yaws
- Install MySQL and create a MySQL database for twoorl.
- Run twoorl.sql to create the Twoorl tables.
- Edit src/twoorl_app.hrl with your appropriate environment variables.
- Compile src/twoorl.erl with 'erlc'.
- Edit yaws.conf to add the ErlyWeb application settings for Twoorl.
- Start Yaws and in the shell, type "twoorl:start()". Alternatively, pass Yaws the parameter "--runmon twoorl".

Cheers!
Yariv

To run in embedded mode:

$ make clean && make && make init
$ erl -sname twoorlapp -setcookie twoorl -mnesia dir "'twoorl.mnesia'" -yaws embedded true -pa ebin -boot start_sasl
1> [application:start(X) || X <- [inets, crypto, mnesia, twoorl]].
2> twoorl:start().

# Nick Gerakines
