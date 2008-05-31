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
