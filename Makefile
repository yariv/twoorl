all: code

code: clean
	erl -s make all load -run twoorl init_mysql -run twoorl compile -s init stop

clean:
	rm -fv ebin/*.beam twoorl.rel twoorl.script twoorl.boot erl_crash.dump *.log *.access

init:
	erl -mnesia dir "'twoorl.mnesia'" -s make all load -run twoorl init_mnesia -s init stop

cleandb:
	rm -rfv *.mnesia Mnesia*

cleandocs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
	rm -fv doc/*.png