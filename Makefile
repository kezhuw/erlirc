PROJECT = erlirc

app:ebin/$(PROJECT).app
	cp src/$(PROJECT).app.src ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	erlc -o ebin/ -pa ebin/ $?

clean:
	rm -rf ebin
