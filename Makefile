PROJECT = erlirc

ERLS =  \
erlirc_app.erl                      \
erlirc_sup.erl                      \
erlirc_acceptor.erl                 \
erlirc_listener.erl                 \
erlirc_listener_sup.erl             \
erlirc_incoming.erl                 \
erlirc_incoming_sup.erl             \

SRCS += $(addprefix src/, $(strip $(ERLS)))

app:ebin/$(PROJECT).app
	cp src/$(PROJECT).app.src ebin/$(PROJECT).app

ebin/$(PROJECT).app: $(SRCS)
	@mkdir -p ebin/
	erlc -o ebin/ -pa ebin/ $?

clean:
	rm -rf ebin
