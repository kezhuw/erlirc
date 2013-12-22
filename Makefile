PROJECT = erlirc

ERLS =  \
erlirc_app.erl                      \
erlirc_sup.erl                      \
erlirc_acceptor.erl                 \
erlirc_listener.erl                 \
erlirc_listener_sup.erl             \
erlirc_connection.erl               \
erlirc_connection_sup.erl           \
erlirc_user.erl                     \
erlirc_user_sup.erl                 \
erlirc_message.erl                  \

SRCS += $(addprefix src/, $(strip $(ERLS)))

app:ebin/$(PROJECT).app
	cp src/$(PROJECT).app.src ebin/$(PROJECT).app

ebin/$(PROJECT).app: $(SRCS)
	@mkdir -p ebin/
	erlc -o ebin/ -pa ebin/ $?

clean:
	rm -rf ebin
