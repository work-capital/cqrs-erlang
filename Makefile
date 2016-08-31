# http://stackoverflow.com/questions/22651850/how-to-use-sync-properly-with-application-built-using-relx-release-assembler
# lock dependencies! https://github.com/inaka/erlang_guidelines, only cowboy needs to be the last
# until cowboy 2.0 is ready.
# we forked eredis, once only master compiled smoothly, so to "snapshot" the version until the next.

PROJECT = cqrs
PROJECT_DESCRIPTION = CQRS work capital lib
PROJECT_VERSION = 0.1.1

EUNIT_OPTS=verbose
ERLC_OPTS=-W0

SHELL_DEPS = kjell sync
SHELL_ERL = $(DEPS_DIR)/kjell/bin/kjell

# dev mode on, change to relx.config for production
RELX_OPTS = -d true 
RELX_CONFIG = $(CURDIR)/relx-dev.config

DEPS = uuid eredis lager sync episcina
dep_uuid         = git https://github.com/okeuday/uuid              v1.5.1
dep_eredis       = git https://github.com/work-capital/eredis       master
dep_lager        = git https://github.com/basho/lager               3.1.0
dep_episcina     = git https://github.com/erlware/episcina          v1.1.0

#DEPS = cowboy gproc uuid amqp_client eredis lager sync jose episcina
#dep_cowboy       = git https://github.com/ninenines/cowboy         2.0.0-pre.2
#dep_gproc        = git https://github.com/uwiger/gproc              0.6
#dep_amqp_client  = git https://github.com/jbrisbin/amqp_client      rabbitmq-3.5.6
#dep_jose         = git https://github.com/potatosalad/erlang-jose   1.7.1

include erlang.mk
