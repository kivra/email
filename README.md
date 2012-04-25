email
=====

Erlang Mail application. Support pluggable backends. Comes default with a mailgun-adapter but adding new adapters is real simple.

## Using
Include email as a rebar dependency with:

	{deps, [{email, ".*", {git, "git://github.com/kivra/email.git", "master"}}]}.

You have to start the email app with `application:start(email)`. Then you can use it as:

	Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

	Eshell V5.9.1  (abort with ^G)
	1> application:start(email).
	ok
	2> email:send({"Silly Wabbit", "test@youremail.com"}, {"Funky Chicken", "me@samples.mailgun.org"}, "Test subject", "test message!! Yihaa.").
	{ok,[{<<"message">>,<<"Queued. Thank you.">>},
     {<<"id">>,
      <<"<20120425121632.29113.10857@samples.mailgun.org>">>}]}

## Configure
To configure edit app.config to your liking.


Happy emailing!

xoxo // Kivra
