email
=====

Erlang Mail application. Support pluggable backends. Comes default with a mailgun-adapter but adding new adapters is real simple.

## Using

### In Erlang

Include email as a rebar dependency with:

``` erlang
	{deps, [{email, ".*", {git, "git://github.com/kivra/email.git", "master"}}]}.
```

You have to start the email app with `application:start(email)`. Then you can use it as:

``` erlang
	Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

	Eshell V5.9.1  (abort with ^G)
	1> application:start(email).
	ok
	2> email:send({<<"Silly Wabbit">>, <<"test@youremail.com">>},
                  {<<"Funky Chicken">>, <<"me@samples.mailgun.org">>},
                  <<"Test subject">>, <<"test message!! Yihaa.">>).
	{ok,[{<<"message">>,<<"Queued. Thank you.">>},
     {<<"id">>,
      <<"<20120425121632.29113.10857@samples.mailgun.org>">>}]}
```

### In Elixir

Add email as a dependency in your `mix.exs` file.

```elixir
defp deps do
  [ { :email, github: "kivra/email" } ]
end
```

After you are done, run `mix deps.get` in your shell to fetch and compile the dependencies.

## Configure

The following example demonstrates a basic app.config section for email
using mailgun.

``` erlang
[
    {email, [
            {adapter, mailgun},
            {mailgun, [
                {domain, "samples.mailgun.org"},
                {apiurl, "https://api.mailgun.net/v2"},
                {apikey, "key-3ax6xnjp29jd6fds4gc373sgvjxteol0"}
            ]}
        ]}
].
```

### In Elixir

Create or update your application configuration file (i.e.: `app.config`) with the configuration
snippet above, and make sure to run `iex` or `mix` with the `erl` `--config` option:

* `iex --erl "-config app.config" -S mix`
or
* `ELIXIR_ERL_OPTS="-config app.config" mix test`

Happy emailing!

xoxo // Kivra
