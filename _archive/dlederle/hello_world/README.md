# Install Eralng, Gleam and Rebar3
```
asdf plugin-add gleam https://github.com/vic/asdf-gleam.git
asdf install gleam latest
```

You'll also need [erlang](https://github.com/asdf-vm/asdf-erlang).

I installed `rebar3` [from source](https://rebar3.readme.io/docs/getting-started), the brew cask didn't work with my version of erlang/otp.

## Running:

`rebar3 shell`
```
> hello_world:hello_world().
```
