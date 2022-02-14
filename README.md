# AvantHeim

## Setup for local development

* Install PostgreSQL and Elixir
* Update config files for PostgreSQL credential and port
* Run `./setup_project`

## Run the project for local development

**Windows not supported yet**

    # If you want to start the whole project
    ./mix --app login_service --name login@127.0.0.1 run --no-halt
    ./mix --app channel_service --name channel@127.0.0.1 run --no-halt

    # If you want to have an iex shell on an app
    ./mix --app login_service --name login@127.0.0.1 run --no-halt
    ./iex --app channel_service --name channel@127.0.0.1 -S mix
