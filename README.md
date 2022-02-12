# AvantHeim

## Setup for local development

* Install PostgreSQL and Elixir
* Update config files for PostgreSQL credential and port
* Run `./setup_project`

## Run the project for local development

**Windows not supported yet**

    # If you want to start the whole project
    ./mix --app caching_service run --no-halt
    ./mix --app login_endpoint run --no-halt
    ./mix --app channel_endpoint run --no-halt

    # If you want to have an iex shell on an app (not norking currently)
    ./mix --app caching_service run --no-halt
    ./mix --app channel_endpoint run --no-halt
    ./iex --app login_endpoint -S mix
