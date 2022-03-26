# AvantHeim

## Setup for local development

* Install PostgreSQL and Elixir
* Update config files for PostgreSQL credential and port
* Run `./setup_project`

## Run the project for local development

**Windows not supported yet**

    # If you want to start the whole project
    ./mix --app login_service --sname login_service run --no-halt
    ./mix --app channel_service --sname channel_service run --no-halt
    ./mix --app map_service --sname map map_service --no-halt

    # If you want to have an iex shell on an app
    ./iex --app login_service --sname login_service -S mix
    ./iex --app channel_service --sname channel_service -S mix
    ./iex --app map_service --sname map_service -S mix

## Run a simple local cluster

    ./mix --app login_service --sname login_service1 run --no-halt
    ./mix --app login_service --sname login_service2 run --no-halt
