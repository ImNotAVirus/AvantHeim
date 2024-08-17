# AvantHeim

## Setup for local development

* Install PostgreSQL and Elixir
* Update config files for PostgreSQL credential and port
* Run `./setup_project`

## Run the project for local development

**Windows not supported yet**

    # If you want to start the whole project
    ./mix --app game_service --sname game run --no-halt
    ./mix --app login_service --sname login run --no-halt
    ./mix --app channel_service --sname channel run --no-halt

    # If you want to have an iex shell on an app
    ./iex --app game_service --sname game -S mix
    ./iex --app login_service --sname login -S mix
    ./iex --app channel_service --sname channel -S mix

## Run a simple local cluster

    ./mix --app login_service --sname login1 run --no-halt
    ./mix --app login_service --sname login2 run --no-halt
