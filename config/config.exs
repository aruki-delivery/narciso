# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# 3rd-party users, it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :legacy_server, key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:legacy_server, :key)
#
# You can also configure a 3rd-party app:
#
#     config :logger, level: :info
#

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env}.exs"

config :eb_wrapper, [
		db_hostname: (System.get_env("EB_PG_HOST") || "localhost") |> String.to_charlist(),
		db_port: ({p, ""} = Integer.parse(System.get_env("EB_PG_PORT") || "5432"); p),
		db_database: (System.get_env("EB_PG_DB") || "postgres") |> String.to_charlist(),
		db_username: (System.get_env("EB_PG_USER") || "postgres") |> String.to_charlist(),
		db_password: (System.get_env("EB_PG_PASS") || "") |> String.to_charlist(),
		db_pool_size: 100,
		db_pool_max_overflow: 20,
		rest_port: 55444,
		templates_dir: './priv/templates'
	]