defmodule Aruki.EbWrapper.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  require Quaff
  require Logger

  Quaff.include_lib(Path.expand("#{__DIR__}/../../src/eb_constants.hrl"))

  def start(_type, _args) do
    #TODO: make this use children and Supervisor properly
    Logger.info("Getting main node")
    main_node = case :init.get_argument(:foo) do
      {:ok, [[temp_main_node|_]]} ->
        main_node = :erlang.list_to_atom(temp_main_node)
        #% Check if it is alive or die
        :pong = :net_adm.ping(main_node)
        main_node
      _ -> :undefined
    end
    Logger.info("Main Node=#{inspect main_node}")
    Logger.info("Calling eb_sup.start_link()...")
    {:ok, pid} = :eb_sup.start_link()
    Logger.info("eb_sup.start_link() = {:ok, #{pid}}")
    :ok = :eb_cache_util.init()
    :ok = :eb_notification.init()
    :ok = :eb_api_deliveries.init(main_node)
    :ok = :eb_api_orders.init()
    :ok = :eb_api_session.init(main_node)

    port = Aruki.EbWrapper.rest_port()
    server_config  = {
      :server_config,
      :default,
      [{:port, port}]
    }
    Logger.info("Starting kill_bill on port: #{port}")
    {:ok, server} = :kill_bill.config_server(server_config)
    webapp_config = {:webapp_config, :eb, [
      {:context, "/api"},
      {:action, [
        {:eb_filter_key, [
          {"session", :eb_rest_session},
          {:eb_filter_auth, [
            {"accounts", :eb_rest_accounts},
            {"config", :eb_rest_config},
            {"contacts", :eb_rest_contacts},
            {"deliveries", :eb_rest_deliveries},
            {"orders", :eb_rest_orders},
            {"users", :eb_rest_users}
          ]}
        ]}
      ]}
    ]}
    :ok = :kill_bill.deploy(server, webapp_config)
    :ok = :kill_bill.start_server(server)

    #children = []
    #opts = [strategy: :one_for_one, name: Aruki.EbWrapper.Supervisor]
    #Supervisor.start_link(children, opts)
    {:ok, pid}
  end

  def stop(_state), do: :ok
end
