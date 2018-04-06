defmodule Aruki.EbWrapper do
  @moduledoc """
  Documentation for Aruki.ReportsApi.
  """

  def rest_port do
    {:ok, port} = Application.get_env(:eb, :rest_port)
    port
  end
end
