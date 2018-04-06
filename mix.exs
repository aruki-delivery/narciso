defmodule Aruki.EbWrapper.Mixfile do
  use Mix.Project

  def project do
    [app: :eb_wrapper,
      version: "0.1.0",
      elixir: "~> 1.6",
      deps: deps(Mix.env())]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Aruki.EbWrapper.Application, []}
    ]
  end

  defp deps(_) do
    [
      {:mix_erlang_tasks, "~> 0.1"},
      {:quaff, "~> 1.0"},
      {:kill_bill, "~> 0.1"},
      {:cclock, "~> 0.1"},
      {:async, "~> 0.1"},
      {:epgsql, "~> 3.4"},
      {:poolboy, "~> 1.5"},
      {:gen_smtp, "~> 0.12"},
      {:erlsom, "~> 1.4"},
      {:iso8601, "~> 1.2"},
      {:uuid, "~> 1.7", hex: :uuid_erl},
    ]
  end
end