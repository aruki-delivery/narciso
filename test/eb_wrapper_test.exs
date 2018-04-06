defmodule EbWrapperTest do
  use ExUnit.Case
  doctest EbWrapper

  test "greets the world" do
    assert EbWrapper.hello() == :world
  end
end
