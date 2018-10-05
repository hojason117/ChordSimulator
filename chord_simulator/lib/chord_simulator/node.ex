defmodule ChordSimulator.Node do
  use GenServer

  # Client

  def start_link(arg) do
    name = {:via, Registry, {ChordSimulator.Registry, "node_#{Enum.at(arg, 1)}"}}
    GenServer.start_link(__MODULE__, arg |> Enum.at(0) |> Map.merge(%{self_id: Enum.at(arg, 1), self_name: "node_#{Enum.at(arg, 1)}"}), name: name)
  end

  # Server (callbacks)

  def init(arg) do
    fail_node = if arg.fail_mode and Enum.random(1..100) <= arg.fail_rate, do: true, else: false
    state = Map.merge(arg, %{fail_node: fail_node, total_hops: 0})
    {:ok, state}
  end

  def terminate(reason, state) do
    if reason == :normal do
      average_hop = state.total_hops / state.num_requests
      :ok = GenServer.call(ChordSimulator.Manager, {:node_finish, state.self_name, average_hop})
    else
      IO.inspect(reason)
    end
  end

  # Aux

  defp create() do

  end

  defp join() do

  end

  defp stabilize() do

  end

  defp notify() do

  end

  defp fix_fingers() do

  end

  defp check_predecessor() do

  end
end
