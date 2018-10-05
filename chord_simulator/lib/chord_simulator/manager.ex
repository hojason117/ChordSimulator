defmodule ChordSimulator.Manager do
  use GenServer

  # Client

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: ChordSimulator.Manager)
  end

  # Server (callbacks)

  def init(arg) do
    nodes_checklist = Enum.reduce(1..arg.total_nodes, %{}, fn(x, acc) -> Map.put(acc, "node_#{x}", false) end)
    state = Map.merge(arg, %{nodes_checklist: nodes_checklist, nodes_finished: 0})
    {:ok, state}
  end

  def handle_call(:get_random_entry_node, from, state) do

  end

  def handle_call({:node_finish, node, average_hop}, from, state) do
    GenServer.reply(from, :ok)

    if state.nodes_checklist[node] == false do
      new_nodes_checklist = Map.put(state.nodes_checklist, node, average_hop)
      new_state = Map.merge(state, %{nodes_checklist: new_nodes_checklist, nodes_finished: state.nodes_finished + 1})
      if new_state.nodes_finished == new_state.total_nodes do
        {:stop, :normal, new_state}
      else
        {:noreply, new_state}
      end
    else
      {:noreply, state}
    end
  end

  def terminate(reason, state) do
    if reason == :normal do
      average_hop = Enum.reduce(state.nodes_checklist, 0, fn(x, acc) -> x + acc end) / state.total_nodes
      IO.puts "Average hops: #{average_hop}"
    else
      IO.inspect(reason)
    end
    send(state.daemon, :done)
  end

  # Aux

end
