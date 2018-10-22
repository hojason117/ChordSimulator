defmodule ChordSimulator.Manager do
  use GenServer

  # Client

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: ChordSimulator.Manager)
  end

  # Server (callbacks)

  def init(arg) do
    nodes_checklist = Enum.reduce(1..arg.total_nodes, %{}, fn(x, acc) -> Map.put(acc, "node_#{x}", false) end)
    state = Map.merge(arg, %{
      joined_nodes: %{},
      joined_nodes_count: 0,
      nodes_checklist: nodes_checklist,
      nodes_finished: 0,
      node_join_progess: 0
    })
    {:ok, state}
  end

  def handle_call(:get_random_entry_node, _from, state) do
    if state.joined_nodes_count == 0 do
      {:reply, nil, state}
    else
      index = :rand.uniform(state.joined_nodes_count)
      random_node = state.joined_nodes[index]
      {:reply, random_node, state}
    end
  end

  def handle_call({:node_joined, node}, _from, state) do
    new_joined_nodes = Map.put(state.joined_nodes, state.joined_nodes_count + 1, node)
    new_state = Map.merge(state, %{joined_nodes: new_joined_nodes, joined_nodes_count: state.joined_nodes_count + 1})
    print_join_progress(new_state)
    if new_state.joined_nodes_count == new_state.total_nodes do
      IO.puts "\nAll nodes have joined the network."
      Enum.each(Map.values(new_state.joined_nodes), fn(x) -> GenServer.cast({:via, Registry, {ChordSimulator.Registry, x.name}}, :all_node_joined) end)
      IO.puts "Start sending requests..."
    end
    {:reply, :ok, new_state}
  end

  def handle_call({:node_finish, node, average_hop}, from, state) do
    GenServer.reply(from, :ok)

    if state.nodes_checklist[node] == false do
      new_nodes_checklist = Map.put(state.nodes_checklist, node, average_hop)
      new_state = Map.merge(state, %{nodes_checklist: new_nodes_checklist, nodes_finished: state.nodes_finished + 1})
      if new_state.nodes_finished == new_state.total_nodes do
        Enum.each(1..new_state.total_nodes, fn(x) -> GenServer.cast({:via, Registry, {ChordSimulator.Registry, "node_#{x}"}}, :terminate) end)
        :timer.sleep(2000)
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
      average_hop = Enum.reduce(Map.values(state.nodes_checklist), 0, fn(x, acc) -> x + acc end) / state.total_nodes
      IO.puts "Average hops: #{average_hop}"
    else
      IO.inspect(reason)
    end
    send(state.daemon, :done)
  end

  # Aux

  defp print_join_progress(state) do
    new_progress = trunc(state.joined_nodes_count / state.total_nodes * 100)
    unless new_progress == state.node_join_progess, do: IO.write("\rNode joined: [#{new_progress |> Integer.to_string() |> String.pad_leading(3)}%]")
    new_progress
  end
end
