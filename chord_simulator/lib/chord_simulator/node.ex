defmodule ChordSimulator.Node do

  defmodule Peer do
    defstruct name: nil, hash: nil
  end

  @hash_func :sha
  @hash_digest 160
  @truncated_digest 4
  @stabilize_period 50
  @check_predecessor_period 50
  @fix_fingers_period 50

  use GenServer

  # Client

  def start_link(arg) do
    name = {:via, Registry, {ChordSimulator.Registry, "node_#{Enum.at(arg, 1)}"}}
    GenServer.start_link(__MODULE__, arg |> Enum.at(0) |> Map.put(:id, Enum.at(arg, 1)), name: name)
  end

  # Server (callbacks)

  def init(arg) do
    GenServer.cast(self(), :start)
    remain_digest = @hash_digest - @truncated_digest
    <<x::@truncated_digest, _::size(remain_digest)>> = :crypto.hash(@hash_func, "node_#{arg.id}")
    x_bits = <<x::@truncated_digest>>
    fail_node = if arg.fail_mode and Enum.random(1..100) <= arg.fail_rate, do: true, else: false
    state = Map.merge(arg, %{
      self: %Peer{name: "node_#{arg.id}", hash: x_bits},
      predecessor: nil,
      successor: nil,
      finger: [],
      fail_node: fail_node,
      total_hops: 0,
      alive: true
    })
    {:ok, state}
  end

  def handle_cast(:start, state) do
    entry_node = GenServer.call(ChordSimulator.Manager, {:get_random_entry_node_n_join, state.self})
    new_state = if entry_node == nil, do: create(state), else: join(state, entry_node)
    Process.send_after(self(), :stabilize, @stabilize_period)
    Process.send_after(self(), :check_predecessor, @check_predecessor_period)
    Process.send_after(self(), {:fix_fingers, 0}, @fix_fingers_period)
    {:noreply, new_state}
  end

  def handle_call({:find_successor, id}, _from, state), do: {:reply, find_successor(state, id), state}

  def handle_call(:predecessor, _from, state), do: {:reply, state.predecessor, state}

  def handle_call({:notify, node}, _from, state), do: {:reply, :ok, notify(state, node)}

  def handle_info(:stabilize, state) do
    if state.alive do
      spawn_link(fn() -> stabilize(state) end)
      Process.send_after(self(), :stabilize, @stabilize_period)
    end
    {:noreply, state}
  end

  def handle_info(:check_predecessor, state) do
    if state.alive do
      spawn_link(fn() -> check_predecessor(state) end)
      Process.send_after(self(), :check_predecessor, @check_predecessor_period)
    end
    {:noreply, state}
  end

  def handle_info({:fix_fingers, next}, state) do
    if state.alive do
      spawn_link(fn() -> fix_fingers(state, next) end)
      next = if next + 1 >= @truncated_digest, do: 0, else: next + 1
      Process.send_after(self(), {:fix_fingers, next}, @fix_fingers_period)
    end
    {:noreply, state}
  end

  def handle_info({:new_state, new_state}, _state), do: {:noreply, new_state}

  def terminate(reason, state) do
    if reason == :normal do
      average_hop = state.total_hops / state.num_requests
      :ok = GenServer.call(ChordSimulator.Manager, {:node_finish, state.self_name, average_hop})
    else
      IO.inspect(reason)
    end
  end

  # Aux

  defp find_successor(state, id) do
    if in_range?(id, state.self.hash, false, state.successor.hash, true) do
      state.successor
    else
      cpn = closest_preceding_node(state, id, @truncated_digest - 1)
      if cpn == state.self do
        state.self
      else
        GenServer.call({:via, Registry, {ChordSimulator.Registry, cpn.name}}, {:find_successor, id})
      end
    end
  end

  defp closest_preceding_node(state, _id, i) when i < 0, do: state.self

  defp closest_preceding_node(state, id, i) do
    if Enum.at(state.finger, i) == nil do
      state.self
    else
      if in_range?(Enum.at(state.finger, i).hash, state.self.hash, false, id, false), do: Enum.at(state.finger, i), else: closest_preceding_node(state, id, i - 1)
    end
  end

  defp create(state) do
    Map.merge(state, %{predecessor: nil, successor: state.self})
  end

  defp join(state, entry_node) do
    successor = GenServer.call({:via, Registry, {ChordSimulator.Registry, entry_node.name}}, {:find_successor, state.self.hash})
    Map.merge(state, %{predecessor: nil, successor: successor})
  end

  defp stabilize(state) do
    # IO.puts "#{state.self.name}\n#{inspect(state.predecessor)}\n#{inspect(state.successor)}"

    x = GenServer.call({:via, Registry, {ChordSimulator.Registry, state.successor.name}}, :predecessor)
    new_state = if x != nil and in_range?(x.hash, state.self.hash, false, state.successor.hash, false), do: Map.put(state, :successor, x), else: state
    :ok = GenServer.call({:via, Registry, {ChordSimulator.Registry, new_state.successor.name}}, {:notify, new_state.self})
    Process.send(self(), {:new_state, new_state}, [])
  end

  defp notify(state, node) do
    if state.predecessor == nil or in_range?(node.hash, state.predecessor.hash, false, state.self.hash, false) do
      Map.put(state, :predecessor, node)
    else
      state
    end
  end

  defp fix_fingers(state, next) do
    <<id_int::@truncated_digest>> = state.self.hash
    id_int = rem(id_int + trunc(:math.pow(2, next)), @truncated_digest)
    id = <<id_int::@truncated_digest>>
    new_finger = List.replace_at(state.finger, next, find_successor(state, id))
    new_state = Map.put(state, :finger, new_finger)
    Process.send(self(), {:new_state, new_state}, [])
  end

  defp check_predecessor(state) do
    new_state =
      if state.predecessor != nil and GenServer.whereis({:via, Registry, {ChordSimulator.Registry, state.predecessor.name}}) == nil do
        Map.put(state, :predecessor, nil)
      else
        state
      end
    Process.send(self(), {:new_state, new_state}, [])
  end

  defp in_range?(target, left, left_inclusive, right, right_inclusive) do
    if left > right do
      if target > right and target < left do
        false
      else
        if (target == left and left_inclusive) or (target == right and right_inclusive), do: true, else: false
      end
    else
      if target >= left and target <= right do
        if (target == left and not left_inclusive) or (target == right and not right_inclusive), do: false, else: true
      else
        false
      end
    end
  end
end
