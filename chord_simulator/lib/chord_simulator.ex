defmodule ChordSimulator do
  def start do
    {:ok, _} = ChordSimulator.Supervisor.start_link(self())
    receive do
      :done -> IO.write("")
    end
  end
end
