defmodule LogParser do
  # handle_file(File.open("/var/log/messages"))
  def handle_file({:ok, file}) do
    {:ok, line} = IO.read(file, :line)    
  end

  def handle_file({_, error}) do
    "Error: #{:file.format_error(error)}"
  end

  def parse_by_line(filename) do
      File.stream!(filename)
        |> Stream.map(&parse(&1))
	|> Stream.run
  end
  
  # Feb 26 13:30:14 openbsd apmd: cannot set hw.perfpolicy
  # Feb 26 13:32:56 openbsd pulseaudio[14084]: [(null)] main.c: Daemon startup failed.
  # ["Dec 20 23:00:01 openbsd newsyslog[95649]: logfile turned over", "Dec", "20", "23", "00", "01", "openbsd", "newsyslog", "[95649]", "95649", "logfile turned over"]
  def parse(line) do
    [match, month, day, hour, minute, second, host, command, _, pid, message] = Regex.run(~r/^(?<Month>\w{3})\s+(?<Day>\d{2})\s(?<Hour>\d{2}):(?<Minute>\d{2}):(?<Second>\d{2})\s(?<Host>\w+?)\s(?<Command>\w+?)(\[(?<Pid>\d+?)\])?:\s(?<Message>.+)$/, line)
  end
end
