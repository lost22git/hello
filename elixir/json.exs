#!/usr/bin/env elixir

defmodule Book do
  @derive JSON.Encoder
  defstruct [:id, :title, :created_at, :tags]

  @type t :: %__MODULE__{
          id: String.t() | nil,
          title: String.t() | nil,
          created_at: DateTime.t() | nil,
          tags: [String.t()] | nil
        }

  @doc """
  JSON decode to an instance of __MODULE__
  """
  @spec from_json!(String.t()) :: __MODULE__.t()
  def from_json!(json) when is_binary(json) do
    map = JSON.decode!(json)

    %__MODULE__{
      id: map["id"],
      title: map["title"],
      tags: map["tags"],
      created_at:
        with s when not is_nil(s) <- map["created_at"],
             {:ok, dt, _} <- DateTime.from_iso8601(s) do
          dt
        else
          _ -> nil
        end
    }
  end
end

defimpl String.Chars, for: Book do
  def to_string(book) do
    inspect(book, pretty: true)
  end
end

# === Main ===

book =
  struct(Book,
    id: "c0d49262-658b-4025-8977-b0f2b43eca3f",
    title: "The Elixir Book",
    created_at: DateTime.utc_now(),
    tags: ["programming-language", "elixir"]
  )

book
|> IO.inspect(label: "Book")
|> JSON.encode!()
|> IO.inspect(label: "json")
|> Book.from_json!()
|> IO.inspect(label: "Book")
