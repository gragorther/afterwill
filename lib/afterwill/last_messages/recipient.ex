defmodule Afterwill.LastMessages.Recipient do
  use Ecto.Schema
  import Ecto.Changeset
  alias Afterwill.LastMessages.LastMessage

  schema "recipients" do
    field :email, :string
    belongs_to :last_message, LastMessage

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(recipient, attrs) do
    recipient
    |> cast(attrs, [:email])
    |> validate_required([:email])
  end
end
