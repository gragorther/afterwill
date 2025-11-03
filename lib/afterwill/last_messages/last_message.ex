defmodule Afterwill.LastMessages.LastMessage do
  use Ecto.Schema
  import Ecto.Changeset
  alias Afterwill.Accounts.User
  alias Afterwill.LastMessages.Recipient

  schema "last_messages" do
    field :title, :string
    field :content, :string
    belongs_to :user, User
    has_many :recipients, Recipient

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(last_message, attrs, user_scope) do
    last_message
    |> cast(attrs, [:title, :content])
    |> validate_required([:title, :content])
    |> cast_assoc(:recipients)
    |> put_change(:user_id, user_scope.user.id)
  end
end
