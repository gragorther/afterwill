defmodule Afterwill.Groups.Group do
  use Ecto.Schema
  import Ecto.Changeset
  alias Afterwill.Accounts.User
  alias Afterwill.LastMessages.LastMessage

  schema "groups" do
    field :description, :string
    field :name, :string
    belongs_to :user, User
    many_to_many :last_messages, LastMessage, join_through: "groups_last_messages"

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(group, attrs, user_scope) do
    group
    |> cast(attrs, [:description, :name])
    |> validate_required([:description, :name])
    |> put_change(:user_id, user_scope.user.id)
  end
end
