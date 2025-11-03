defmodule Afterwill.LastMessages.LastMessage do
  use Ecto.Schema
  import Ecto.Changeset
  alias Afterwill.Accounts.User
  alias Afterwill.Groups.Group

  schema "last_messages" do
    field :title, :string
    field :content, :string
    belongs_to :user, User

    many_to_many(:groups, Group, join_through: "groups_last_messages")

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(last_message, attrs, user_scope) do
    last_message
    |> cast(attrs, [:title, :content])
    |> cast_assoc(:groups, with: &Group.changeset(&1, &2, user_scope))
    |> validate_required([:title, :content])
    |> put_change(:user_id, user_scope.user.id)
  end
end
