defmodule Afterwill.Groups do
  @moduledoc """
  The Groups context.
  """

  import Ecto.Query, warn: false
  alias Afterwill.Repo

  alias Afterwill.Groups.Group
  alias Afterwill.Accounts.Scope

  @doc """
  Subscribes to scoped notifications about any group changes.

  The broadcasted messages match the pattern:

    * {:created, %Group{}}
    * {:updated, %Group{}}
    * {:deleted, %Group{}}

  """
  def subscribe_groups(%Scope{} = scope) do
    key = scope.user.id

    Phoenix.PubSub.subscribe(Afterwill.PubSub, "user:#{key}:groups")
  end

  defp broadcast_group(%Scope{} = scope, message) do
    key = scope.user.id

    Phoenix.PubSub.broadcast(Afterwill.PubSub, "user:#{key}:groups", message)
  end

  @doc """
  Returns the list of groups.

  ## Examples

      iex> list_groups(scope)
      [%Group{}, ...]

  """
  def list_groups(%Scope{} = scope) do
    Repo.all_by(Group, user_id: scope.user.id)
  end

  @doc """
  Gets a single group.

  Raises `Ecto.NoResultsError` if the Group does not exist.

  ## Examples

      iex> get_group!(scope, 123)
      %Group{}

      iex> get_group!(scope, 456)
      ** (Ecto.NoResultsError)

  """
  def get_group!(%Scope{} = scope, id) do
    Repo.get_by!(Group, id: id, user_id: scope.user.id)
  end

  @doc """
  Creates a group.

  ## Examples

      iex> create_group(scope, %{field: value})
      {:ok, %Group{}}

      iex> create_group(scope, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_group(%Scope{} = scope, attrs) do
    with {:ok, group = %Group{}} <-
           %Group{}
           |> Group.changeset(attrs, scope)
           |> Repo.insert() do
      broadcast_group(scope, {:created, group})
      {:ok, group}
    end
  end

  @doc """
  Updates a group.

  ## Examples

      iex> update_group(scope, group, %{field: new_value})
      {:ok, %Group{}}

      iex> update_group(scope, group, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_group(%Scope{} = scope, %Group{} = group, attrs) do
    true = group.user_id == scope.user.id

    with {:ok, group = %Group{}} <-
           group
           |> Group.changeset(attrs, scope)
           |> Repo.update() do
      broadcast_group(scope, {:updated, group})
      {:ok, group}
    end
  end

  @doc """
  Deletes a group.

  ## Examples

      iex> delete_group(scope, group)
      {:ok, %Group{}}

      iex> delete_group(scope, group)
      {:error, %Ecto.Changeset{}}

  """
  def delete_group(%Scope{} = scope, %Group{} = group) do
    true = group.user_id == scope.user.id

    with {:ok, group = %Group{}} <-
           Repo.delete(group) do
      broadcast_group(scope, {:deleted, group})
      {:ok, group}
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking group changes.

  ## Examples

      iex> change_group(scope, group)
      %Ecto.Changeset{data: %Group{}}

  """
  def change_group(%Scope{} = scope, %Group{} = group, attrs \\ %{}) do
    true = group.user_id == scope.user.id

    Group.changeset(group, attrs, scope)
  end
end
