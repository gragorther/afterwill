defmodule Afterwill.LastMessages do
  @moduledoc """
  The LastMessages context.
  """

  import Ecto.Query, warn: false
  alias Afterwill.Repo

  alias Afterwill.LastMessages.LastMessage
  alias Afterwill.Accounts.Scope

  @doc """
  Subscribes to scoped notifications about any last_message changes.

  The broadcasted messages match the pattern:

    * {:created, %LastMessage{}}
    * {:updated, %LastMessage{}}
    * {:deleted, %LastMessage{}}

  """
  def subscribe_last_messages(%Scope{} = scope) do
    key = scope.user.id

    Phoenix.PubSub.subscribe(Afterwill.PubSub, "user:#{key}:last_messages")
  end

  defp broadcast_last_message(%Scope{} = scope, message) do
    key = scope.user.id

    Phoenix.PubSub.broadcast(Afterwill.PubSub, "user:#{key}:last_messages", message)
  end

  @doc """
  Returns the list of last_messages.

  ## Examples

      iex> list_last_messages(scope)
      [%LastMessage{}, ...]

  """
  def list_last_messages(%Scope{} = scope) do
    Repo.all_by(LastMessage, user_id: scope.user.id)
  end

  @doc """
  Gets a single last_message.

  Raises `Ecto.NoResultsError` if the Last message does not exist.

  ## Examples

      iex> get_last_message!(scope, 123)
      %LastMessage{}

      iex> get_last_message!(scope, 456)
      ** (Ecto.NoResultsError)

  """
  def get_last_message!(%Scope{} = scope, id) do
    Repo.get_by!(LastMessage, id: id, user_id: scope.user.id)
  end

  def get_last_message(scope, id) do
    Repo.get_by(LastMessage, id: id, user_id: scope.user.id)
  end

  @doc """
  Creates a last_message.

  ## Examples

      iex> create_last_message(scope, %{field: value})
      {:ok, %LastMessage{}}

      iex> create_last_message(scope, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_last_message(%Scope{} = scope, attrs) do
    with {:ok, last_message = %LastMessage{}} <-
           %LastMessage{}
           |> LastMessage.changeset(attrs, scope)
           |> Repo.insert() do
      broadcast_last_message(scope, {:created, last_message})
      {:ok, last_message}
    end
  end

  @doc """
  Updates a last_message.

  ## Examples

      iex> update_last_message(scope, last_message, %{field: new_value})
      {:ok, %LastMessage{}}

      iex> update_last_message(scope, last_message, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_last_message(%Scope{} = scope, %LastMessage{} = last_message, attrs) do
    true = last_message.user_id == scope.user.id

    with {:ok, last_message = %LastMessage{}} <-
           last_message
           |> LastMessage.changeset(attrs, scope)
           |> Repo.update() do
      broadcast_last_message(scope, {:updated, last_message})
      {:ok, last_message}
    end
  end

  @doc """
  Deletes a last_message.

  ## Examples

      iex> delete_last_message(scope, last_message)
      {:ok, %LastMessage{}}

      iex> delete_last_message(scope, last_message)
      {:error, %Ecto.Changeset{}}

  """
  def delete_last_message(%Scope{} = scope, %LastMessage{} = last_message) do
    true = last_message.user_id == scope.user.id

    with {:ok, last_message = %LastMessage{}} <-
           Repo.delete(last_message) do
      broadcast_last_message(scope, {:deleted, last_message})
      {:ok, last_message}
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking last_message changes.

  ## Examples

      iex> change_last_message(scope, last_message)
      %Ecto.Changeset{data: %LastMessage{}}

  """
  def change_last_message(%Scope{} = scope, %LastMessage{} = last_message, attrs \\ %{}) do
    true = last_message.user_id == scope.user.id

    LastMessage.changeset(last_message, attrs, scope)
  end
end
