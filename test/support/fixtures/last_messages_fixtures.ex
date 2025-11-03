defmodule Afterwill.LastMessagesFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Afterwill.LastMessages` context.
  """

  @doc """
  Generate a last_message.
  """
  def last_message_fixture(scope, attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        content: "some content",
        title: "some title"
      })

    {:ok, last_message} = Afterwill.LastMessages.create_last_message(scope, attrs)
    last_message
  end
end
