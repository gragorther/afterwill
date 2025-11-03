defmodule Afterwill.GroupsFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Afterwill.Groups` context.
  """

  @doc """
  Generate a group.
  """
  def group_fixture(scope, attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        description: "some description",
        name: "some name"
      })

    {:ok, group} = Afterwill.Groups.create_group(scope, attrs)
    group
  end
end
