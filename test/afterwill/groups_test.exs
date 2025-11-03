defmodule Afterwill.GroupsTest do
  use Afterwill.DataCase

  alias Afterwill.Groups

  describe "groups" do
    alias Afterwill.Groups.Group

    import Afterwill.AccountsFixtures, only: [user_scope_fixture: 0]
    import Afterwill.GroupsFixtures

    @invalid_attrs %{name: nil, description: nil}

    test "list_groups/1 returns all scoped groups" do
      scope = user_scope_fixture()
      other_scope = user_scope_fixture()
      group = group_fixture(scope)
      other_group = group_fixture(other_scope)
      assert Groups.list_groups(scope) == [group]
      assert Groups.list_groups(other_scope) == [other_group]
    end

    test "get_group!/2 returns the group with given id" do
      scope = user_scope_fixture()
      group = group_fixture(scope)
      other_scope = user_scope_fixture()
      assert Groups.get_group!(scope, group.id) == group
      assert_raise Ecto.NoResultsError, fn -> Groups.get_group!(other_scope, group.id) end
    end

    test "create_group/2 with valid data creates a group" do
      valid_attrs = %{name: "some name", description: "some description"}
      scope = user_scope_fixture()

      assert {:ok, %Group{} = group} = Groups.create_group(scope, valid_attrs)
      assert group.name == "some name"
      assert group.description == "some description"
      assert group.user_id == scope.user.id
    end

    test "create_group/2 with invalid data returns error changeset" do
      scope = user_scope_fixture()
      assert {:error, %Ecto.Changeset{}} = Groups.create_group(scope, @invalid_attrs)
    end

    test "update_group/3 with valid data updates the group" do
      scope = user_scope_fixture()
      group = group_fixture(scope)
      update_attrs = %{name: "some updated name", description: "some updated description"}

      assert {:ok, %Group{} = group} = Groups.update_group(scope, group, update_attrs)
      assert group.name == "some updated name"
      assert group.description == "some updated description"
    end

    test "update_group/3 with invalid scope raises" do
      scope = user_scope_fixture()
      other_scope = user_scope_fixture()
      group = group_fixture(scope)

      assert_raise MatchError, fn ->
        Groups.update_group(other_scope, group, %{})
      end
    end

    test "update_group/3 with invalid data returns error changeset" do
      scope = user_scope_fixture()
      group = group_fixture(scope)
      assert {:error, %Ecto.Changeset{}} = Groups.update_group(scope, group, @invalid_attrs)
      assert group == Groups.get_group!(scope, group.id)
    end

    test "delete_group/2 deletes the group" do
      scope = user_scope_fixture()
      group = group_fixture(scope)
      assert {:ok, %Group{}} = Groups.delete_group(scope, group)
      assert_raise Ecto.NoResultsError, fn -> Groups.get_group!(scope, group.id) end
    end

    test "delete_group/2 with invalid scope raises" do
      scope = user_scope_fixture()
      other_scope = user_scope_fixture()
      group = group_fixture(scope)
      assert_raise MatchError, fn -> Groups.delete_group(other_scope, group) end
    end

    test "change_group/2 returns a group changeset" do
      scope = user_scope_fixture()
      group = group_fixture(scope)
      assert %Ecto.Changeset{} = Groups.change_group(scope, group)
    end
  end
end
