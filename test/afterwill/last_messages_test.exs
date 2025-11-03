defmodule Afterwill.LastMessagesTest do
  use Afterwill.DataCase

  alias Afterwill.LastMessages

  describe "last_messages" do
    alias Afterwill.LastMessages.LastMessage

    import Afterwill.AccountsFixtures, only: [user_scope_fixture: 0]
    import Afterwill.LastMessagesFixtures

    @invalid_attrs %{title: nil, content: nil}

    test "list_last_messages/1 returns all scoped last_messages" do
      scope = user_scope_fixture()
      other_scope = user_scope_fixture()
      last_message = last_message_fixture(scope)
      other_last_message = last_message_fixture(other_scope)
      assert LastMessages.list_last_messages(scope) == [last_message]
      assert LastMessages.list_last_messages(other_scope) == [other_last_message]
    end

    test "get_last_message!/2 returns the last_message with given id" do
      scope = user_scope_fixture()
      last_message = last_message_fixture(scope)
      other_scope = user_scope_fixture()
      assert LastMessages.get_last_message!(scope, last_message.id) == last_message
      assert_raise Ecto.NoResultsError, fn -> LastMessages.get_last_message!(other_scope, last_message.id) end
    end

    test "create_last_message/2 with valid data creates a last_message" do
      valid_attrs = %{title: "some title", content: "some content"}
      scope = user_scope_fixture()

      assert {:ok, %LastMessage{} = last_message} = LastMessages.create_last_message(scope, valid_attrs)
      assert last_message.title == "some title"
      assert last_message.content == "some content"
      assert last_message.user_id == scope.user.id
    end

    test "create_last_message/2 with invalid data returns error changeset" do
      scope = user_scope_fixture()
      assert {:error, %Ecto.Changeset{}} = LastMessages.create_last_message(scope, @invalid_attrs)
    end

    test "update_last_message/3 with valid data updates the last_message" do
      scope = user_scope_fixture()
      last_message = last_message_fixture(scope)
      update_attrs = %{title: "some updated title", content: "some updated content"}

      assert {:ok, %LastMessage{} = last_message} = LastMessages.update_last_message(scope, last_message, update_attrs)
      assert last_message.title == "some updated title"
      assert last_message.content == "some updated content"
    end

    test "update_last_message/3 with invalid scope raises" do
      scope = user_scope_fixture()
      other_scope = user_scope_fixture()
      last_message = last_message_fixture(scope)

      assert_raise MatchError, fn ->
        LastMessages.update_last_message(other_scope, last_message, %{})
      end
    end

    test "update_last_message/3 with invalid data returns error changeset" do
      scope = user_scope_fixture()
      last_message = last_message_fixture(scope)
      assert {:error, %Ecto.Changeset{}} = LastMessages.update_last_message(scope, last_message, @invalid_attrs)
      assert last_message == LastMessages.get_last_message!(scope, last_message.id)
    end

    test "delete_last_message/2 deletes the last_message" do
      scope = user_scope_fixture()
      last_message = last_message_fixture(scope)
      assert {:ok, %LastMessage{}} = LastMessages.delete_last_message(scope, last_message)
      assert_raise Ecto.NoResultsError, fn -> LastMessages.get_last_message!(scope, last_message.id) end
    end

    test "delete_last_message/2 with invalid scope raises" do
      scope = user_scope_fixture()
      other_scope = user_scope_fixture()
      last_message = last_message_fixture(scope)
      assert_raise MatchError, fn -> LastMessages.delete_last_message(other_scope, last_message) end
    end

    test "change_last_message/2 returns a last_message changeset" do
      scope = user_scope_fixture()
      last_message = last_message_fixture(scope)
      assert %Ecto.Changeset{} = LastMessages.change_last_message(scope, last_message)
    end
  end
end
