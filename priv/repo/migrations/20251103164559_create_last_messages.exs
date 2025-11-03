defmodule Afterwill.Repo.Migrations.CreateLastMessagesAndGroups do
  use Ecto.Migration

  def change do
    create table(:last_messages) do
      add :title, :string, null: false
      add :content, :text
      add :user_id, references(:users, type: :id, on_delete: :delete_all), null: false

      timestamps(type: :utc_datetime)
    end

    create table(:groups) do
      add :description, :text
      add :name, :string, null: false
      add :user_id, references(:users, type: :id, on_delete: :delete_all), null: false

      timestamps(type: :utc_datetime)
    end

    create table(:groups_last_messages) do
      add :last_message_id, references(:last_messages, on_delete: :delete_all), null: false
      add :group_id, references(:groups, on_delete: :delete_all), null: false
    end

    create index(:groups, [:user_id])

    create index(:last_messages, [:user_id])
  end
end
