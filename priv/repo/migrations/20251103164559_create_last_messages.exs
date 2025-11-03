defmodule Afterwill.Repo.Migrations.CreateLastMessagesAndGroups do
  use Ecto.Migration

  def change do
    create table(:last_messages) do
      add :title, :string, null: false
      add :content, :text
      add :user_id, references(:users, type: :id, on_delete: :delete_all), null: false

      timestamps(type: :utc_datetime)
    end

    create index(:last_messages, [:user_id])
  end
end
