defmodule Afterwill.Repo.Migrations.CreateRecipients do
  use Ecto.Migration

  def change do
    create table(:recipients) do
      add :email, :string
      add :last_message, references(:last_messages, on_delete: :nothing)

      timestamps(type: :utc_datetime)
    end

    create index(:recipients, [:user_id])

    create index(:recipients, [:last_message])
  end
end
