defmodule Afterwill.Repo do
  use Ecto.Repo,
    otp_app: :afterwill,
    adapter: Ecto.Adapters.Postgres
end
