defmodule AfterwillWeb.PageController do
  use AfterwillWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
