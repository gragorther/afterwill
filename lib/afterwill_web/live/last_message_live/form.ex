defmodule AfterwillWeb.LastMessageLive.Form do
  use AfterwillWeb, :live_view

  alias Afterwill.LastMessages
  @impl true
  def mount(params, _session, socket) do
    socket =
      socket
      |> apply_action(socket.assigns.live_action, params)

    {:ok, socket}
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    last_message =
      LastMessages.get_last_message!(socket.assigns.current_scope, id)

    changeset = LastMessages.change_last_message(socket.assigns.current_scope, last_message)

    socket
    |> assign(:page_title, "Edit last message")
    |> assign(:form, to_form(changeset))
    |> assign(:last_message, last_message)
  end

  defp apply_action(socket, :new, _params) do
    last_message = %LastMessages.LastMessage{
      user_id: socket.assigns.current_scope.user.id,
      recipients: []
    }

    changeset = LastMessages.change_last_message(socket.assigns.current_scope, last_message)

    socket
    |> assign(:page_title, "Create last message")
    |> assign(:form, to_form(changeset))
    |> assign(:last_message, last_message)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <.form
        for={@form}
        id="last-message-form"
        phx-submit="save"
        phx-change="validate"
      >
        <.input field={@form[:title]} label="Title" />
        <.input field={@form[:content]} label="Content" />

        <.button phx-disable-with="Saving...">Save Raffle</.button>
      </.form>
    </Layouts.app>
    """
  end

  @impl true
  def handle_event("validate", %{"last_message" => last_message}, socket) do
    changeset =
      LastMessages.change_last_message(
        socket.assigns.current_scope,
        socket.assigns.last_message,
        last_message
      )

    socket = socket |> assign(:form, to_form(changeset, action: :validate))
    {:noreply, socket}
  end

  def handle_event("save", %{"last_message" => last_message}, socket) do
    save_last_message(socket, socket.assigns.live_action, last_message)
  end

  defp save_last_message(socket, :new, attrs) do
    case LastMessages.create_last_message(socket.assigns.current_scope, attrs) do
      {:ok, _last_message} ->
        socket = socket |> redirect(to: ~p"/")
        {:noreply, socket}

      {:error, changeset} ->
        changeset_to_form_and_noreply(socket, changeset)
    end
  end

  defp save_last_message(socket, :edit, attrs) do
    assigns = socket.assigns

    case LastMessages.update_last_message(assigns.current_scope, assigns.last_message, attrs) do
      {:ok, _last_message} ->
        socket = socket |> redirect(to: ~p"/")
        {:noreply, socket}

      {:error, changeset} ->
        changeset_to_form_and_noreply(socket, changeset)
    end
  end

  defp changeset_to_form_and_noreply(socket, changeset) do
    socket = assign(socket, :form, to_form(changeset))
    {:noreply, socket}
  end
end
