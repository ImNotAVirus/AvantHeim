defmodule ElvenGard.ECS.LiveDashboard.Page do
  @moduledoc """
  TODO: Documentation for ElvenGard.ECS.LiveDashboard.Page
  """

  use Phoenix.LiveDashboard.PageBuilder

  alias ElvenGard.ECS.LiveDashboard.Store
  alias Phoenix.LiveDashboard.PageBuilder

  @base_tabs [:startup_systems, :systems]

  ## PageBuilder Behaviour

  @impl PageBuilder
  def mount(params, _session, socket) do
    case params["nav"] do
      nil ->
        to = live_dashboard_path(socket, socket.assigns.page, nav: :startup_systems)
        {:ok, push_navigate(socket, to: to)}

      nav ->
        socket =
          socket
          # |> assign_systems()
          # |> assign_components()
          |> assign(
            selector: nav,
            tabs: @base_tabs
            # component_type: nil,
            # component_table: []
          )

        {:ok, socket}
    end
  end

  @impl PageBuilder
  def menu_link(_, _) do
    {:ok, "ElvenGard.ECS"}
  end

  @impl PageBuilder
  def render(assigns) do
    ~H"""
    <.live_nav_bar id="elvengard_ecs_nav_bar" page={@page}>
      <:item :for={tab <- @tabs} name={to_string(tab)} label={format_nav_name(tab)} method="patch">
        <div></div>
      </:item>
    </.live_nav_bar>
    """
  end

  ## Private functions

  defp format_nav_name(:startup_systems), do: "Startup Systems"
  defp format_nav_name(:systems), do: "Systems"
end
