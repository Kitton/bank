defmodule AssistantWeb.PageController do
  use AssistantWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
