defmodule Assistant.Banks do
  require Poison
  require HTTPoison
  require Logger
  
  alias HTTPoison.Response

  @doc """
  Core of the transfers assistant. Manages communication with the banks
  """
  
  def available do
    ["Bank_A", "Bank_B"]
  end

  @doc """
  Attempts a transfer betweem two accounts through the given bank
  """
  def transfer(bank, from, to, value, currency) do
    case endpoint bank do
      :error ->
        Logger.error("Unknown bank #{inspect bank}")
        :error
      endpoint ->
        body = %{sender: from, receiver: to, value: value, currency: currency}
        response =
          HTTPoison.post(
            endpoint <> "/transfers",
            Poison.encode!(body),
            [{"Content-Type", "application/json"}],
            [])
        case response do
          {:error, reason} ->
            Logger.error("Connection error #{inspect reason}")
            :error
          {:ok, %Response{status_code: "200", body: resp_body}} ->
            transfer = Poison.decode(resp_body)
            Logger.info("SUCCESS. Transaction ID: #{transfer["id"]}")
            {:ok, transfer}
          {:ok, %Response{body: resp_body}} ->
            %{"error_type" => type} = Poison.decode(resp_body)
            Logger.info("FAILURE. Transfer unsuccesful. Reason: #{type}")
            :error
        end
    end
  end

  @doc """
  Retrieves the transactions of a given account
  """
  def get_transactions(bank, account) do
    case endpoint bank do
      :error ->
        Logger.error("Unknown bank #{inspect bank}")
        :error
      endpoint ->
        response =
          HTTPoison.get(
            endpoint <> "/#{account}/transactions",
            [{"Content-Type", "application/json"}],
            [])
        case response do
          {:error, reason} ->
            Logger.error("Connection error #{inspect reason}")
            :error
          {:ok, %Response{status_code: "200", body: resp_body}} ->
            transactions = Poison.decode(resp_body)
            Logger.info("SUCCESS. Transactions: #{transactions}")
            {:ok, transactions}
          {:ok, %Response{body: resp_body}} ->
            %{"error_type" => type} = Poison.decode(resp_body)
            Logger.info("FAILURE. Transfer unsuccesful. Reason: #{type}")
            :error
        end
    end    
  end

  # Returns the bank's endpoint
  defp endpoint(bank) do
    case bank do
      "Bank_A" ->
        "http://localhost:8001"
      "Bank_B" ->
        "http://localhost:8002"
      _ ->
        :error
    end
  end
  
end
