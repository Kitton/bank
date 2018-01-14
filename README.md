# Bank


Bank is an erlang application set on top of a Cowboy server. It is a _toy_ banking server, which provides the following functionality:

- Making **transfers**. Money can be transferred between accounts of the same bank or to accounts in other banks. Other bank servers can be launched to simulate a network of banks. For now, only transactions in Euros are allowed, but other currencies can be added by refactoring only two functions; `bn_cer:cer/2` and `bn_cer:available/1`.
- Retrieving transfers linked to an account

Customers can be registered and accounts opened by using the erlang API defined in `bank.erl`.

**Compilation**

You'll need [rebar3 and erlang](https://www.rebar3.org/docs/getting-started). Tested for versions 19+ of Erlang.

To compile, simply run

```
rebar3 compile
```

To execute in a shell,

```
rebar3 run
```

By default, the server listens on port `8000`. The *port* and *bank code* (ie. IBAN Institution Code) can be set up through config files (examples can be found in the `priv/` directory).

## API

The _bank_ server offers a HTTP JSON API with three resources,

- `/transfers` to order transactions
- `/:iban/transfers` to fetch the transfers linked to an account
- `/interbank/consolidations` used by other _bank_ servers to communicate an inbound transaction

### /transfers

Used to order transactions. For example, to send 1 Euro from account `ESXX000A1` to account `ESXX000B2`, we would do,

```
HTTP/1.1 POST http://localhost:8001/transfers
Content-Type: application/json
{
    "currency": "EUR",
    "receiver": "ESXX000B2",
    "sender": "ESXX000A1",
    "value": 100
}
```

Field `value` is set in cents (eg. 1.23 EUR would be 123). All fields are mandatory.

### /:iban/transfers

Used to retrieve the transactions linked to an account. For example,

```
HTTP/1.1 GET http://localhost:8002/ESXX000B2/transfers
{
    "currency": "EUR",
    "receiver": "ESXX000B2",
    "sender": "ESXX000A1",
    "value": 100
}
```

would result in
```
HTTP/1.1 200 OK
[
    {
        "commission": 0,
        "consolidated": 1515910444,
        "created": 1515910444,
        "currency": "EUR",
        "failed": null,
        "id": "1804120628",
        "preconsolidated": null,
        "receiver": "ESXX000B2",
        "sender": "ESXX000B1",
        "type": "internal",
        "value": 100
    }
]
```

## Data Model

### Conceptual Data Model

Conceptually, banks have customers, and customers have accounts with which they make transactions,


### Logical Data Model

Because each bank holds the information of its customers and their accounts in its own database, the logical model differs slightly from the conceptual one,


### Physical Data Model

The physical data model is completely dependant on the database implementation. In this case, because erlang ETS tables were used, the physical model is as follows,

```erlang
-record(transfer,
        {
          id :: binary(),
          type :: bn_model:transfer_type(),
          sender :: binary(),
          receiver :: binary(),
          value :: non_neg_integer(),
          currency :: binary(),
          commission = 0 :: non_neg_integer(),
          created :: integer(),
          preconsolidated = null :: integer() | null,
          consolidated = null :: integer() | null,
          failed = null :: integer() | null
        }).

-record(customer,
        {
          id :: binary(),
          name :: binary(),
          created :: non_neg_integer()
        }).

-record(account,
        {
          id :: binary(),
          customer :: binary(),
          available = 0 :: integer(),
          balance = 0 :: integer(),
          currency = <<"EUR">> :: binary(),
          created :: integer()
        }).
```

## Architecture


The system is highly modular and great emphasis has been placed on allowing it to be easily expandible.

There is a strong separation between the physical and logical models, to the point that if a different database software is desired, only the module `bn_db.erl` must be refactored. An in-house database implementation was chosen as a self-contained solution was required. Using a database like Postgres or MySQL would have required the tester to further download and configure packages.

New transaction types can be added. Multi-currency support can be added by modifying only two functions in the `bn_cer.erl` module.

