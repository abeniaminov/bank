%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2017 17:52
%%%-------------------------------------------------------------------
-author("abeniaminov").

-type(customer_id() :: binary()).
-type(session_id() :: binary()).
-type(bank_id() :: integer()).
-type(account_id() :: binary()).
-type(datetime() :: {{1970..2050, 1..12,1..31},{0..23,0..59,0..59}}).
-type(ip() :: {0..255, 0..255, 0..255, 0..255}).

-record(session, {
    id :: session_id(),
    customer_id :: customer_id(),
    customer_name :: binary(),
    customer_card :: list(),
    account_id :: account_id(),
    bank_id :: bank_id(),
    state :: integer()
}).

-record(customer, {
    id :: binary(),
    name :: binary()
}).

-record(account, {
    id :: binary(),
    uid :: customer_id() | bank_id(),
    amount :: float()
}).

-record(bank, {
    id  :: bank_id(),
    name :: binary(),
    account_id :: binary()
}).

-record(card, {
    id :: binary(),
    account_id :: binary(),
    card_no :: integer(),
    bank_id :: bank_id(),
    pin_code_hash :: binary(),
    status :: true | false
}).


-record(transfer_order, {
    id :: binary(),
    type :: inter_bank | intra_bank,
    from :: { bank_id(), account_id()} | null,
    to :: { bank_id() , account_id()} | null,
    amount :: float(),
    created :: datetime(),
    modified :: datetime(),
    state :: integer()
}).

-record(transaction, {
    id :: binary(),
    transfer_order_id :: binary(),
    type :: income | outlay,
    type_val :: 1 | -1,
    account_id ::  account_id(),
    parent_transaction_id :: binary(),
    state :: integer()
}).

-record(card_order, {
    id :: binary(),
    type :: replenishment | write_off | transfer,
    amount :: float(),
    from_card_no :: integer() | null,
    to_card_no :: integer() | null,
    order_datetime :: datetime(),
    state :: integer()
}).

-record(known_bank,  {
    bank_id :: integer(),
    name :: binary(),
    host :: ip(),
    port :: integer()
}).