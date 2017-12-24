%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2017 17:52
%%%-------------------------------------------------------------------
-author("abeniaminov").

-type(customer_id() :: integer()).
-type(bank_id() :: integer()).
-type(account_id() :: binary()).
-type(datetime() :: {{1970..2050, 1..12, 1..31},{0..23, 0..59, 0..59}}).
-type(ip() :: list()).
-type(id() :: bianry()).

-record(session, {
    id :: id(),
    customer_id :: customer_id(),
    customer_name :: list(),
    customer_card :: list(),
    account_id :: account_id(),
    bank_id :: bank_id(),
    state :: integer()
}).

-define(q_session, #session{id = '_', customer_id = '_', customer_id = '_', customer_card = '_', account_id = '_', bank_id = '-', state = '_'}).

-record(customer, {
    id ::   customer_id(),
    name :: list()
}).

-define(q_customer, #customer{id = '_', name = '_'}).

-record(account, {
    id :: id(),
    cid :: customer_id() | bank_id()
}).

-define(q_account, #account{id = '_', cid = '_'}).


-record(bank, {
    id  :: bank_id(),
    name :: list(),
    account_id :: binary()
}).

-define(q_bank, #bank{id = '_', name = '_', account_id = '_'}).


-record(transfer_type, {
    type :: inter_bank | intra_bank,
    limit :: float,
    commission :: float
}).

-define(q_transfer_type, #transfer_type{type = '_', limit = '_', commission = '_'}).


-record(card, {
    card_no :: list(),
    account_id :: binary(),
    bank_id :: bank_id(),
    pin_code_hash :: binary()
}).

-define(q_card, #card{card_no = '_', account_id = '_', bank_id = '_', pin_code_hash = '_'}).


-record(transfer_order, {
    id :: id(),
    type :: inter_bank | intra_bank,
    from :: { bank_id(), account_id()} | null,
    to :: { bank_id() , account_id()} | null,
    amount :: float(),
    created :: datetime(),
    modified :: datetime(),
    state :: integer()
}).

-define(q_transfer_order, #transfer_order{id = '_', type = '_', from = '_', to = '_', amount = '_', created = '_', modified = '_', state = '_'}).

-record(transaction_order,{
    id :: binary(),
    transfer_order_id :: binary(),
    type :: inter_bank | intra_bank,
    tr_type :: refill | withdraw,
    account_id ::  account_id(),
    created :: datetime(),
    amount :: float(),
    modified :: datetime(),
    state :: integer()
}).

-record(transaction, {
    id :: id(),
    transaction_order_id :: id(),
    type :: refill | withdraw | commission_income | commission_outlay,
    type_val :: 1 | -1,
    account_id ::  account_id(),
    amount :: float(),
    state :: integer()
}).

-define(q_transaction, #transaction{id = '_', type = '_', type_val = '_',
    transaction_order_id = '_', amount = '_', account_id = '_', state = '_'}).


-record(card_order, {
    id :: id(),
    type :: refill | withdraw | transfer,
    amount :: float(),
    from_card_no :: list() | null,
    to_card_no :: list() | null,
    order_datetime :: datetime(),
    state :: integer()
}).

-define(q_card_order, #card_order{id = '_', type = '_', from_card_no = '_',
    to_card_no = '_', amount = '_', order_datetime = '_',  state = '_'}).



-record(known_bank,  {
    bank_id :: integer(),
    name :: binary(),
    host :: ip(),
    port :: integer()
}).

-define(q_known_bank, #known_bank{bank_id = '_', name = '_', host = '_', port = '_'}).

-define(preparing, 1).
-define(prepared, 2).
-define(committing, 3).
-define(committed, 4).
-define(rollbacked, 5).
-define(aborted, 6).