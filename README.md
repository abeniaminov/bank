bank
=====

An OTP application

Build
-----

    $ rebar3 compile


Start applications
-----
  ./bank_1.sh startd    %% listen 8001
  ./bank_2.sh startd    %% listen 8002

Banks initiated with customer accounts and cards

Bank1  Jose with account 50000 euro, card # 100001

Bank2  Antonio account 50000 euro, card # 200001
       Maria   account 1000 euro,  card # 200002

Transfer
-----

Banks application combined with Transfer Application

Send from bank 1 card 100001 to bank 2 card 200001 100 euro:

http://localhost:8001/transfer/order?card_from=100001&card_to=200001&amount=100
or
http://localhost:8002/transfer/order?card_from=100001&card_to=200001&amount=100
gives the same

Get amount of account linked to card:

http://localhost:8002/bank/card_amount?card_no=200001

Get amount of account linked to bank:

http://localhost:8001/bank/amount   %for bank 1
http://localhost:8002/bank/amount   %for bank 2