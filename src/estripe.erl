-module(estripe).

-export([create_customer/1]).
-export([update_customer/2]).
-export([delete_customer/1]).
-export([get_customer/1]).
-export([update_subscription/2]).
-export([cancel_subscription/2]).

-export([last_charge/1]).
-export([issue_refund/1]).
-export([issue_refund/2]).

-export([get_invoices/2]).
-export([get_invoices/0]).
-export([get_customer_invoices/1]).
-export([get_customer_invoices/3]).

-export([customer_id/1]).
-export([active_card/1]).
-export([subscription/1]).

-record(customer, {obj}).
-record(subscription, {obj}).
-record(invoices, {obj}).

-define(HTTP_TIMEOUT, 10000).

authorization() ->
    {ok, SK} = application:get_env(estripe, stripe_key),
    {<<"Authorization">>, <<"Bearer ", SK/binary>>}.

create_customer(Params) ->
    Body = form_urlencode(Params),
    Res = lhttpc:request(
        "https://api.stripe.com/v1/customers",
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    ),
    case Res of
        {ok, {{200, _}, _, Json}} ->
            {ok, #customer{obj = jiffy:decode(Json)}};
        {ok, {{402, "Payment Required"}, _, Json}} ->
            {error, jiffy:decode(Json)}
    end.

update_customer(CustomerId, Params) when is_binary(CustomerId) ->
    Body = form_urlencode(Params),
    Res = lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId),
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    ),
    case Res of
        {ok, {{200, _}, _, Json}} ->
            {ok, #customer{obj = jiffy:decode(Json)}};
        {ok, {{402, "Payment Required"}, _, Json}} ->
            {error, jiffy:decode(Json)}
    end.

delete_customer(CustomerId) when is_binary(CustomerId) ->
    Res = lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId),
        "DELETE",
        [authorization()],
        ?HTTP_TIMEOUT
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, #customer{obj = jiffy:decode(Json)}}.

get_customer(CustomerId) when is_binary(CustomerId) ->
    Res = lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId),
        "GET",
        [authorization()],
        ?HTTP_TIMEOUT
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, #customer{obj = jiffy:decode(Json)}}.

update_subscription(CustomerId, Params) ->
    Body = form_urlencode(Params),
    Res = lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId) ++ "/subscription",
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, #subscription{obj = jiffy:decode(Json)}}.

cancel_subscription(CustomerId, Params) ->
    Body = form_urlencode(Params),
    Res = lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId) ++ "/subscription",
        "DELETE",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, #subscription{obj = jiffy:decode(Json)}}.

charges(CustomerId) ->
    charges(CustomerId, 0, []).

charges(CustomerId, Offset, Acc) ->
    Params = binary_to_list(form_urlencode([
        {<<"customer">>, CustomerId},
        {<<"count">>, 100},
        {<<"offset">>, Offset}
    ])),
    Res = lhttpc:request(
        "https://api.stripe.com/v1/charges?" ++ Params,
        "GET",
        [authorization()],
        ?HTTP_TIMEOUT
    ),
    {ok, {{200, _}, _, Json}} = Res,
    Charges = jsonq:q([<<"data">>], jiffy:decode(Json)),
    case Charges of
        [] ->
            {ok, Acc};
        Charges ->
            charges(CustomerId, Offset + 100, Acc ++ Charges)
    end.

last_charge(CustomerId) ->
    {ok, Charges} = charges(CustomerId),
    case Charges of
        [] ->
            no_charges;
        Charges ->
            {ok, lists:last(Charges)}
    end.

issue_refund(ChargeId) ->
    Res = lhttpc:request(
        "https://api.stripe.com/v1/charges/" ++ binary_to_list(ChargeId) ++ "/refund",
        "POST",
        [authorization()],
        ?HTTP_TIMEOUT
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, jiffy:decode(Json)}.

issue_refund(ChargeId, RefundAmount) ->
    Body = form_urlencode([
        {<<"amount">>, RefundAmount}
    ]),
    Res = lhttpc:request(
        "https://api.stripe.com/v1/charges/" ++ binary_to_list(ChargeId) ++ "/refund",
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, jiffy:decode(Json)}.

get_invoices() ->
    get_invoices(1, 100).

get_invoices(Count, Offset) ->
    get_customer_invoices(<<>>, Count, Offset).

get_customer_invoices(CustomerId) ->
    get_customer_invoices(CustomerId, 1, 100).

get_customer_invoices(CustomerId, Count, Offset) when Count > 0, Count =< 100, Offset >= 0 ->
    Res = lhttpc:request(
        "https://api.stripe.com/v1/invoices/?customer=" ++ binary_to_list(CustomerId),
        "GET",
        [authorization()],
        5000
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, #invoices{obj = jsonq:q([<<"data">>], jiffy:decode(Json))}}.

customer_id(#customer{obj = Obj}) ->
    jsonq:q([<<"id">>], Obj).

active_card(#customer{obj = Obj}) ->
    jsonq:q([<<"active_card">>], Obj).

subscription(#customer{obj = Obj}) ->
    jsonq:q([<<"subscription">>], Obj).

form_urlencode(Proplist) ->
    form_urlencode(Proplist, []).

form_urlencode([], Acc) ->
    list_to_binary(string:join(lists:reverse(Acc), "&"));

form_urlencode([{Key, Value} | R], Acc) when is_binary(Key), is_integer(Value) ->
    form_urlencode([{binary_to_list(Key), integer_to_list(Value)} | R], Acc);

form_urlencode([{Key, Value} | R], Acc) when is_list(Key), is_integer(Value) ->
    form_urlencode([{Key, integer_to_list(Value)} | R], Acc);

form_urlencode([{Key, Value} | R], Acc) when is_binary(Key), is_binary(Value) ->
    form_urlencode([{binary_to_list(Key), binary_to_list(Value)} | R], Acc);

form_urlencode([{Key, Value} | R], Acc) when is_list(Key), is_list(Value) ->
    form_urlencode(R, [esc(Key) ++ "=" ++ esc(Value) | Acc]).

esc(S) -> http_uri:encode(S).
