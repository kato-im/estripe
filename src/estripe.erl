-module(estripe).

-export([create_customer/3]).
-export([update_customer/2]).
-export([delete_customer/1]).
-export([get_customer/1, get_customer/2]).
-export([update_subscription/3, update_subscription/4]).
-export([cancel_subscription/1]).

-export([last_charge/1]).
-export([issue_refund/1]).
-export([issue_refund/2]).

-export([get_invoices/0]).
-export([get_customer_invoices/1]).

-export([account/0, account/1]).

-export([customer_id/1]).
-export([customer_active_card/1]).
-export([customer_subscription/1]).

-export([charge_amount/1]).
-export([charge_id/1]).

-define(HTTP_TIMEOUT, 10000).
-define(CHARGES_PAGE_SIZE, 100).

authorization(StripeKey) ->
    {<<"Authorization">>, <<"Bearer ", StripeKey/binary>>}.

authorization() ->
    {ok, StripeKey} = application:get_env(estripe, stripe_key),
    authorization(StripeKey).

handle_customer_response({ok, {{200, _}, _, Json}}) ->
    {ok, jiffy:decode(Json)};
handle_customer_response({ok, {{402, "Payment Required"}, _, _}}) ->
    {error, payment_required};
handle_customer_response({error, Error}) ->
    {error, Error}.

handle_account_response({ok, {{200, _}, _, Json}}) ->
    {ok, jiffy:decode(Json)};
handle_account_response({error, Error}) ->
    {error, Error}.

create_customer(Token, PlanId, Quantity) ->
    Params = [
        {<<"card">>, Token},
        {<<"plan">>, PlanId},
        {<<"quantity">>, list_to_binary(integer_to_list(Quantity))}
    ],
    Body = form_urlencode(Params),
    handle_customer_response(lhttpc:request(
        "https://api.stripe.com/v1/customers",
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    )).

update_customer(CustomerId, Token) when is_binary(CustomerId) ->
    Params = [{<<"card">>, Token}],
    Body = form_urlencode(Params),
    handle_customer_response(lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId),
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    )).

delete_customer(CustomerId) when is_binary(CustomerId) ->
    handle_customer_response(lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId),
        "DELETE",
        [authorization()],
        ?HTTP_TIMEOUT
    )).

get_customer(CustomerId, StripeKey) ->
    handle_customer_response(lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId),
        "GET",
        [authorization(StripeKey)],
        ?HTTP_TIMEOUT
    )).

get_customer(CustomerId) when is_binary(CustomerId) ->
    {ok, StripeKey} = application:get_env(estripe, stripe_key),
    get_customer(CustomerId, StripeKey).

handle_subscription_response({ok, {{200, _}, _, Json}}) ->
    {ok, jiffy:decode(Json)};
handle_subscription_response({error, Error}) ->
    {error, Error}.

update_subscription(CustomerId, Token, PlanId, Quantity) ->
    Params = [
        {<<"card">>, Token},
        {<<"plan">>, PlanId},
        {<<"quantity">>, list_to_binary(integer_to_list(Quantity))}
    ],
    update_subscription_internal(CustomerId, Params).

update_subscription(CustomerId, PlanId, Quantity) ->
    Params = [
        {<<"plan">>, PlanId},
        {<<"quantity">>, list_to_binary(integer_to_list(Quantity))}
    ],
    update_subscription_internal(CustomerId, Params).

update_subscription_internal(CustomerId, Params) ->
    Body = form_urlencode(Params),
    handle_subscription_response(lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId) ++ "/subscription",
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    )).

cancel_subscription(CustomerId) ->
    Params = [],
    Body = form_urlencode(Params),
    handle_subscription_response(lhttpc:request(
        "https://api.stripe.com/v1/customers/" ++ binary_to_list(CustomerId) ++ "/subscription",
        "DELETE",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    )).

account(StripeKey) ->
    handle_account_response(lhttpc:request(
        "https://api.stripe.com/v1/account",
        "GET",
        [authorization(StripeKey)],
        ?HTTP_TIMEOUT
    )).

account() ->
    {ok, StripeKey} = application:get_env(estripe, stripe_key),
    account(StripeKey).

charges(CustomerId) ->
    charges(CustomerId, 0, []).

charges(CustomerId, Offset, Acc) ->
    Params = binary_to_list(form_urlencode([
        {<<"customer">>, CustomerId},
        {<<"count">>, ?CHARGES_PAGE_SIZE},
        {<<"offset">>, Offset}
    ])),
    Res = lhttpc:request(
        "https://api.stripe.com/v1/charges?" ++ Params,
        "GET",
        [authorization()],
        ?HTTP_TIMEOUT
    ),
    case Res of
        {ok, {{200, _}, _, Json}} ->
            Charges = jsonq:q([<<"data">>], jiffy:decode(Json)),
            case Charges of
                [] ->
                    {ok, Acc};
                Charges ->
                    charges(CustomerId, Offset + ?CHARGES_PAGE_SIZE, Acc ++ Charges)
            end;
        {error, Error} ->
            {error, Error}
    end.

last_charge(CustomerId) ->
    {ok, Charges} = charges(CustomerId),
    case Charges of
        [] ->
            no_charges;
        Charges ->
            {ok, lists:last(Charges)}
    end.

handle_refund_response({ok, {{200, _}, _, Json}}) ->
    {ok, jiffy:decode(Json)};
handle_refund_response({error, Error}) ->
    {error, Error}.

issue_refund(ChargeId) ->
    handle_refund_response(lhttpc:request(
        "https://api.stripe.com/v1/charges/" ++ binary_to_list(ChargeId) ++ "/refund",
        "POST",
        [authorization()],
        ?HTTP_TIMEOUT
    )).

issue_refund(ChargeId, RefundAmount) ->
    Body = form_urlencode([
        {<<"amount">>, RefundAmount}
    ]),
    handle_refund_response(lhttpc:request(
        "https://api.stripe.com/v1/charges/" ++ binary_to_list(ChargeId) ++ "/refund",
        "POST",
        [authorization()],
        Body,
        ?HTTP_TIMEOUT
    )).

handle_invoices_response({ok, {{200, _}, _, Json}}) ->
    {ok, jsonq:q([<<"data">>], jiffy:decode(Json))};
handle_invoices_response({error, Error}) ->
    {error, Error}.

get_invoices() ->
    get_customer_invoices(<<>>).

get_customer_invoices(CustomerId) ->
    handle_invoices_response(lhttpc:request(
        "https://api.stripe.com/v1/invoices/?customer=" ++ binary_to_list(CustomerId),
        "GET",
        [authorization()],
        ?HTTP_TIMEOUT
    )).

customer_id(Obj) ->
    jsonq:q([<<"id">>], Obj).

customer_active_card(Obj) ->
    jsonq:q([<<"active_card">>], Obj).

customer_subscription(Obj) ->
    jsonq:q([<<"subscription">>], Obj).

charge_amount(Obj) ->
    jsonq:q([<<"amount">>], Obj).

charge_id(Obj) ->
    jsonq:q([<<"id">>], Obj).

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
