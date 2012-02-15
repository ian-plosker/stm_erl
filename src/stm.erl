-module(stm).
-author("Ian Plosker").

-compile(export_all).

-on_load(init/0).

-define(atomic(X), atomic(fun() -> X end)).
-define(DEFAULT_TRANS_RETRIES, 5).

-ifdef(TEST).
    %-include_lib("eqc/include/eqc.hrl").
    -include_lib("eunit/include/eunit.hrl").
-endif.

atomic(Fun) -> atomic(Fun, ?DEFAULT_TRANS_RETRIES).

atomic(_Fun, 0) -> {error, transaction_failed};
atomic(Fun, N) ->
    trans_start(),
    Result = Fun(),
    case commit() of
        ok ->
            Result;
        error ->
            atomic(Fun, N - 1)
    end.

-spec init() -> ok | {error, any()}.
init() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName =
                    filename:join([filename:dirname(Filename),"../priv",
                            "stm_erl"]);
                _ ->
                    SoName = filename:join("../priv", "stm_erl")
            end;
         Dir ->
            SoName = filename:join(Dir, "stm_erl")
    end,
    erlang:load_nif(SoName, 0).

-spec initialize() -> ok.
initialize() ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-spec close() -> ok.
close() ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-spec trans_start() -> ok.
trans_start() ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-spec commit() -> ok.
commit() ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-spec new_var(integer()) -> {ok, reference()}.
new_var(_Val) ->
    case random:uniform(999999999999) of
        666 -> {ok, make_ref()};
        _   -> exit("NIF library not loaded")
    end.

-spec load_var(reference()) -> {ok, integer()}.
load_var(_var) ->
    case random:uniform(999999999999) of
        666 -> {ok, 1};
        _   -> exit("NIF library not loaded")
    end.

-spec store_var(reference(), integer()) -> ok.
store_var(_Var, _Val) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-ifdef(TEST).

int_test() ->
    initialize(),

    {ok, Var} = ?atomic(new_var(1)),

    Val1 = ?atomic(begin
                load_var(Var)
        end),
    ?assert(Val1 == 1),

    ?atomic(store_var(2, Var)),
    Val2 = ?atomic(load_var(Var)),
    ?assert(Val2 == 2),

    Val3 = ?atomic(begin
                Val2 = load_var(Var),
                ok = store_var(Val2 + 1, Var),
                load_var(Var)
        end),
    ?assert(Val3 == 3).

binary_test() ->
    initialize(),
    {ok, Var} = new_var(<<"abc">>),

    Val1 = ?atomic(load_var(Var)),

    ?assertEqual(<<"abc">>, Val1),

    ?atomic(
        begin
                Val1 = load_var(Var),
                store_var(<<Val1/binary, "def">>, Var)
        end),

    Val2 = ?atomic(load_var(Var)),
    ?assertEqual(<<"abcdef">>, Val2).

sync_test() ->
    initialize(),

    {ok, Var} = new_var(0),

    Self = self(),
    Fun = fun() ->
            timer:sleep(random:uniform(10)),
            case ?atomic(
                begin
                        Val0 = load_var(Var),
                        ok = store_var(Val0 + 1, Var)
                end
            ) of
                {error, _} ->
                    Self ! failed;
                _ ->
                    Self ! successful
            end
    end,

    spawn_fun_n(Fun, 10000),

    TransCount = gather_successful_trans_count(10000),
    Val = ?atomic(load_var(Var)),
    ?assertEqual(TransCount, Val).

spawn_fun_n(_Fun, 0) -> ok;
spawn_fun_n(Fun, N) ->
    spawn(Fun),
    spawn_fun_n(Fun, N - 1).

gather_successful_trans_count(N) ->
    gather_successful_trans_count(N, 0).

gather_successful_trans_count(0, Count) ->
    Count;
gather_successful_trans_count(N, Count) ->
    receive
        failed ->
            gather_successful_trans_count(N - 1, Count);
        successful ->
            gather_successful_trans_count(N - 1, Count + 1)
    end.

-endif.
