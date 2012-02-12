-module(stm_erl).
-author("Ian Plosker").

-compile(export_all).

-define(atomic(X), (fun() ->
            trans_start(),
            Result = X,
            commit(),
            io:format("A: ~p~n", [ [?LINE] ]),
            Result
    end)()).

-on_load(init/0).

-ifdef(TEST).
    -include_lib("eqc/include/eqc.hrl").
    -include_lib("eunit/include/eunit.hrl").
-endif.

-spec init() -> ok | {error, any()}.
init() ->
    case code:priv_dir(dlht) of
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
            SoName = filename:join(Dir, "dlcbf")
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

basic_test() ->
    initialize(),
    ?assert(true),
    {ok, Var} = ?atomic(new_var(1)),
    Val1 = ?atomic(begin
                load_var(Var)
        end),
    ?assert(Val1 == 1),

    ok = ?atomic(store_var(2, Var)),
    Val2 = ?atomic(load_var(Var)),
    ?assert(Val2 == 2),

    Val3 = ?atomic(begin
                Val2 = load_var(Var),
                ok = store_var(Val2 + 1, Var),
                load_var(Var)
        end),
    ?assert(Val3 == 3).

-endif.

