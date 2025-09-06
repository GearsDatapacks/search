-module(search_ffi).

-export([decompress/1]).

decompress(Bits) ->
    case erl_tar:extract({binary, Bits}, [{files, ["contents.tar.gz"]}, memory]) of
        {ok, [{"contents.tar.gz", Data}]} -> {ok, Data};
        _ -> {error, nil}
    end.
