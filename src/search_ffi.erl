-module(search_ffi).

-export([extract_contents_zip/1, decompress_gzip/1, extract_all_files/1]).

extract_contents_zip(Bits) ->
    case erl_tar:extract({binary, Bits}, [{files, ["contents.tar.gz"]}, memory]) of
        {ok, [{"contents.tar.gz", Data}]} -> {ok, Data};
        _ -> {error, nil}
    end.

decompress_gzip(Bits) ->
    try zlib:gunzip(Bits) of
        Data -> {ok, Data}
    catch
        _:_:_ -> {error, nil}
    end.

extract_all_files(Bits) ->
    case erl_tar:extract({binary, Bits}, [memory]) of
        {ok, Files = [_ | _]} ->
            {ok, remap_files(Files, [])};
        _ ->
            {error, nil}
    end.

remap_files(Files, Out) ->
    case Files of
        [] ->
            Out;
        [{Name, Contents} | Rest] ->
            remap_files(Rest, [{list_to_binary(Name), Contents} | Out])
    end.
