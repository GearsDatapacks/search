-module(search_ffi).

-export([extract_all_files/1]).

extract_all_files(Bits) ->
    case erl_tar:extract({binary, Bits}, [{files, ["contents.tar.gz"]}, memory]) of
        {ok, [{"contents.tar.gz", Contents}]} ->
            try
                Data = zlib:gunzip(Contents),
                case erl_tar:extract({binary, Data}, [memory]) of
                    {ok, Files = [_ | _]} ->
                        {ok, remap_files(Files, [])};
                    _ ->
                        {error, nil}
                end
            catch
                _:_:_ -> {error, nil}
            end;
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
