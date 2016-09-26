-module(sling_mailgun).

-export([]).


url(Url) ->
    Url ++ "/" ++ "messages".

auth_header(Password) ->
    [{ "Authorization"
     , "Basic "++ base64:encode_to_string(lists:append(["api",":",Password])) }].
