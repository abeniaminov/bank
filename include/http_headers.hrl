-define(OUTPUT_CONTROL_ALLOW_HEADERS, [
    {<<"Access-Control-Allow-Origin">>, <<"*">>},
    {<<"Access-Control-Allow-Headers">>, <<"origin, x-requested-with, content-type">>},
    {<<"Access-Control-Allow-Methods">>, <<"PUT, GET, POST, DELETE, OPTIONS">>}
]).

-define(OUTPUT_JSON_HEADER_CTYPE,
    {<<"Content-Type">>, <<"application/json;charset=UTF-8">>}
).

-define(OUTPUT_JSON_HEADERS,[
    ?OUTPUT_JSON_HEADER_CTYPE
]).

-define(OUTPUT_XML_HEADER_CTYPE,
    {<<"Content-Type">>, <<"application/xml;charset=UTF-8">>}
).

-define(OUTPUT_XML_HEADERS,[
    ?OUTPUT_XML_HEADER_CTYPE
]).

-define(OUTPUT_HTML_HEADER_CTYPE,
    {<<"Content-Type">>, <<"text/html;charset=UTF-8">>}
).

-define(OUTPUT_HTML_HEADERS,[
    ?OUTPUT_HTML_HEADER_CTYPE
]).

-define(OUTPUT_TEXT_HEADER_CTYPE,
    {<<"Content-Type">>, <<"text/plain;charset=UTF-8">>}
).

-define(OUTPUT_TEXT_HEADERS,[
    ?OUTPUT_TEXT_HEADER_CTYPE
]).

-define(OUTPUT_PNG_HEADER_CTYPE,
    {<<"Content-Type">>, <<"image/png">>}
).

-define(OUTPUT_PNG_HEADERS,[
    ?OUTPUT_PNG_HEADER_CTYPE
]).

-define(OUTPUT_JPEG_HEADER_CTYPE,
    {<<"Content-Type">>, <<"image/jpeg">>}
).

-define(OUTPUT_JPEG_HEADERS,[
    ?OUTPUT_JPEG_HEADER_CTYPE
]).
