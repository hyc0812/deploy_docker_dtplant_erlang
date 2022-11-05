-module(definedt).
-export([dt_properties/1]).

dt_properties(name)         -> "{\"name\":\"peonydt\"}";
dt_properties(ownership)    -> "yong";
dt_properties(dt_id)        -> "flower_peony_01";
dt_properties(e_tag)        -> "94271cba-f780-46a5-a646-0f0da8d21ecc";
dt_properties(flower_name)  -> "peony";
dt_properties(humidity)     -> F = 30 + rand:uniform(), io_lib:format("~.5f",[F]);
dt_properties(last_update)  -> "2022-11-04T18:39:22.0353689Z".
