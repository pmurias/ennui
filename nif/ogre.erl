-module(ogre).
-export([init_ogre/0]).
-on_load(load_c_module/0).
load_c_module() ->
      erlang:load_nif("./ogre", 0).
init_ogre() ->
      "NIF library not loaded".
