-module(ogre).
-export([init_ogre/0,init_ogre/0,destroy_ogre/0,render_frame/0,key_down/1,capture_input/0,create_scenenode/0,create_entity/2,play/0]).
-on_load(load_c_module/0).
load_c_module() ->
      erlang:load_nif("./ogre", 0).
init_ogre() ->
      "NIF library not loaded".
destroy_ogre() ->
      "NIF library not loaded".
capture_input() ->
      "NIF library not loaded".
render_frame() ->
      "NIF library not loaded".
key_down(_) ->
      "NIF library not loaded".
create_scenenode() ->
      "NIF library not loaded".
create_entity(_,_) ->
      "NIF library not loaded".

play() ->
    init_ogre(),
    Node = create_scenenode(),
    Entity = create_entity(Node, 'Cube.mesh'),
    play_loop(),
    destroy_ogre().
play_loop () ->
    capture_input(),
    render_frame(),
    Esc = key_down(1),
    case Esc of
        false -> play_loop();
        true -> ok
    end.
