-module(ogre).
-export([init_ogre/0,init_ogre/0,destroy_ogre/0,render_frame/0,key_down/1,capture_input/0,create_scenenode/0,create_entity/2,set_node_position/4,set_node_orientation/5,get_node_position/1,get_node_orientation/1,get_average_fps/0,log_message/1,play/1]).
-on_load(load_c_module/0).
load_c_module() ->
      erlang:load_nif("./ogre", 0).
init_ogre() -> "NIF library not loaded".
destroy_ogre() -> "NIF library not loaded".
capture_input() -> "NIF library not loaded".
render_frame() -> "NIF library not loaded".
key_down(_) -> "NIF library not loaded".
create_scenenode() -> "NIF library not loaded".
create_entity(_,_) -> "NIF library not loaded".
set_node_position(_,_,_,_) -> "NIF library not loaded".
set_node_orientation(_,_,_,_,_) -> "NIF library not loaded".
get_node_position(_) -> "NIF library not loaded".
get_node_orientation(_) -> "NIF library not loaded".
get_average_fps() -> "NIF library not loaded".
log_message(_) -> "NIF library not loaded".

-record(player,{id,leftDown,rightDown,upDown,downDown,node}).

create_player(ID) ->
    Node = create_scenenode(),
    Entity = create_entity(Node, 'Cube.mesh'),
    set_node_position(Node,0.0,0.0,25.0),
    #player{id=ID,leftDown=false,rightDown=false,upDown=false,downDown=false,node=Node}.

play(ID) ->
    init_ogre(),
    play_loop(ID,[create_player(0),create_player(1)], {false,false,false,false}),

    destroy_ogre().

-define(KC_ESCAPE,1).
-define(KC_DOWN,16#D0).
-define(KC_LEFT,16#CB).
-define(KC_UP,16#C8).
-define(KC_RIGHT,16#CD).

log(Format, Args) ->
    Str = lists:flatten(io_lib:format(Format, Args)),
    log_message(Str).

handle_input(ID,{OldLeft,OldRight,OldUp,OldDown}) ->
    Left = key_down(?KC_LEFT),
    Right = key_down(?KC_RIGHT),
    Up = key_down(?KC_UP),
    Down = key_down(?KC_DOWN),
    case Left of
        OldLeft -> ok;
        _ -> send_to_clients({ID,keyChange,?KC_LEFT,Left})
    end,
    case Right of
        OldRight -> ok;
        _ -> send_to_clients({ID,keyChange,?KC_RIGHT,Right})
    end,
    {Left,Right,Up,Down}.

send_to_clients(Event) ->  log("~p,~n", [Event]), self() ! Event.


handle_player(Player) ->
    ID = Player#player.id,
    receive
        {ID,keyChange,?KC_LEFT,State}  -> Player#player{leftDown=State};
        {ID,keyChange,?KC_RIGHT,State} -> Player#player{rightDown=State}
    after
        0 -> Player
    end.

player_logic(Player) ->
    case Player#player.leftDown of
        true -> move_node(Player#player.node,{0.01,0.0,0.0});
        false -> ok
    end,
    case Player#player.rightDown of
        true -> move_node(Player#player.node,{-0.01,0.0,0.0});
        false -> ok
    end.
move_node(Node,{ByX,ByY,ByZ}) -> 
    {X,Y,Z} = get_node_position(Node),
    set_node_position(Node,X + ByX,Y+ByY,Z+ByZ).

play_loop (LocalPlayerID,Players,InputState) ->
    capture_input(),
    render_frame(),
    NewInputState = handle_input(LocalPlayerID,InputState),
    NewPlayers = lists:map(fun handle_player/1,Players),
    lists:foreach(fun player_logic/1,NewPlayers),

    Esc = key_down(?KC_ESCAPE),
    case Esc of
        false -> play_loop(LocalPlayerID,NewPlayers,NewInputState);
        true -> ok
    end.
