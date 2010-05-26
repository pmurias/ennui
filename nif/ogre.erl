-module(ogre).
-export([init_ogre/0,destroy_ogre/0,render_frame/0,key_down/1,capture_input/0,create_scenenode/0,create_entity/2,set_node_position/2,set_node_orientation/2,get_node_position/1,get_node_orientation/1,get_average_fps/0,log_message/1,set_camera_position/1,set_camera_orientation/1,get_camera_position/0,get_camera_orientation/0,get_rotation_to/2,mult_quaternion_quaternion/2,play/2]).
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
set_node_position(_,_) -> "NIF library not loaded".
set_node_orientation(_,_) -> "NIF library not loaded".
get_node_position(_) -> "NIF library not loaded".
get_node_orientation(_) -> "NIF library not loaded".
get_average_fps() -> "NIF library not loaded".
log_message(_) -> "NIF library not loaded".
set_camera_position(_) -> "NIF library not loaded".
set_camera_orientation(_) -> "NIF library not loaded".
get_camera_position() -> "NIF library not loaded".
get_camera_orientation() -> "NIF library not loaded".
get_rotation_to(_,_) -> "NIF library not loaded".
mult_quaternion_quaternion(_,_) -> "NIF library not loaded".

-record(player,{id,leftDown,rightDown,upDown,downDown,node}).

create_player(ID, Mesh) ->
    Node = create_scenenode(),
    create_entity(Node, Mesh),
    set_node_position(Node,{0.0,0.0,0.0}),
    #player{id=ID,leftDown=false,rightDown=false,upDown=false,downDown=false,node=Node}.

play(ID, Clients) ->
    init_ogre(),
    GrassNode = create_scenenode(),
    set_camera_position({0.0, 2.8, 0.0}),
    Orient = get_rotation_to({0.0, 0.0, 1.0}, {0.0, 0.6, 2.0}),
    set_camera_orientation(Orient),
    create_entity(GrassNode, 'Grass.mesh'),
    register(ID, self()),
    play_loop(ID,[create_player(p0, 'Policeman.mesh'),create_player(p1, 'GreenCube.mesh')], {false,false,false,false}, [self()|Clients]),
    destroy_ogre().

-define(KC_ESCAPE,1).
-define(KC_DOWN,16#D0).
-define(KC_LEFT,16#CB).
-define(KC_UP,16#C8).
-define(KC_RIGHT,16#CD).

log(Format, Args) ->
    Str = lists:flatten(io_lib:format(Format, Args)),
    log_message(Str).

handle_input(ID,{OldLeft,OldRight,OldUp,OldDown},Clients) ->
    Left = key_down(?KC_LEFT),
    Right = key_down(?KC_RIGHT),
    Up = key_down(?KC_UP),
    Down = key_down(?KC_DOWN),
    case Left of
        OldLeft -> ok;
        _ -> send_to_clients(Clients, {ID,keyChange,?KC_LEFT,Left})
    end,
    case Right of
        OldRight -> ok;
        _ -> send_to_clients(Clients, {ID,keyChange,?KC_RIGHT,Right})
    end,
    case Up of
        OldUp -> ok;
        _ -> send_to_clients(Clients, {ID,keyChange,?KC_UP,Up})
    end,
    case Down of
        OldDown -> ok;
        _ -> send_to_clients(Clients, {ID,keyChange,?KC_DOWN,Down})
    end,
    {Left,Right,Up,Down}.

send_to_clients(Clients,Event) -> lists:foreach((fun(Client)->Client ! Event end), Clients).


handle_player(Player) ->
    ID = Player#player.id,
    receive
        {ID,keyChange,?KC_LEFT,State}  -> Player#player{leftDown=State};
        {ID,keyChange,?KC_RIGHT,State} -> Player#player{rightDown=State};
        {ID,keyChange,?KC_DOWN,State}  -> Player#player{downDown=State};
        {ID,keyChange,?KC_UP,State} -> Player#player{upDown=State}
    after
        0 -> Player
    end.

player_logic(Player) ->
    Speed = 0.1,
    case Player#player.leftDown of
        true -> move_node(Player#player.node,{-Speed,0.0,0.0});
        false -> ok
    end,
    case Player#player.upDown of
        true -> move_node(Player#player.node,{0,0.0,-Speed});
        false -> ok
    end,
    case Player#player.downDown of
        true -> move_node(Player#player.node,{0,0.0,Speed});
        false -> ok
    end,
    case Player#player.rightDown of
        true -> move_node(Player#player.node,{Speed,0.0,0.0});
        false -> ok
    end.
move_node(Node,{ByX,ByY,ByZ}) ->  
    {X,Y,Z} = get_node_position(Node),
    set_node_position(Node,{X + ByX,Y+ByY,Z+ByZ}).

    

find_localplayer(Players,LocalPlayerID) ->
    [LocalPlayer] = lists:filter((fun(Player) -> ID = Player#player.id, ID == LocalPlayerID end), Players),
    LocalPlayer.

play_loop (LocalPlayerID,Players,InputState,Clients) ->
    capture_input(),
    render_frame(),
    NewInputState = handle_input(LocalPlayerID,InputState,Clients),
    NewPlayers = lists:map(fun handle_player/1,Players),
    lists:foreach(fun player_logic/1,NewPlayers),
    LocalPlayer = find_localplayer(NewPlayers,LocalPlayerID),
    LPNode = LocalPlayer#player.node,
    {X,Y,Z} = get_node_position(LPNode),
    set_camera_position({X,Y+3.2,Z+5.0}),

    Esc = key_down(?KC_ESCAPE),
    case Esc of
        false -> play_loop(LocalPlayerID,NewPlayers,NewInputState,Clients);
        true -> ok
    end.
