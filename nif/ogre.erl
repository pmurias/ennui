-module(ogre).
-export([init_ogre/0,destroy_ogre/0,render_frame/0,key_down/1,capture_input/0,create_scenenode/0,create_entity/1,set_node_position/2,set_node_orientation/2,get_node_position/1,get_node_orientation/1,get_average_fps/0,log_message/1,set_camera_position/1,set_camera_orientation/1,get_camera_position/0,get_camera_orientation/0,get_rotation_to/2,mult_quaternion_quaternion/2,mult_quaternion_vector/2,get_quaternion_inverse/1,get_animationstate/2,set_animationstate_enabled/2,set_animationstate_loop/2,add_animationstate_time/2,set_ambient_light/1,attach_entity_to_bone/3,play/2]).
-on_load(load_c_module/0).
load_c_module() ->
      erlang:load_nif("./ogre", 0).

init_ogre() -> "NIF library not loaded".
destroy_ogre() -> "NIF library not loaded".
capture_input() -> "NIF library not loaded".
render_frame() -> "NIF library not loaded".
key_down(_) -> "NIF library not loaded".
create_scenenode() -> "NIF library not loaded".
create_entity(_) -> "NIF library not loaded".
attach_entity_to_node(_,_) -> "NIF library not loaded".
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
mult_quaternion_vector(_,_) -> "NIF library not loaded".
get_quaternion_inverse(_) -> "NIF library not loaded".
get_animationstate(_,_) -> "NIF library not loaded".
set_animationstate_enabled(_,_) -> "NIF library not loaded".
set_animationstate_loop(_,_) -> "NIF library not loaded".
add_animationstate_time(_,_) -> "NIF library not loaded".
set_ambient_light(_) -> "NIF library not loaded".
attach_entity_to_bone(_,_,_) -> "NIF library not loaded".

-record(player,{id,leftDown,rightDown,upDown,downDown,node,entity}).

create_player(ID, Mesh) ->
    Node = create_scenenode(),
    Entity=create_entity(Mesh),
    attach_entity_to_node(Entity,Node),
    ClubEntity = create_entity('PoliceClub.mesh'),
    attach_entity_to_bone(Entity, ClubEntity, 'Bone.001_R.004'),

    set_node_position(Node,{0.0,0.0,0.0}),
    #player{id=ID,leftDown=false,rightDown=false,upDown=false,downDown=false,node=Node,entity=Entity}.

play(ID, Clients) ->
    init_ogre(),
    set_ambient_light({0.7, 0.7, 0.7}),
    GrassNode = create_scenenode(),    
    set_camera_position({0.0, 2.8, 0.0}),
    Orient = get_rotation_to({0.0, 0.0, 1.0}, {0.0, 0.6, 2.0}),
    set_camera_orientation(Orient),
    GrassEntity = create_entity('Grass.mesh'),
    attach_entity_to_node(GrassEntity, GrassNode),
    register(ID, self()),
    Players = [create_player(p0, 'Policeman.mesh'),create_player(p1, 'Policeman.mesh')],
    play_loop(1,ID, Players, {false,false,false,false}, [self()|Clients]),
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
    LeftRotation = get_rotation_to({0.0, 0.0, 1.0}, {0.04, 0.0, 1.0}),
    RightRotation = get_rotation_to({0.0, 0.0, 1.0}, {-0.04, 0.0, 1.0}),
    RunAnimState = get_animationstate(Player#player.entity, 'Run'),
    IdleAnimState = get_animationstate(Player#player.entity, 'Idle'),
    case Player#player.leftDown of
        true -> rotate_node(Player#player.node,LeftRotation);
        false -> ok
    end,
    case Player#player.upDown of
        true -> move_node(Player#player.node,{0,0.0,Speed}), 
            set_animationstate_enabled(IdleAnimState, 0),        
            set_animationstate_enabled(RunAnimState, 1),
            add_animationstate_time(RunAnimState, 0.01666);
        false ->
            set_animationstate_enabled(IdleAnimState, 1),
            set_animationstate_enabled(RunAnimState, 0),
            add_animationstate_time(IdleAnimState, 0.00666),
            ok
    end,
    case Player#player.downDown of
        true -> move_node(Player#player.node,{0,0.0,-Speed});
        false -> ok
    end,
    case Player#player.rightDown of
        true -> rotate_node(Player#player.node,RightRotation);
        false -> ok
    end.

move_node(Node,By) ->
    {X,Y,Z} = get_node_position(Node),
    Orientation = get_node_orientation(Node),
    {ByX, ByY, ByZ} = mult_quaternion_vector(Orientation, By),
    set_node_position(Node,{X + ByX,Y+ByY,Z+ByZ}).

rotate_node(Node, By) ->
    CurrentOrientation = get_node_orientation(Node), 
    NewOrientation = mult_quaternion_quaternion(CurrentOrientation, By),
    set_node_orientation(Node, NewOrientation).

find_localplayer(Players,LocalPlayerID) ->
    [LocalPlayer] = lists:filter((fun(Player) -> ID = Player#player.id, ID == LocalPlayerID end), Players),
    LocalPlayer.

wait_for_player(Player,Frame) ->
    ID = Player#player.id,
    receive 
        {frameDone,ID,Frame} -> ok
    end.
play_loop (Frame,LocalPlayerID,Players,InputState,Clients) ->
    capture_input(),
    render_frame(),
    NewPlayers = lists:map(fun handle_player/1,Players),
    NewInputState = handle_input(LocalPlayerID,InputState,Clients),
    lists:foreach(fun player_logic/1,NewPlayers),
    LocalPlayer = find_localplayer(NewPlayers,LocalPlayerID),

    LPNode = LocalPlayer#player.node,
    {X,Y,Z} = get_node_position(LPNode),
    NodeOrientation = get_node_orientation(LPNode),
    CameraDownRotation = get_rotation_to({0.0, 0.0, 1.0}, {0.0, 0.4, 2.0}),
    Camera180Rotation = get_rotation_to({0.0, 0.0, 1.0}, {0.0, 0.0, -1.0}),
    CameraOrientation = mult_quaternion_quaternion(mult_quaternion_quaternion(NodeOrientation, Camera180Rotation), CameraDownRotation),
    set_camera_orientation(CameraOrientation),
    {CamMovementX, CamMovementY, CamMovementZ} = mult_quaternion_vector(NodeOrientation, {0.0, 0.0, -6.0}),
    set_camera_position({X+CamMovementX,Y+CamMovementY+3.2,Z+CamMovementZ}),

    Esc = key_down(?KC_ESCAPE),
    case Esc of
        false -> 
            send_to_clients(Clients,{frameDone,LocalPlayerID,Frame}),
            lists:foreach(fun (Player) -> wait_for_player(Player,Frame) end,NewPlayers),
            play_loop(Frame+1,LocalPlayerID,NewPlayers,NewInputState,Clients);
        true -> ok
    end.

