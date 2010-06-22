-module(ogre).
-import(bullet).
-export([init_ogre/0,destroy_ogre/0,render_frame/0,key_down/1,capture_input/0,create_scenenode/0,create_entity/1,set_node_position/2,set_node_orientation/2,get_node_position/1,get_node_orientation/1,get_average_fps/0,log_message/1,set_camera_position/1,set_camera_orientation/1,get_camera_position/0,get_camera_orientation/0,get_rotation_to/2,get_quaternion_inverse/1,get_animationstate/2,set_animationstate_enabled/2,set_animationstate_loop/2,add_animationstate_time/2,set_ambient_light/1,attach_entity_to_bone/3,create_overlay/1,create_overlay_container/2,set_overlay_container_dimensions/3,set_overlay_container_position/3,set_overlay_element_colour/2,add_overlay_container/2,show_overlay/1,set_overlay_element_height/2,set_overlay_element_width/2,set_overlay_element_parameter/3,set_overlay_element_caption/2,add_overlay_container_child/2,set_overlay_element_fontname/2,set_overlay_element_metrics_mode/2,add_compositor/1,setup_world_physics/1,play/3]).

-on_load(load_c_module/0).
load_c_module() ->
      erlang:load_nif("./ogre", 0).

init_ogre() -> throw('nif library not loaded').
destroy_ogre() -> throw('nif library not loaded').
capture_input() -> throw('nif library not loaded').
render_frame() -> throw('nif library not loaded').
key_down(_) -> throw('nif library not loaded').
create_scenenode() -> throw('nif library not loaded').
create_entity(_) -> throw('nif library not loaded').
attach_entity_to_node(_,_) -> throw('nif library not loaded').
set_node_position(_,_) -> throw('nif library not loaded').
set_node_orientation(_,_) -> throw('nif library not loaded').
get_node_position(_) -> throw('nif library not loaded').
get_node_orientation(_) -> throw('nif library not loaded').
get_average_fps() -> throw('nif library not loaded').
log_message(_) -> throw('nif library not loaded').
set_camera_position(_) -> throw('nif library not loaded').
set_camera_orientation(_) -> throw('nif library not loaded').
get_camera_position() -> throw('nif library not loaded').
get_camera_orientation() -> throw('nif library not loaded').
get_rotation_to(_,_) -> throw('nif library not loaded').
get_quaternion_inverse(_) -> throw('nif library not loaded').
get_animationstate(_,_) -> throw('nif library not loaded').
set_animationstate_enabled(_,_) -> throw('nif library not loaded').
set_animationstate_loop(_,_) -> throw('nif library not loaded').
add_animationstate_time(_,_) -> throw('nif library not loaded').
set_ambient_light(_) -> throw('nif library not loaded').
attach_entity_to_bone(_,_,_) -> throw('nif library not loaded').
create_overlay(_) -> throw('nif library not loaded').
create_overlay_container(_,_) -> throw('nif library not loaded').
set_overlay_container_dimensions(_,_,_) -> throw('nif library not loaded').
set_overlay_container_position(_,_,_) -> throw('nif library not loaded').
add_overlay_container(_,_) -> throw('nif library not loaded').
show_overlay(_) -> throw('nif library not loaded').
set_overlay_element_metrics_mode(_,_) -> throw('nif library not loaded').
set_overlay_element_width(_,_) -> throw('nif library not loaded').
set_overlay_element_height(_,_) -> throw('nif library not loaded').
set_overlay_element_colour(_,_) -> throw('nif library not loaded').
set_overlay_element_parameter(_,_,_) -> throw('nif library not loaded').
set_overlay_element_caption(_,_) -> throw('nif library not loaded').
add_overlay_container_child(_,_) -> throw('nif library not loaded').
set_overlay_element_fontname(_,_) -> throw('nif library not loaded').
setup_world_physics(_) -> throw('nif library not loaded').
add_compositor(_) -> throw('nif library not loaded').

-record(player,{id,leftDown,rightDown,upDown,downDown,node,entity,body}).
-record(bully,{node,entity,body}).

init_text_overlay() ->
    Overlay = create_overlay('ov1'),
    Panel = create_overlay_container('Panel', 'cont1'),
    set_overlay_container_dimensions(Panel, 1.0, 1.0),
    set_overlay_container_position(Panel, 0.0, 0.0),
    add_overlay_container(Overlay, Panel),
    show_overlay(Overlay),
    Panel.

create_textbox(Panel,Id, X,Y, W, H, Colour, InitialText) ->
    TextBox = create_overlay_container('TextArea', Id),
    set_overlay_element_metrics_mode(TextBox, 1),
    set_overlay_container_dimensions(TextBox, W, H),
    set_overlay_container_position(TextBox, X, Y),
    set_overlay_element_width(TextBox, W),
    set_overlay_element_height(TextBox, H),
    set_overlay_element_parameter(TextBox, 'font_name', 'Liberation'),
    set_overlay_element_parameter(TextBox, 'char_height', '16'),
    set_overlay_element_colour(TextBox, Colour),
    set_overlay_element_caption(TextBox, InitialText),
    add_overlay_container_child(Panel, TextBox),
    TextBox.


create_console(Panel, Size) ->
    lists:map((fun(I) -> create_textbox(Panel, list_to_atom("Console"++[I]), 10.0, 10.0 + (I * 11.0), 500.0, 30.0, {0.0, 0.0, 0.0}, '') end), lists:seq(0, Size)).

log_console([Tb|Console], Format, Args) ->
    Str = lists:flatten(io_lib:format(Format, Args)),
    set_overlay_element_caption(Tb, Str),
    Console++[Tb].
    
create_player(ID, Mesh, BulletWorld) ->
    Node = create_scenenode(),
    Entity=create_entity(Mesh),
    attach_entity_to_node(Entity,Node),
    ClubEntity = create_entity('PoliceClub.mesh'),
    attach_entity_to_bone(Entity, ClubEntity, 'Bone.001_R.004'),

    ColShape = bullet:new_btCylinderShape({0.6, 0.8, 0.0}),
    MotionState = bullet:new_btDefaultMotionState({{0.0,0.0,0.0,1.0}, {0.0,20.0,0.0}}),
    Mass = 70.0,
    Inertia = bullet:btCollisionShape_calculateLocalInertia(ColShape, Mass),
    BodyCI = bullet:new_btRigidBodyConstructionInfo(Mass, MotionState, ColShape, Inertia),
    Body = bullet:new_btRigidBody(BodyCI),

    bullet:btRigidBody_translate(Body, {0,5.0, 0}),
    bullet:btRigidBody_setActivationState(Body, 4), % disable deavtivation
    bullet:btRigidBody_setAngularFactor(Body, {0.0, 1.0, 0.0}),

    bullet:btDynamicsWorld_addRigidBody(BulletWorld, Body),


    set_node_position(Node,{0.0,0.0,0.0}),
    #player{id=ID,leftDown=false,rightDown=false,upDown=false,downDown=false,node=Node,entity=Entity,body=Body}.

create_enemy(Mesh, BulletWorld) ->
    Node = create_scenenode(),
    Entity=create_entity(Mesh),
    attach_entity_to_node(Entity,Node),

    ColShape = bullet:new_btCylinderShape({0.6, 0.8, 0.0}),
    MotionState = bullet:new_btDefaultMotionState({{0.0,0.0,0.0,1.0}, {0.0,20.0,0.0}}),
    Mass = 20.0,
    Inertia = bullet:btCollisionShape_calculateLocalInertia(ColShape, Mass),
    BodyCI = bullet:new_btRigidBodyConstructionInfo(Mass, MotionState, ColShape, Inertia),
    Body = bullet:new_btRigidBody(BodyCI),

    bullet:btRigidBody_translate(Body, {0,5.0, 0}),
    bullet:btRigidBody_setActivationState(Body, 4), % disable deavtivation
    bullet:btRigidBody_setAngularFactor(Body, {0.0, 1.0, 0.0}),

    bullet:btDynamicsWorld_addRigidBody(BulletWorld, Body),
    #bully{node=Node,entity=Entity,body=Body}.

play(ID, Clients, Players_) ->
    init_ogre(),

    Broadphase = bullet:new_btDbvtBroadphase(),
    DCConfiguration = bullet:new_btDefaultCollisionConfiguration(),
    ColDispatcher = bullet:new_btCollisionDispatcher(),
    ConstraintSolver = bullet:new_btSequentialImpulseConstraintSolver(),
    BulletWorld = bullet:new_btDiscreteDynamicsWorld(ColDispatcher, Broadphase, ConstraintSolver, DCConfiguration),


%    add_compositor('Bloom'),
    Panel = init_text_overlay(),
    Con = create_console(Panel, 20),
    create_textbox(Panel, 'ver', 10.0, 480.0, 500.0, 30.0, {1.0, 0.0, 0.0}, ?VERSION),

    set_ambient_light({0.7, 0.7, 0.7}),
    GrassNode = create_scenenode(),
    set_camera_position({0.0, 2.8, 0.0}),
    Orient = get_rotation_to({0.0, 0.0, 1.0}, {0.0, 0.6, 2.0}),
    set_camera_orientation(Orient),
    GrassEntity = create_entity('Grass.mesh'),
    attach_entity_to_node(GrassEntity, GrassNode),
    register(ID, self()),

    WorldBodyInt = setup_world_physics('Grass.mesh'),
    WorldBody = bullet:btIntToBody(WorldBodyInt),
    bullet:btDynamicsWorld_addRigidBody(BulletWorld, WorldBody),

    Enemies = [ create_enemy('Bully.mesh', BulletWorld) ],

    Players = lists:map(
        fun (p0) -> create_player(p0, 'Policeman.mesh',BulletWorld);
            (p1) -> create_player(p1, 'Blackman.mesh',BulletWorld) end,Players_),
    play_loop(1,ID, Players, Enemies, {false,false,false,false}, [self()|Clients], Con, BulletWorld),
    destroy_ogre().

-define(KC_ESCAPE,1).
-define(KC_DOWN,16#D0).
-define(KC_LEFT,16#CB).
-define(KC_UP,16#C8).
-define(KC_RIGHT,16#CD).

log(Format, Args) ->
    Str = lists:flatten(io_lib:format(Format, Args)),
    log_message(Str).

handle_input({OldLeft,OldRight,OldUp,OldDown}) ->
    Left = key_down(?KC_LEFT),
    Right = key_down(?KC_RIGHT),
    Up = key_down(?KC_UP),
    Down = key_down(?KC_DOWN),
    Input =
    case Left of
        OldLeft -> [];
        _ -> [{keyChange,?KC_LEFT,Left}]
    end ++
    case Right of
        OldRight -> [];
        _ -> [{keyChange,?KC_RIGHT,Right}]
    end ++
    case Up of
        OldUp -> [];
        _ -> [{keyChange,?KC_UP,Up}]
    end ++ 
    case Down of
        OldDown -> [];
        _ -> [{keyChange,?KC_DOWN,Down}]
    end,
    {{Left,Right,Up,Down},Input}.

send_to_clients(Clients,Event) -> lists:foreach((fun(Client)->Client ! Event end), Clients).

handle_player(Player,Input) ->
    lists:foldl(fun
        ({keyChange,?KC_LEFT,State},P)  -> P#player{leftDown=State};
        ({keyChange,?KC_RIGHT,State},P) -> P#player{rightDown=State};
        ({keyChange,?KC_DOWN,State},P)  -> P#player{downDown=State};
        ({keyChange,?KC_UP,State},P) -> P#player{upDown=State} end,
        Player,Input
    ).

enemy_logic(Enemy=#bully{}) ->
    Speed = 3.0,
    LeftRotation = get_rotation_to({0.0, 0.0, 1.0}, {0.04, 0.0, 1.0}),
    RightRotation = get_rotation_to({0.0, 0.0, 1.0}, {-0.04, 0.0, 1.0}),
    RunAnimState = get_animationstate(Enemy#bully.entity, 'Run'),
    IdleAnimState = get_animationstate(Enemy#bully.entity, 'Idle'),
    Node = Enemy#bully.node,
    Body = Enemy#bully.body,
    CurrentVelocity = bullet:btRigidBody_getLinearVelocity(Body),
    Pos = get_node_position(Node),

            set_animationstate_enabled(IdleAnimState, 1),
            set_animationstate_enabled(RunAnimState, 0),
            add_animationstate_time(IdleAnimState, 0.00666),
    bullet:btRigidBody_setLinearVelocity(Body, {0.0, vec_y(CurrentVelocity) - 0.41, 0.0}),

    {Bx,By,Bz} = bullet:btRigidBody_getCenterOfMassPosition(Body),
    set_node_position(Node, {Bx,By-1.0,Bz}).


player_logic(Player) ->
    Speed = 5.0,
    LeftRotation = get_rotation_to({0.0, 0.0, 1.0}, {0.04, 0.0, 1.0}),
    RightRotation = get_rotation_to({0.0, 0.0, 1.0}, {-0.04, 0.0, 1.0}),
    RunAnimState = get_animationstate(Player#player.entity, 'Run'),
    IdleAnimState = get_animationstate(Player#player.entity, 'Idle'),
    Node = Player#player.node,
    Body = Player#player.body,
    CurrentVelocity = bullet:btRigidBody_getLinearVelocity(Body),
    Pos = get_node_position(Node),
    log("player ~p position: ~p", [Player#player.id, Pos]),
    case Player#player.leftDown of
        true -> rotate_node(Player#player.node,LeftRotation);
        false -> ok
    end,
    case Player#player.upDown of
        true -> 
            Forward = vec_mult_quat({0.0, vec_y(CurrentVelocity) - 0.41, Speed}, get_node_orientation(Node)),
            bullet:btRigidBody_setLinearVelocity(Body, Forward),
            bullet:btRigidBody_setFriction(Body, 0.0),
            set_animationstate_enabled(IdleAnimState, 0),
            set_animationstate_enabled(RunAnimState, 1),
            add_animationstate_time(RunAnimState, 0.01666);
        false ->
            bullet:btRigidBody_setLinearVelocity(Body, {0.0, vec_y(CurrentVelocity) - 0.41, 0.0}),
            bullet:btRigidBody_setFriction(Body, 1.0),
            set_animationstate_enabled(IdleAnimState, 1),
            set_animationstate_enabled(RunAnimState, 0),
            add_animationstate_time(IdleAnimState, 0.00666),
            ok
    end,    
    case Player#player.rightDown of
        true -> rotate_node(Player#player.node,RightRotation);
        false -> ok
    end,
    {Bx,By,Bz} = bullet:btRigidBody_getCenterOfMassPosition(Body),
    set_node_position(Node, {Bx,By-1.0,Bz}).

vec_x({V,_,_}) -> V.
vec_y({_,V,_}) -> V.
vec_z({_,_,V}) -> V.

vec_cross_product({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    {Y1 * Z2 - Z1 * Y2, 
    Z1 * X2 - X1 * Z2, 
    X1 * Y2 - Y1 * X2}.

vec_mult_quat({VX,VY,VZ}, {QW,QX,QY,QZ}) ->
    {UVx, UVy, UVz} = vec_cross_product({QX,QY,QZ}, {VX,VY,VZ}),
    {UUVx, UUVy, UUVz} = vec_cross_product({QX,QY,QZ}, {UVx,UVy,UVz}),
    {VX + (2.0 * QW) * UVx + UUVx * 2.0 ,
    VY + (2.0 * QW) * UVy + UUVy * 2.0,
    VZ + (2.0 * QW) * UVz + UUVz * 2.0}.

quat_mult_quat({W1,X1,Y1,Z1}, {W2,X2,Y2,Z2}) ->
    { W1 * W2 - X1 * X2 - Y1 * Y2 - Z1 * Z2,
      W1 * X2 + X1 * W2 + Y1 * Z2 - Z1 * Y2,
      W1 * Y2 + Y1 * W2 + Z1 * X2 - X1 * Z2,
      W1 * Z2 + Z1 * W2 + X1 * Y2 - Y1 * X2 }.

vec_sub({X1,Y1,Z1},{X2,Y2,Z2}) -> {X1-X2,Y1-Y2,Z1-Z2}.
vec_add({X1,Y1,Z1},{X2,Y2,Z2}) -> {X1+X2,Y1+Y2,Z1+Z2}.
vec_length({X,Y,Z}) -> math:sqrt(X*X + Y*Y + Z*Z).

move_node(Node,By) ->
    {X,Y,Z} = get_node_position(Node),
    Orientation = get_node_orientation(Node),
    {ByX, ByY, ByZ} = vec_mult_quat(By, Orientation),
    set_node_position(Node,{X + ByX,Y+ByY,Z+ByZ}).

rotate_node(Node, By) ->
    CurrentOrientation = get_node_orientation(Node),
    NewOrientation = quat_mult_quat(CurrentOrientation, By),
    set_node_orientation(Node, NewOrientation).

find_localplayer(Players,LocalPlayerID) ->
    [LocalPlayer] = lists:filter((fun(Player) -> ID = Player#player.id, ID == LocalPlayerID end), Players),
    LocalPlayer.

play_loop(Frame,LocalPlayerID,Players,Enemies,InputState,Clients,Console,BulletWorld) ->
    capture_input(),
    bullet:btDynamicsWorld_stepSimulation(BulletWorld),
    render_frame(),

    {NewInputState,Input} = handle_input(InputState),

    Sending = {frameDone,LocalPlayerID,Frame,Input},
    log("sending to clients ~w",[Sending]),

    send_to_clients(Clients,Sending),
    NewPlayers = lists:map(fun (Player) ->
        ID = Player#player.id,
        log("waiting for player ~p ~p ~p",[ID,Frame,?VERSION]),
        receive 
            {frameDone,ID,Frame,Input2} -> log("player ~p recieved ~p", [Player, Input2]),handle_player(Player,Input2)          
        after 2000 -> halt()
        end
    end,Players),

    NewEnemies = Enemies,

    lists:foreach(fun player_logic/1,NewPlayers),
    lists:foreach(fun enemy_logic/1,NewEnemies),
    LocalPlayer = find_localplayer(NewPlayers,LocalPlayerID),


    LPNode = LocalPlayer#player.node,
    {X,Y,Z} = get_node_position(LPNode),
    NewConsole = log_console(Console, "FPS ~w", [get_average_fps()]),
    NodeOrientation = get_node_orientation(LPNode),
    CameraDownRotation = get_rotation_to({0.0, 0.0, 1.0}, {0.0, 0.4, 2.0}),
    Camera180Rotation = get_rotation_to({0.0, 0.0, 1.0}, {0.0, 0.0, -1.0}),
    CameraOrientation = quat_mult_quat(quat_mult_quat(NodeOrientation, Camera180Rotation), CameraDownRotation),
    set_camera_orientation(CameraOrientation),
    {CamMovementX, CamMovementY, CamMovementZ} = vec_mult_quat({0.0, 0.0, -6.0}, NodeOrientation),
    set_camera_position({X+CamMovementX,Y+CamMovementY+3.2,Z+CamMovementZ}),

%    NewConsole = log_console(Console, "FPS: ~p (~p,~p,~p)", [get_average_fps(),X,Y,Z]),

    Esc = key_down(?KC_ESCAPE),
    case Esc of
        false -> 
            play_loop(Frame+1,LocalPlayerID,NewPlayers,NewEnemies,NewInputState,Clients,NewConsole,BulletWorld);
        true -> halt
    end.

