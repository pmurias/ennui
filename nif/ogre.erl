-module(ogre).
-import(bullet).
-export([init_ogre/0,destroy_ogre/0,render_frame/0,key_down/1,capture_input/0,create_scenenode/0,create_entity/1,set_node_position/2,set_node_orientation/2,get_node_position/1,get_node_orientation/1,get_average_fps/0,log_message/1,set_camera_position/1,set_camera_orientation/1,get_camera_position/0,get_camera_orientation/0,get_rotation_to/2,get_quaternion_inverse/1,get_animationstate/2,set_animationstate_enabled/2,set_animationstate_loop/2,add_animationstate_time/2,get_animationstate_time/1,set_animationstate_time/2,set_ambient_light/1,attach_entity_to_bone/3,create_overlay/1,create_overlay_container/2,set_overlay_container_dimensions/3,set_overlay_container_position/3,set_overlay_element_colour/2,add_overlay_container/2,show_overlay/1,set_overlay_element_height/2,set_overlay_element_width/2,set_overlay_element_parameter/3,set_overlay_element_caption/2,add_overlay_container_child/2,set_overlay_element_fontname/2,set_overlay_element_metrics_mode/2,add_compositor/1,setup_world_physics/1,play/3]).

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
get_animationstate_time(_) -> throw('nif library not loaded').
set_animationstate_time(_,_) -> throw('nif library not loaded').
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

-record(player,{id,leftDown,rightDown,upDown,downDown,attacks,node,entity,body,hp}).
-record(bully,{node,entity,body,covers,hp,actions,idle}).

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

    ColShape = bullet:new_btCylinderShape({0.35, 1.3, 0.0}),
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
    #player{id=ID,leftDown=false,rightDown=false,upDown=false,downDown=false,attacks=false,node=Node,entity=Entity,body=Body,hp=120}.

create_enemy(Mesh, BulletWorld, Idle, StartPos) ->
    Node = create_scenenode(),
    Entity=create_entity(Mesh),
    attach_entity_to_node(Entity,Node),

    ColShape = bullet:new_btCylinderShape({0.6, 0.8, 0.0}),
    MotionState = bullet:new_btDefaultMotionState({{0.0,0.0,0.0,1.0}, {0.0,0.0,0.0}}),
    Mass = 20.0,
    Inertia = bullet:btCollisionShape_calculateLocalInertia(ColShape, Mass),
    BodyCI = bullet:new_btRigidBodyConstructionInfo(Mass, MotionState, ColShape, Inertia),
    Body = bullet:new_btRigidBody(BodyCI),

    bullet:btRigidBody_translate(Body, StartPos),
    bullet:btRigidBody_setActivationState(Body, 4), % disable deavtivation
    bullet:btRigidBody_setAngularFactor(Body, {0.0, 1.0, 0.0}),

    bullet:btDynamicsWorld_addRigidBody(BulletWorld, Body),
    #bully{node=Node,entity=Entity,body=Body,covers=false,hp=10,actions=[],idle=Idle}.

play(ID, Clients, Players_) ->
    init_ogre(),

    Broadphase = bullet:new_btDbvtBroadphase(),
    DCConfiguration = bullet:new_btDefaultCollisionConfiguration(),
    ColDispatcher = bullet:new_btCollisionDispatcher(),
    ConstraintSolver = bullet:new_btSequentialImpulseConstraintSolver(),
    BulletWorld = bullet:new_btDiscreteDynamicsWorld(ColDispatcher, Broadphase, ConstraintSolver, DCConfiguration),


    add_compositor('Bloom'),
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

    Enemies = [ 
        create_enemy('Bully.mesh', BulletWorld, [], {20.0, 5.0, 10.0} ) 
        ,
        create_enemy('Bully.mesh', BulletWorld, [], {15.0, 5.0, 10.0} ) 
        ,
        create_enemy('Bully2.mesh', BulletWorld, [], {-15.0, 5.0, 10.0} ) 
        ,
        create_enemy('Bully.mesh', BulletWorld, [], {15.0, 5.0, 10.0} ) 
        ,
        create_enemy('Bully2.mesh', BulletWorld, [], {20.0, 5.0, 5.0} ) 
        ,
        create_enemy('Bully.mesh', BulletWorld, [], {0.0, 5.0, 15.0} ) 
        ,
        create_enemy('Bully2.mesh', BulletWorld, [], {3.0, 5.0, -15.0} ) 
        ],

    Players = lists:map(
        fun (p0) -> create_player(p0, 'Policeman.mesh',BulletWorld);
            (p1) -> create_player(p1, 'Blackman.mesh',BulletWorld) end,Players_),
    play_loop(1,ID, Players, Enemies, {false,false,false,false,false}, [self()|Clients], Con, BulletWorld, []),
    destroy_ogre().

-define(KC_ESCAPE,1).
-define(KC_DOWN,16#D0).
-define(KC_LEFT,16#CB).
-define(KC_UP,16#C8).
-define(KC_RIGHT,16#CD).
-define(KC_ATTACK,16#1E).

log(Format, Args) ->
    Str = lists:flatten(io_lib:format(Format, Args)),
    log_message(Str).

handle_input({OldLeft,OldRight,OldUp,OldDown,OldAttack}) ->
    Left = key_down(?KC_LEFT),
    Right = key_down(?KC_RIGHT),
    Up = key_down(?KC_UP),
    Down = key_down(?KC_DOWN),
    Attack = key_down(?KC_ATTACK),
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
    end ++
    case Attack of
        OldAttack -> [];
        _ -> [{keyChange,?KC_ATTACK,Attack}]
    end,
    {{Left,Right,Up,Down,Attack},Input}.

send_to_clients(Clients,Event) -> lists:foreach((fun(Client)->Client ! Event end), Clients).

handle_player(Player,Input) ->
    lists:foldl(fun
        ({keyChange,?KC_LEFT,State},P)  -> P#player{leftDown=State};
        ({keyChange,?KC_RIGHT,State},P) -> P#player{rightDown=State};
        ({keyChange,?KC_DOWN,State},P)  -> P#player{downDown=State};
        ({keyChange,?KC_UP,State},P) -> P#player{upDown=State};
        ({keyChange,?KC_ATTACK,State},P)  -> 
            case State of 
                true ->
                    case P#player.upDown of
                        true -> P;
                        false -> P#player{attacks=State}
                    end;
                false -> P
            end
        end,
        Player,Input
    ).

   
findClosestPlayer(Pos, Players) ->
    {_, Closest} = lists:foldl( fun(Player,{ClosestDist,ClosestPlayer}) ->
        PlayPos = get_node_position(Player#player.node),
        Dist = vec_length(vec_sub(Pos,PlayPos)),
        if
            Dist < ClosestDist and (Player#player.hp > 0) ->
                {Dist,Player};
            true ->
                {ClosestDist,ClosestPlayer}
        end end, {20.0, too_far}, Players ),
    Closest.


enemy_logic(Enemy=#bully{}, Hits, Players) ->
    Speed = 4.5,
    RunAnimState = get_animationstate(Enemy#bully.entity, 'Run'),
    IdleAnimState = get_animationstate(Enemy#bully.entity, 'Idle'),
    FuryAnimState = get_animationstate(Enemy#bully.entity, 'Fury'),
    CoversAnimState = get_animationstate(Enemy#bully.entity, 'Cover'),
    Node = Enemy#bully.node,
    Body = Enemy#bully.body,
    CurrentVelocity = bullet:btRigidBody_getLinearVelocity(Body),
    Pos = get_node_position(Node),
    {NewActions,NewHits} = case Enemy#bully.covers of
        true ->
            set_animationstate_enabled(IdleAnimState, 0),
            set_animationstate_enabled(RunAnimState, 0),
            set_animationstate_enabled(FuryAnimState, 0),
            set_animationstate_enabled(CoversAnimState, 1),
            set_animationstate_loop(CoversAnimState, 0),
            bullet:btRigidBody_setLinearVelocity(Body, {0.0, vec_y(CurrentVelocity) - 0.41, 0.0}),
            add_animationstate_time(CoversAnimState, 0.00666),
            {[], ignore};
        false ->
            set_animationstate_enabled(IdleAnimState, 1),
            set_animationstate_enabled(RunAnimState, 0),
            set_animationstate_enabled(FuryAnimState, 0),
            set_animationstate_enabled(CoversAnimState,0 ),
            add_animationstate_time(IdleAnimState, 0.01666),
            case Enemy#bully.actions of 
                [] ->
                    Closest = findClosestPlayer(Pos,Players),
                    case Closest of
                        too_far -> {Enemy#bully.idle, ignore};
                        Player ->
                            {[{go,get_node_position(Player#player.node),600},{attack,90}], ignore}
                    end;
                [{go,Destination,Frames}|Rest]->
                    Direction = vec_sub(Destination,Pos),
                    Dist = vec_length(Direction),
                    if
                        Frames == 1 ->
                            {[], ignore};
                        Dist < 0.5 ->
                            {Rest, ignore};
                        true ->
                            set_animationstate_enabled(IdleAnimState, 0),
                            set_animationstate_enabled(RunAnimState, 1),
                            set_animationstate_enabled(FuryAnimState, 0),
                            set_animationstate_enabled(CoversAnimState,0 ),
                            add_animationstate_time(RunAnimState, 0.03666),
                            DirectionNorm = vec_normalize(Direction), 
                            set_node_orientation(Node, get_rotation_to({0.0,0.0,1.0}, {vec_x(DirectionNorm),0.0,vec_z(DirectionNorm)})),
                            DesiredVelocity = vec_mult_scalar(DirectionNorm,Speed),
                            bullet:btRigidBody_setLinearVelocity(Body, {vec_x(DesiredVelocity), vec_y(CurrentVelocity) - 0.41, vec_z(DesiredVelocity)}),
                            {[{go,Destination,Frames-1}|Rest], ignore}
                    end;
                [{attack,Frames}|Rest] ->
                    case Frames of
                        1 ->
                            {Rest, ignore};
                        _ ->
                            set_animationstate_enabled(IdleAnimState, 0),
                            set_animationstate_enabled(RunAnimState, 0),
                            set_animationstate_enabled(FuryAnimState, 1),
                            set_animationstate_enabled(CoversAnimState,0 ),
                            add_animationstate_time(FuryAnimState, 0.01666),
                            {[{attack,Frames-1}|Rest], Pos}
                    end
            end
    end,
    GotHit = lists:foldl(fun ({player_attack, HitPos},GotHit) ->
        Dist2 = vec_length(vec_sub(HitPos, Pos)),
        if 
            Dist2 < 2.0 ->
                Direction2 = vec_sub(Pos, HitPos), 
                case Enemy#bully.covers of 
                    false -> bullet:btRigidBody_applyCentralImpulse(Body, {vec_x(Direction2)*200.0, 100.0, vec_z(Direction2)*200.0});
                    true -> ok
                end,
                true;
            true ->
                GotHit
        end
        end, false, Hits),

    Enemy0 = case GotHit of
        true ->
            case Enemy#bully.hp of
                1 -> Enemy#bully{covers=true};
                _ -> Enemy#bully{hp=Enemy#bully.hp-1}
            end;
        false -> Enemy
    end,
    {Enemy0#bully{actions=NewActions}, NewHits}.


player_logic(Player, EnemyHits) ->
    Speed = 5.0,
    LeftRotation = get_rotation_to({0.0, 0.0, 1.0}, {0.04, 0.0, 1.0}),
    RightRotation = get_rotation_to({0.0, 0.0, 1.0}, {-0.04, 0.0, 1.0}),
    RunAnimState = get_animationstate(Player#player.entity, 'Run'),
    IdleAnimState = get_animationstate(Player#player.entity, 'Idle'),
    CoversAnimState = get_animationstate(Player#player.entity, 'Cover'),
    Melee1AnimState = get_animationstate(Player#player.entity, 'MeeleHit1'),
    Node = Player#player.node,
    Body = Player#player.body,
    CurrentVelocity = bullet:btRigidBody_getLinearVelocity(Body),
    Pos = get_node_position(Node),
    HitCount = length(lists:filter(fun(Hit) ->
        HitDist = vec_length(vec_sub(Pos,Hit)),
        HitDist < 2.0 end, EnemyHits)),

    if 
        Player#player.hp < 0 ->
            set_animationstate_enabled(IdleAnimState, 0),
            set_animationstate_enabled(RunAnimState, 0),
            set_animationstate_enabled(CoversAnimState, 1),
            set_animationstate_enabled(Melee1AnimState, 0),
            set_animationstate_loop(CoversAnimState, 0),
            add_animationstate_time(CoversAnimState, 0.01666),
            { Player, ignore };
        true ->        
            case Player#player.attacks of
                true ->
                    set_animationstate_enabled(IdleAnimState, 0),
                    set_animationstate_enabled(RunAnimState, 0),
                    set_animationstate_enabled(CoversAnimState, 0),
                    set_animationstate_enabled(Melee1AnimState, 1),
                    add_animationstate_time(Melee1AnimState, 0.04666);
                false -> 
                    case Player#player.upDown of
                        true -> 
                            Forward = vec_mult_quat({0.0, vec_y(CurrentVelocity) - 0.41, Speed}, get_node_orientation(Node)),
                            bullet:btRigidBody_setLinearVelocity(Body, Forward),
                            bullet:btRigidBody_setFriction(Body, 0.0),
                            set_animationstate_enabled(IdleAnimState, 0),
                            set_animationstate_enabled(RunAnimState, 1),
                            set_animationstate_enabled(CoversAnimState, 0),
                            set_animationstate_enabled(Melee1AnimState, 0),
                            add_animationstate_time(RunAnimState, 0.04666);
                        false ->
                            bullet:btRigidBody_setLinearVelocity(Body, {0.0, vec_y(CurrentVelocity) - 0.41, 0.0}),
                            bullet:btRigidBody_setFriction(Body, 1.0),
                            set_animationstate_enabled(IdleAnimState, 1),
                            set_animationstate_enabled(RunAnimState, 0),
                            set_animationstate_enabled(CoversAnimState, 0),
                            set_animationstate_enabled(Melee1AnimState, 0),
                            add_animationstate_time(IdleAnimState, 0.02666),
                            ok
                    end
            end,
            case Player#player.leftDown of
                true -> rotate_node(Player#player.node,LeftRotation);
                false -> ok
            end,
            case Player#player.rightDown of
                true -> rotate_node(Player#player.node,RightRotation);
                false -> ok
            end,
            { Player0, Attacks } = case Player#player.attacks of
                true ->
                    AnimTime = get_animationstate_time(Melee1AnimState),
                    if
                        AnimTime > 1.03 ->
                            set_animationstate_time(Melee1AnimState, 0.0),
                            { Player#player{attacks=false}, {player_attack, Pos} };
                        true ->
                            { Player, ignore }
                    end;
                false ->
                    { Player, ignore }
            end,
            { Player0#player{hp=Player#player.hp-HitCount}, Attacks}
    end.
 

vec_x({V,_,_}) -> V.
vec_y({_,V,_}) -> V.
vec_z({_,_,V}) -> V.

vec_normalize({X,Y,Z}) ->
    Len = vec_length({X,Y,Z}),
    {X/Len,Y/Len,Z/Len}.

vec_mult_scalar({X,Y,Z},S) -> {X*S,Y*S,Z*S}.

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


rotate_node(Node, By) ->
    CurrentOrientation = get_node_orientation(Node),
    NewOrientation = quat_mult_quat(CurrentOrientation, By),
    set_node_orientation(Node, NewOrientation).

find_localplayer(Players,LocalPlayerID) ->
    [LocalPlayer] = lists:filter((fun(Player) -> ID = Player#player.id, ID == LocalPlayerID end), Players),
    LocalPlayer.


get_all_positions(p0, Players, Enemies) ->
    {
    lists:map(fun(Player) ->
            Body = Player#player.body,
            Node = Player#player.node,
            Position = bullet:btRigidBody_getCenterOfMassPosition(Body),
            Orientation = get_node_orientation(Node),
            {Position,Orientation}
            end, Players)
    ,
    lists:map(fun(Enemy) ->
            Body = Enemy#bully.body,
            Node = Enemy#bully.node,
            Position = bullet:btRigidBody_getCenterOfMassPosition(Body),
            Orientation = get_node_orientation(Node),
            {Position,Orientation}
            end, Enemies)
    };
get_all_positions(_, _, _) ->  ignore.

set_all_positions(Players, Enemies, {PlPos, EnPos}) ->
    lists:foreach(fun({Player,{Position,Orientation}}) ->
            Node = Player#player.node,
            set_node_position(Node, vec_sub(Position,{0.0,1.0,0.0})),
            set_node_orientation(Node,Orientation)
            end, lists:zip(Players,PlPos)),
    lists:foreach(fun({Enemy,{Position,Orientation}}) ->
            Node = Enemy#bully.node,
            set_node_position(Node, vec_sub(Position,{0.0,0.9,0.0})),
            set_node_orientation(Node,Orientation)
            end, lists:zip(Enemies,EnPos)).



play_loop(Frame,LocalPlayerID,Players,Enemies,InputState,Clients,Console,BulletWorld,PreviousHits) ->
    capture_input(),
    bullet:btDynamicsWorld_stepSimulation(BulletWorld),
    render_frame(),

    {NewInputState,Input} = handle_input(InputState),

    OutPositions = get_all_positions(LocalPlayerID, Players, Enemies),

    Sending = {frameDone,LocalPlayerID,Frame,Input,OutPositions},

    send_to_clients(Clients,Sending),
    NewPlayers = lists:map(fun (Player) ->
        ID = Player#player.id,
        receive 
            {frameDone,ID,Frame,Input2,InPositions} -> log("player ~p recieved ~p", [Player, Input2]),
            case InPositions of
                ignore -> ok;
                _ -> set_all_positions(Players, Enemies, InPositions)
            end,
            handle_player(Player,Input2)
        after 2000 -> halt()
        end
    end,Players),

    {NewPlayers2,Hits} = lists:unzip(lists:map(fun(Player) -> player_logic(Player,PreviousHits) end,NewPlayers)),
    RealHits = lists:filter(fun (Hit) -> 
        case Hit of
            ignore -> false;
            _ -> true
        end end, Hits),
    
    {NewEnemies,EnemyHits} = lists:unzip(lists:map(fun (Enemy) -> enemy_logic(Enemy,RealHits,NewPlayers2) end,Enemies)),
    RealEnemyHits = lists:filter(fun (Hit) -> 
        case Hit of
            ignore -> false;
            _ -> true
        end end, EnemyHits),
    LocalPlayer = find_localplayer(NewPlayers2,LocalPlayerID),


    LPNode = LocalPlayer#player.node,
    {X,Y,Z} = get_node_position(LPNode),
    NewConsole = Console,
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
            play_loop(Frame+1,LocalPlayerID,NewPlayers2,NewEnemies,NewInputState,Clients,NewConsole,BulletWorld,RealEnemyHits);
        true -> halt
    end.

