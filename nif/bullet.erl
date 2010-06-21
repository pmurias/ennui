-module(bullet).
-export([
    new_btDbvtBroadphase/0,
    new_btDefaultCollisionConfiguration/0,
    new_btCollisionDispatcher/0,
    new_btSequentialImpulseConstraintSolver/0,
    new_btDiscreteDynamicsWorld/4,
    btDynamicsWorld_setGravity/2
]).
-on_load(load_c_module/0).
load_c_module() ->
      erlang:load_nif("./bullet", 0).

new_btDbvtBroadphase() -> throw('nif library not loaded').
new_btDefaultCollisionConfiguration() -> throw('nif library not loaded').
new_btCollisionDispatcher() -> throw('nif library not loaded').
new_btSequentialImpulseConstraintSolver() -> throw('nif library not loaded').
new_btDiscreteDynamicsWorld(_,_,_,_) -> throw('nif library not loaded').
btDynamicsWorld_setGravity(_,_) -> throw('nif library not loaded').
