-module(bullet).
-export([
new_btDbvtBroadphase/0
,new_btDefaultCollisionConfiguration/0
,new_btCollisionDispatcher/0
,new_btSequentialImpulseConstraintSolver/0
,new_btDiscreteDynamicsWorld/4
,btDynamicsWorld_setGravity/2
,new_btBoxShape/1
,new_btSphereShape/1
,new_btCylinderShape/1
,new_btDefaultMotionState/1
,btCollisionShape_calculateLocalInertia/2
,new_btRigidBodyConstructionInfo/4
,new_btRigidBody/1
,btRigidBody_setDamping/3
,btRigidBody_setFriction/2
,btDynamicsWorld_addRigidBody/2
,btDynamicsWorld_removeRigidBody/2
,btRigidBody_getCenterOfMassPosition/1
,btRigidBody_getOrientation/1
,btRigidBody_translate/2
,btRigidBody_setWorldTransform/2
,btRigidBody_setAngularFactor/2
,btRigidBody_setActivationState/2

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
new_btBoxShape(_) -> throw('nif library not loaded').
new_btSphereShape(_) -> throw('nif library not loaded').
new_btCylinderShape(_) -> throw('nif library not loaded').
new_btDefaultMotionState(_) -> throw('nif library not loaded').
btCollisionShape_calculateLocalInertia(_,_) -> throw('nif library not loaded').
new_btRigidBodyConstructionInfo(_,_,_,_) -> throw('nif library not loaded').
new_btRigidBody(_) -> throw('nif library not loaded').
btRigidBody_setDamping(_,_,_) -> throw('nif library not loaded').
btRigidBody_setFriction(_,_) -> throw('nif library not loaded').
btDynamicsWorld_addRigidBody(_,_) -> throw('nif library not loaded').
btDynamicsWorld_removeRigidBody(_,_) -> throw('nif library not loaded').
btRigidBody_getCenterOfMassPosition(_) -> throw('nif library not loaded').
btRigidBody_getOrientation(_) -> throw('nif library not loaded').
btRigidBody_translate(_,_) -> throw('nif library not loaded').
btRigidBody_setWorldTransform(_,_) -> throw('nif library not loaded').
btRigidBody_setAngularFactor(_,_) -> throw('nif library not loaded').
btRigidBody_setActivationState(_,_) -> throw('nif library not loaded').
