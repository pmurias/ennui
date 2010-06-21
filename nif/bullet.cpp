#include <stdio.h>
extern "C" {
#include "erl_driver.h"
#include "erl_nif.h"
}

#include <btBulletDynamicsCommon.h>
#include <btRigidBody.h>
#include <btCollisionShape.h>


static ErlNifResourceType* btBroadphaseInterface_resource;
static ErlNifResourceType* btDefaultCollisionConfiguration_resource;
static ErlNifResourceType* btCollisionDispatcher_resource;
static ErlNifResourceType* btSequentialImpulseConstraintSolver_resource;
static ErlNifResourceType* btDynamicsWorld_resource;
static ErlNifResourceType* btCollisionShape_resource;
static ErlNifResourceType* btMotionState_resource;
static ErlNifResourceType* btRigidBodyConstructionInfo_resource;
static ErlNifResourceType* btRigidBody_resource;




static ERL_NIF_TERM wrap_pointer(ErlNifEnv* env,ErlNifResourceType* type,void* ptr) {
    void** resource = (void**) enif_alloc_resource(env,type,sizeof(void*));
    *resource = ptr;
    ERL_NIF_TERM term = enif_make_resource(env,resource);
    enif_release_resource(env,(void*)resource);
    return term;
}
static void* unwrap_pointer(ErlNifEnv* env,ErlNifResourceType* type,ERL_NIF_TERM term) {
    void* ptr;
    enif_get_resource(env,term,type,&ptr);
    return *((void**)ptr);
}


static btVector3 get_vector(ErlNifEnv *env, const ERL_NIF_TERM arg) {
    double x,y,z;
    const ERL_NIF_TERM *tuple;
    int arity;
    enif_get_tuple(env, arg, &arity, &tuple);
    enif_get_double(env, tuple[0], &x);
    enif_get_double(env, tuple[1], &y);
    enif_get_double(env, tuple[2], &z);
    return btVector3(x,y,z);
}

static btQuaternion get_quaternion(ErlNifEnv *env, const ERL_NIF_TERM arg) {
    double x,y,z,w;
    const ERL_NIF_TERM *tuple;
    int arity;
    enif_get_tuple(env, arg, &arity, &tuple);
    enif_get_double(env, tuple[0], &x);
    enif_get_double(env, tuple[1], &y);
    enif_get_double(env, tuple[2], &z);
    enif_get_double(env, tuple[3], &w);
    return btQuaternion(x,y,z,w);
}
static btTransform get_transform(ErlNifEnv *env, const ERL_NIF_TERM arg) {
    double x,y,z,w;
    const ERL_NIF_TERM *tuple;
    int arity;
    enif_get_tuple(env, arg, &arity, &tuple);
    return btTransform(get_quaternion(env,tuple[0]),get_vector(env,tuple[1]));
}

static ERL_NIF_TERM new_btDbvtBroadphase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    return wrap_pointer(env,btBroadphaseInterface_resource,new btDbvtBroadphase());
}

static ERL_NIF_TERM new_btDefaultCollisionConfiguration(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    return wrap_pointer(env,btDefaultCollisionConfiguration_resource,new btDefaultCollisionConfiguration());
}

static ERL_NIF_TERM new_btCollisionDispatcher(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    return wrap_pointer(
        env,
        btCollisionDispatcher_resource,
        new btCollisionDispatcher(
            (btDefaultCollisionConfiguration*)unwrap_pointer(
                env,
                btDefaultCollisionConfiguration_resource,
                argv[0]
                )
        )
    );
}
static ERL_NIF_TERM new_btSequentialImpulseConstraintSolver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    return wrap_pointer(env,btSequentialImpulseConstraintSolver_resource,new btSequentialImpulseConstraintSolver());
}
static ERL_NIF_TERM new_btDiscreteDynamicsWorld(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    return wrap_pointer(env,
        btDynamicsWorld_resource,
        new btDiscreteDynamicsWorld(
            (btCollisionDispatcher*)unwrap_pointer(
                env,
                btCollisionDispatcher_resource,
                argv[0]
             ),
            (btBroadphaseInterface*)unwrap_pointer(
                env,
                btBroadphaseInterface_resource,
                argv[1]
            ),
            (btSequentialImpulseConstraintSolver*)unwrap_pointer(
                env,
                btSequentialImpulseConstraintSolver_resource,
                argv[2]
             ),
            (btDefaultCollisionConfiguration*)unwrap_pointer(
                env,
                btDefaultCollisionConfiguration_resource,
                argv[3]
             )
    ));
}

static ERL_NIF_TERM btDynamicsWorld_setGravity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ((btDynamicsWorld*)unwrap_pointer(
        env,
        btDynamicsWorld_resource,
        argv[0]
    ))->setGravity(get_vector(env,argv[1]));
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM new_btBoxShape(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return wrap_pointer(env,btCollisionShape_resource,(void*)new btBoxShape(get_vector(env,argv[0])));
}
static ERL_NIF_TERM new_btSphereShape(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double radius;
    enif_get_double(env, argv[0], &radius);
    return wrap_pointer(env,btCollisionShape_resource,(void*)new btSphereShape(radius));
}
static ERL_NIF_TERM new_btCylinderShape(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return wrap_pointer(env,btCollisionShape_resource,new btCylinderShape(get_vector(env,argv[0])));
}

static ERL_NIF_TERM new_btDefaultMotionState(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return wrap_pointer(env,btMotionState_resource,new btDefaultMotionState(get_transform(env,argv[0])));
}

static ERL_NIF_TERM vector_to_tuple(ErlNifEnv* env, btVector3 v) {
    return enif_make_tuple3(env, enif_make_double(env, v.getX()), enif_make_double(env, v.getY()), enif_make_double(env,v.getZ()));
}

static ERL_NIF_TERM btCollisionShape_calculateLocalInertia(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    btVector3 ret;
    double mass;
    enif_get_double(env, argv[1], &mass);
    ((btCollisionShape*)unwrap_pointer(env,btCollisionShape_resource,argv[0]))->calculateLocalInertia((btScalar)mass,ret);
    return vector_to_tuple(env,ret);
}

static ERL_NIF_TERM new_btRigidBodyConstructionInfo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double mass;
    enif_get_double(env, argv[0], &mass);
    return wrap_pointer(env,btRigidBodyConstructionInfo_resource, new btRigidBody::btRigidBodyConstructionInfo(
        mass,
        (btMotionState*)unwrap_pointer(env,btMotionState_resource,argv[1]),
        (btCollisionShape*)unwrap_pointer(env,btCollisionShape_resource,argv[2]),
        get_vector(env,argv[3])
        ));
}

static ERL_NIF_TERM new_btRigidBody(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double mass;
    enif_get_double(env, argv[0], &mass);
    return wrap_pointer(env,btRigidBodyConstructionInfo_resource, new btRigidBody::btRigidBodyConstructionInfo(
        mass,
        (btMotionState*)unwrap_pointer(env,btMotionState_resource,argv[1]),
        (btCollisionShape*)unwrap_pointer(env,btCollisionShape_resource,argv[2]),
        get_vector(env,argv[3])
        ));
}


static ErlNifFunc nif_funcs[] =
{

};


static int load(ErlNifEnv* env,void** priv_data,ERL_NIF_TERM load_info) {
    btBroadphaseInterface_resource = enif_open_resource_type(
        env,"btBroadphaseInterface",NULL,ERL_NIF_RT_CREATE,NULL
    );
    btDefaultCollisionConfiguration_resource = enif_open_resource_type(
        env,"btDefaultCollisionConfiguration",NULL,ERL_NIF_RT_CREATE,NULL
    );
    btCollisionDispatcher_resource = enif_open_resource_type(
        env,"btCollisionDispatcher",NULL,ERL_NIF_RT_CREATE,NULL
    );
    btSequentialImpulseConstraintSolver_resource = enif_open_resource_type(
        env,"btSequentialImpulseConstraintSolver",NULL,ERL_NIF_RT_CREATE,NULL
    );

    btDynamicsWorld_resource = enif_open_resource_type(
        env,"btDynamicsWorld",NULL,ERL_NIF_RT_CREATE,NULL
    );

    btCollisionShape_resource = enif_open_resource_type(
        env,"btCollisionShape",NULL,ERL_NIF_RT_CREATE,NULL
    );

    btMotionState_resource = enif_open_resource_type(
        env,"btMotionState",NULL,ERL_NIF_RT_CREATE,NULL
    );
    btRigidBodyConstructionInfo_resource = enif_open_resource_type(
        env,"btRigidBodyConstructionInfo",NULL,ERL_NIF_RT_CREATE,NULL
    );
    btRigidBody_resource = enif_open_resource_type(
        env,"btRigidBody",NULL,ERL_NIF_RT_CREATE,NULL
    );


    return 0;
}

extern "C" {
ERL_NIF_INIT(bullet,nif_funcs,load,NULL,NULL,NULL)
}
