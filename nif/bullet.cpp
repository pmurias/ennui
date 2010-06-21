#include <stdio.h>
extern "C" {
#include "erl_driver.h"
#include "erl_nif.h"
}

#include <btBulletDynamicsCommon.h>


static ErlNifResourceType* btBroadphaseInterface_resource;
static ErlNifResourceType* btDefaultCollisionConfiguration_resource;
static ErlNifResourceType* btCollisionDispatcher_resource;
static ErlNifResourceType* btSequentialImpulseConstraintSolver_resource;
static ErlNifResourceType* btDynamicsWorld_resource;



/*
static Vector3 get_vector(ErlNifEnv *env, const ERL_NIF_TERM *arg) {
    double x,y,z,w;
    const ERL_NIF_TERM *tuple;
    int arity;
    enif_get_tuple(env, *arg, &arity, &tuple);
    enif_get_double(env, tuple[0], &x);
    enif_get_double(env, tuple[1], &y);
    enif_get_double(env, tuple[2], &z);
    return Vector3(x,y,z);
}
*/

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




static ErlNifFunc nif_funcs[] =
{
    {"new_btDbvtBroadphase", 0, new_btDbvtBroadphase},
    {"new_btDefaultCollisionConfiguration", 0, new_btDefaultCollisionConfiguration},
    {"new_btCollisionDispatcher", 0, new_btCollisionDispatcher},
    {"new_btSequentialImpulseConstraintSolver", 0, new_btSequentialImpulseConstraintSolver},
    {"new_btDiscreteDynamicsWorld", 4, new_btDiscreteDynamicsWorld},

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


    return 0;
}

extern "C" {
ERL_NIF_INIT(bullet,nif_funcs,load,NULL,NULL,NULL)
}
