#include <stdio.h>
extern "C" {
#include "erl_driver.h"
#include "erl_nif.h"
}
#include <OGRE/Ogre.h>
#include <OIS/OIS.h>

using namespace Ogre;

Root* root;
RenderWindow* window;
SceneManager* sceneMgr;
Camera* camera;
Viewport* viewPort;
OIS::Keyboard* keyboard;
OIS::Mouse*     mouse;
Real timeDelta;
Real timeStep;
OIS::InputManager* inputManager;

static ErlNifResourceType* node_resource;
static ErlNifResourceType* entity_resource;

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM init_ogre(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    root = new Root;

    ConfigFile cf;
    cf.load("resources.cfg");

    ConfigFile::SectionIterator seci = cf.getSectionIterator();
    String secName, typeName, archName;
    while (seci.hasMoreElements()) {
        secName = seci.peekNextKey();
        ConfigFile::SettingsMultiMap *settings = seci.getNext();
        ConfigFile::SettingsMultiMap::iterator i;
        for (i = settings->begin(); i != settings->end(); ++i) {
            typeName = i->first;
            archName = i->second;
            ResourceGroupManager::getSingleton().addResourceLocation(
                archName, typeName, secName);
        }
    }

    /* To dziadowskie okienko mozna zastapic pozniej ustawieniami z pliku */
    if(!root->showConfigDialog()) {
        delete root;
        return enif_make_atom(env, "not ok");
    }

    window = root->initialise(true);

    ResourceGroupManager::getSingleton().initialiseAllResourceGroups();

    sceneMgr = root->createSceneManager(ST_GENERIC);
    camera = sceneMgr->createCamera("MainCamera");
    viewPort = window->addViewport(camera);

    OIS::ParamList pl;
    size_t windowHnd = 0;
    std::ostringstream windowHndStr;

    window->getCustomAttribute("WINDOW", &windowHnd);
    windowHndStr << windowHnd;
    pl.insert(std::make_pair(std::string("WINDOW"), windowHndStr.str()));

    inputManager = OIS::InputManager::createInputSystem(pl);
    keyboard = static_cast<OIS::Keyboard*>(inputManager->createInputObject(OIS::OISKeyboard, true));
    mouse = static_cast<OIS::Mouse*>(inputManager->createInputObject(OIS::OISMouse, true));

    camera->setPosition(Vector3(0,0,0));
    camera->lookAt(Vector3(0,0,15));
    camera->setNearClipDistance(0.1);
    camera->setFarClipDistance(1000);
    sceneMgr->setAmbientLight(ColourValue(1,1,1,1));
    viewPort->setBackgroundColour(ColourValue(1,0,0));
    camera->setAspectRatio(4.0/3.0);

    unsigned int width, height, depth;
    int top, left;
    window->getMetrics(width, height, depth, left, top);
    const OIS::MouseState &ms = mouse->getMouseState(); ms.width = width; ms.height = height;
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM capture_input(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    keyboard->capture();
    mouse->capture();
    return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM render_frame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    root->renderOneFrame();
    window->update();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM destroy_ogre(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    inputManager->destroyInputObject(mouse); mouse = 0;
    inputManager->destroyInputObject(keyboard); keyboard = 0;
    OIS::InputManager::destroyInputSystem(inputManager); inputManager = 0;
    delete root;
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM key_down(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int key;
    enif_get_int(env,argv[0],&key);
    if (keyboard->isKeyDown((OIS::KeyCode)key)) return enif_make_atom(env,"true");
    return enif_make_atom(env,"false");
}

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

static ERL_NIF_TERM create_scenenode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    void *node = (void*)(sceneMgr->getRootSceneNode()->createChildSceneNode());
    return wrap_pointer(env,node_resource,node) ;
}
static ERL_NIF_TERM create_entity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char meshName[200];
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    enif_get_atom(env,argv[1],meshName,200);
    Entity *entity = sceneMgr->createEntity(meshName);
    node->attachObject(entity);
    return wrap_pointer(env,entity_resource,(void*)entity);
}

static ERL_NIF_TERM set_node_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double x,y,z;
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    enif_get_double(env, argv[1], &x);
    enif_get_double(env, argv[2], &y);
    enif_get_double(env, argv[3], &z);
    node->setPosition(Vector3(x,y,z));
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM set_node_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double w,x,y,z;
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    enif_get_double(env, argv[1], &x);
    enif_get_double(env, argv[2], &x);
    enif_get_double(env, argv[3], &y);
    enif_get_double(env, argv[4], &z);
    node->setOrientation(Quaternion(w,x,y,z));
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM get_node_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    Vector3 p = node->getPosition();
    return enif_make_tuple3(env, enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM get_node_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    Quaternion p = node->getOrientation();
    return enif_make_tuple4(env, enif_make_double(env,p.w), enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM get_average_fps(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_double(env, window->getAverageFPS());
}

static ErlNifFunc nif_funcs[] =
{
    {"init_ogre", 0, init_ogre},
    {"capture_input", 0, capture_input},
    {"render_frame", 0, render_frame},
    {"destroy_ogre", 0, destroy_ogre},
    {"key_down", 1, key_down},
    {"create_entity", 2, create_entity},
    {"create_scenenode", 0, create_scenenode},
    {"set_node_position", 4, set_node_position},
    {"set_node_orientation", 5, set_node_position},
    {"get_node_position", 1, get_node_position},
    {"get_node_orientation", 1, get_node_orientation},
    {"get_average_fps", 0, get_average_fps}
};
static int load(ErlNifEnv* env,void** priv_data,ERL_NIF_TERM load_info) {
    node_resource = enif_open_resource_type(env,"Ogre Node",NULL,ERL_NIF_RT_CREATE,NULL);
    entity_resource = enif_open_resource_type(env,"Ogre Entity",NULL,ERL_NIF_RT_CREATE,NULL);
    return 0;
}
extern "C" {
ERL_NIF_INIT(ogre,nif_funcs,load,NULL,NULL,NULL)
}
