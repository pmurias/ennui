#include <stdio.h>
extern "C" {
#include "erl_driver.h"
#include "erl_nif.h"
}
#include <OGRE/Ogre.h>
#include <OIS/OIS.h>
#include <OGRE/OgreTextAreaOverlayElement.h>

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
static ErlNifResourceType* animationstate_resource;
static ErlNifResourceType* overlay_resource;
static ErlNifResourceType* overlaycontainer_resource;

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static Quaternion get_quaternion(ErlNifEnv *env, const ERL_NIF_TERM *arg) {
    double x,y,z,w;
    const ERL_NIF_TERM *tuple;
    int arity;
    enif_get_tuple(env, *arg, &arity, &tuple);
    enif_get_double(env, tuple[0], &w);
    enif_get_double(env, tuple[1], &x);
    enif_get_double(env, tuple[2], &y);
    enif_get_double(env, tuple[3], &z);
    return Quaternion(w,x,y,z);
}

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

static ERL_NIF_TERM init_ogre(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    LogManager *lm = new LogManager();
    lm->createLog("./Ogre.log0", true, false, false);
    root = new Root("plugins.cfg", "ogre.cfg", "");

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
    if (!root->restoreConfig()) {
        if(!root->showConfigDialog()) {
            delete root;
            return enif_make_atom(env, "not ok");
        }
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

    camera->setNearClipDistance(0.1);
    camera->setFarClipDistance(1000);
    viewPort->setBackgroundColour(ColourValue(0.3,0.6,1.0));
    camera->setAspectRatio(4.0/3.0);
    sceneMgr->setShadowTechnique(SHADOWTYPE_STENCIL_ADDITIVE);

    Light *light = sceneMgr->createLight();
    light->setType(Light::LT_DIRECTIONAL);
    light->setDiffuseColour(ColourValue(1,1,1));
    light->setSpecularColour(ColourValue(0.2,0.2,0.2));
    light->setDirection(Vector3(1,-1,1));

    unsigned int width, height, depth;
    int top, left;
    window->getMetrics(width, height, depth, left, top);
    const OIS::MouseState &ms = mouse->getMouseState(); ms.width = width; ms.height = height;

    Ogre::CompositorManager::getSingleton().initialise();

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
    return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM create_entity(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char meshName[200];
    enif_get_atom(env,argv[0],meshName,200);
    Entity *entity = sceneMgr->createEntity(meshName);
    entity->setCastShadows(true);
    return wrap_pointer(env,entity_resource,(void*)entity);
}

static ERL_NIF_TERM set_node_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    node->setPosition(get_vector(env, &argv[1]));
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM attach_entity_to_node(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Entity *entity = (Entity *)unwrap_pointer(env,entity_resource,argv[0]);
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[1]);
    node->attachObject(entity);
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM attach_entity_to_bone(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Entity *parent = (Entity *)unwrap_pointer(env,entity_resource,argv[0]);
    Entity *entity = (Entity *)unwrap_pointer(env,entity_resource,argv[1]);
    char boneName[200];
    enif_get_atom(env,argv[2],boneName,200);
    parent->attachObjectToBone(boneName,entity);
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM set_camera_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    camera->setPosition(get_vector(env, &argv[0]));
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM set_node_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    node->setOrientation(get_quaternion(env, &argv[1]));
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM set_camera_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    camera->setOrientation(get_quaternion(env, &argv[0]));
    return enif_make_atom(env,"ok");
}

static ERL_NIF_TERM get_node_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    Vector3 p = node->getPosition();
    return enif_make_tuple3(env, enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM get_camera_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Vector3 p = camera->getPosition();
    return enif_make_tuple3(env, enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM get_node_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    SceneNode *node = (SceneNode *)unwrap_pointer(env,node_resource,argv[0]);
    Quaternion p = node->getOrientation();
    return enif_make_tuple4(env, enif_make_double(env,p.w), enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM get_camera_orientation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Quaternion p = camera->getOrientation();
    return enif_make_tuple4(env, enif_make_double(env,p.w), enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM get_rotation_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Quaternion p = get_vector(env,&argv[0]).getRotationTo(get_vector(env,&argv[1]));
    return enif_make_tuple4(env, enif_make_double(env,p.w), enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM mult_quaternion_quaternion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Quaternion p = get_quaternion(env, &argv[0]) * get_quaternion(env, &argv[1]);
    return enif_make_tuple4(env, enif_make_double(env,p.w), enif_make_double(env, p.x), enif_make_double(env, p.y), enif_make_double(env,p.z));
}

static ERL_NIF_TERM mult_quaternion_vector(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Vector3 p = get_quaternion(env, &argv[0]) * get_vector(env, &argv[1]);
    return enif_make_tuple3(env, enif_make_double(env,p.x), enif_make_double(env, p.y), enif_make_double(env, p.z));
}

static ERL_NIF_TERM get_quaternion_inverse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Quaternion p = get_quaternion(env, &argv[0]).Inverse();
    return enif_make_tuple4(env, enif_make_double(env, p.w), enif_make_double(env,p.x), enif_make_double(env, p.y), enif_make_double(env, p.z));
}
static ERL_NIF_TERM get_average_fps(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_double(env, window->getAverageFPS());
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM get_animationstate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char animName[200];
    Entity *entity = (Entity *)unwrap_pointer(env,entity_resource,argv[0]);
    enif_get_atom(env,argv[1], animName,200);
    AnimationState *animationState = entity->getAnimationState(animName);
    return wrap_pointer(env,animationstate_resource,(void*)animationState);
}

static ERL_NIF_TERM set_animationstate_enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    AnimationState *as = (AnimationState *)unwrap_pointer(env,animationstate_resource,argv[0]);
    int enabled;
    enif_get_int(env,argv[1], &enabled);
    as->setEnabled((bool)(enabled));
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_animationstate_loop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    AnimationState *as = (AnimationState *)unwrap_pointer(env,animationstate_resource,argv[0]);
    int enabled;
    enif_get_int(env,argv[1], &enabled);
    as->setLoop((bool)(enabled));
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM add_animationstate_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    AnimationState *as = (AnimationState *)unwrap_pointer(env,animationstate_resource,argv[0]);
    double time;
    enif_get_double(env,argv[1], &time);
    as->addTime(time);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_ambient_light(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Vector3 c = get_vector(env, &argv[0]);
    sceneMgr->setAmbientLight(ColourValue(c.x,c.y,c.z));
    return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM create_overlay(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char name[200];
    enif_get_atom(env,argv[0], name,200);
    Overlay *overlay = Ogre::OverlayManager::getSingletonPtr()->create(name);
    return wrap_pointer(env,overlay_resource,(void*)overlay);
}

static ERL_NIF_TERM create_overlay_container(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char name[200];
    char type[200];
    enif_get_atom(env,argv[0], type,200);
    enif_get_atom(env,argv[1], name,200);
    OverlayElement *con = (Ogre::OverlayManager::getSingletonPtr()->createOverlayElement(type,name));
    return wrap_pointer(env,overlaycontainer_resource,(void*)con);
}

static ERL_NIF_TERM set_overlay_container_dimensions(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *con = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    double x, y;
    enif_get_double(env, argv[1], &x);
    enif_get_double(env, argv[2], &y);
    con->setDimensions(x, y);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_overlay_container_position(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *con = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    double x, y;
    enif_get_double(env, argv[1], &x);
    enif_get_double(env, argv[2], &y);
    con->setPosition(x, y);
    return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM add_overlay_container(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Overlay *overlay = (Overlay*)unwrap_pointer(env,overlay_resource,argv[0]);
    OverlayContainer *con = (OverlayContainer*)unwrap_pointer(env,overlaycontainer_resource,argv[1]);
    overlay->add2D(con);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM show_overlay(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Overlay *overlay = (Overlay*)unwrap_pointer(env,overlay_resource,argv[0]);
    overlay->show();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_overlay_element_metrics_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *elem = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    int metrics;
    enif_get_int(env, argv[1], &metrics);
    elem->setMetricsMode((Ogre::GMM_PIXELS));
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_overlay_element_width(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *elem = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    double x;
    enif_get_double(env, argv[1], &x);
    elem->setWidth(x);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_overlay_element_height(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *elem = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    double x;
    enif_get_double(env, argv[1], &x);
    elem->setHeight(x);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_overlay_element_parameter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *elem = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    char pname[200];
    char pval[200];
    enif_get_atom(env,argv[1], pname,200);
    enif_get_atom(env,argv[2], pval,200);
    elem->setParameter(pname, pval);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM set_overlay_element_fontname(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TextAreaOverlayElement *elem = (TextAreaOverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    char pname[200];
    enif_get_atom(env,argv[1], pname,200);
    elem->setFontName(pname);
    return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM set_overlay_element_colour(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *elem = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    Vector3 v = get_vector(env, &argv[1]);
    elem->setColour(ColourValue(v.x, v.y, v.z));
    return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM set_overlay_element_caption(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayElement *elem = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    char pval[4096];
    enif_get_atom(env,argv[1],pval,4096);
    elem->setCaption(pval);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM add_overlay_container_child(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    OverlayContainer *paren = (OverlayContainer*)unwrap_pointer(env,overlaycontainer_resource,argv[0]);
    OverlayElement *chld = (OverlayElement*)unwrap_pointer(env,overlaycontainer_resource,argv[1]);
    paren->addChild(chld);
    return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM log_message(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char message[4096];
    enif_get_atom(env,argv[0],message,4096);
    LogManager::getSingleton().getDefaultLog()->logMessage(message);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM add_compositor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char name[255];
    enif_get_atom(env,argv[0],name,255);
    printf("Add Compositor: %s\n", name);
    Ogre::CompositorManager::getSingleton().addCompositor(viewPort,name);
    Ogre::CompositorManager::getSingleton().setCompositorEnabled(viewPort, name, true);
    return enif_make_atom(env, "ok");
}


static ErlNifFunc nif_funcs[] =
{
    {"init_ogre", 0, init_ogre},
    {"capture_input", 0, capture_input},
    {"render_frame", 0, render_frame},
    {"destroy_ogre", 0, destroy_ogre},
    {"key_down", 1, key_down},
    {"create_entity", 1, create_entity},
    {"create_scenenode", 0, create_scenenode},
    {"attach_entity_to_node", 2, attach_entity_to_node},
    {"set_node_position", 2, set_node_position},
    {"set_node_orientation", 2, set_node_orientation},
    {"get_node_position", 1, get_node_position},
    {"get_node_orientation", 1, get_node_orientation},
    {"get_average_fps", 0, get_average_fps},
    {"log_message", 1, log_message},
    {"set_camera_position", 1, set_camera_position},
    {"set_camera_orientation", 1, set_camera_orientation},
    {"get_camera_position", 0, get_camera_position},
    {"get_camera_orientation", 0, get_camera_orientation},
    {"get_rotation_to", 2, get_rotation_to},
    {"mult_quaternion_quaternion", 2, mult_quaternion_quaternion},
    {"mult_quaternion_vector", 2, mult_quaternion_vector},
    {"get_quaternion_inverse", 1, get_quaternion_inverse},
    {"get_animationstate", 2, get_animationstate},
    {"set_animationstate_enabled", 2, set_animationstate_enabled},
    {"set_animationstate_loop", 2, set_animationstate_loop},
    {"add_animationstate_time", 2, add_animationstate_time},
    {"set_ambient_light", 1, set_ambient_light},
    {"attach_entity_to_bone", 3, attach_entity_to_bone},
    {"create_overlay", 1, create_overlay},
    {"create_overlay_container", 2, create_overlay_container},
    {"set_overlay_container_dimensions", 3, set_overlay_container_dimensions},
    {"set_overlay_container_position", 3, set_overlay_container_position},
    {"add_overlay_container", 2, add_overlay_container},
    {"show_overlay", 1, show_overlay},
    {"set_overlay_element_metrics_mode", 2, set_overlay_element_metrics_mode},
    {"set_overlay_element_width", 2, set_overlay_element_width},
    {"set_overlay_element_height", 2, set_overlay_element_height},
    {"set_overlay_element_parameter", 3, set_overlay_element_parameter},
    {"set_overlay_element_colour", 2, set_overlay_element_colour},
    {"set_overlay_element_caption", 2, set_overlay_element_caption},
    {"add_overlay_container_child", 2, add_overlay_container_child},
    {"set_overlay_element_fontname", 2, set_overlay_element_fontname},
    {"add_compositor", 1, add_compositor}
};

static int load(ErlNifEnv* env,void** priv_data,ERL_NIF_TERM load_info) {
    node_resource = enif_open_resource_type(env,"Ogre Node",NULL,ERL_NIF_RT_CREATE,NULL);
    entity_resource = enif_open_resource_type(env,"Ogre Entity",NULL,ERL_NIF_RT_CREATE,NULL);
    animationstate_resource = enif_open_resource_type(env,"Ogre AnimationState",NULL,ERL_NIF_RT_CREATE,NULL);
    overlay_resource = enif_open_resource_type(env,"Ogre Overlay",NULL,ERL_NIF_RT_CREATE,NULL);
    overlaycontainer_resource = enif_open_resource_type(env,"Ogre OverlayContainer",NULL,ERL_NIF_RT_CREATE,NULL);
    return 0;
}

extern "C" {
ERL_NIF_INIT(ogre,nif_funcs,load,NULL,NULL,NULL)
}
