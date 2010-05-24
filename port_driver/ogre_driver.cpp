/* port_driver.c */
#include <stdio.h>
#include "erl_driver.h"
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

int is_key_down(int key) {
    if (keyboard->isKeyDown((OIS::KeyCode)key)) return 1;
    return 0;
}
float get_timer(void) {
    return (float)root->getTimer()->getMilliseconds();
}

void input_capture(void) {
    keyboard->capture();
    mouse->capture();
}
void render_frame(void) {
    root->renderOneFrame();
    window->update();
}

int create_scenenode() {
    SceneNode *node = sceneMgr->getRootSceneNode()->createChildSceneNode();
    return (int)(node);
}

int create_entity(int nodePtr, char *meshName) {
    SceneNode *node = (SceneNode *)(nodePtr);
    Entity *entity = sceneMgr->createEntity(meshName);
    node->attachObject(entity);
    return (int)(entity);
}

void set_node_position(int nodePtr, float x, float y, float z) {
    SceneNode *node = (SceneNode *)(nodePtr);
    node->setPosition(Vector3(x,y,z));
}

void init_ogre(void) {
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
        return;
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
    viewPort->setBackgroundColour(ColourValue(0.1,0,0.02));
    camera->setAspectRatio(4.0/3.0);

    unsigned int width, height, depth;
    int top, left;
    window->getMetrics(width, height, depth, left, top);
    const OIS::MouseState &ms = mouse->getMouseState(); ms.width = width; ms.height = height;
}
void destroy_ogre(void) {
    inputManager->destroyInputObject(mouse); mouse = 0;
    inputManager->destroyInputObject(keyboard); keyboard = 0;
    OIS::InputManager::destroyInputSystem(inputManager); inputManager = 0;
    delete root;
}

typedef struct {
    ErlDrvPort port;
} ogre_data;

static ErlDrvData start(ErlDrvPort port, char *buff)
{
    ogre_data* d = (ogre_data*)driver_alloc(sizeof(ogre_data));
    d->port = port;
    return (ErlDrvData)d;
}
static void stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void process(ErlDrvData handle, ErlIOVec *ev) {
    ogre_data* driver_data = (ogre_data*) handle;
    ErlDrvBinary* data = ev->binv[1];

    ErlDrvTermData ok_spec[] = {ERL_DRV_ATOM, driver_mk_atom("ok")};

    switch (data->orig_bytes[0]) {
        case 1:
            init_ogre(); 
            break;
        case 2:
            destroy_ogre();
            break;
        case 3:
            render_frame();
            break;
        case 4:
            input_capture();
            break;
        case 5:
            {
                int key = data->orig_bytes[1];
                int down = is_key_down(key);
                ErlDrvTermData spec[] = {ERL_DRV_INT, down};
                driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
            }
            return;
        case 6:
            {
                int sceneNodePtr = create_scenenode();
                ErlDrvTermData spec[] = {ERL_DRV_INT, sceneNodePtr};
                driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
            }
            return;
        case 7:
            {
                int nodePtr = *((int*)(data->orig_bytes+1));
                char *meshName = (data->orig_bytes+5);
                int entityPtr = create_entity(nodePtr, meshName);
                ErlDrvTermData spec[] = {ERL_DRV_INT, entityPtr};
                driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
            }
            return;
        case 8:
            {
                int nodePtr = *((int*)(data->orig_bytes+1));
                float x = *((float *)(data->orig_bytes+5));
                float y = *((float *)(data->orig_bytes+9));
                float z = *((float *)(data->orig_bytes+13));
            }
            break;
  }

  driver_output_term(driver_data->port, ok_spec, sizeof(ok_spec) / sizeof(ok_spec[0]));
}

static ErlDrvEntry ogre_driver_entry = {
    NULL, /* init */
    start, /* startup */
    stop, /* shutdown */
    NULL, /* output */
    NULL, /* ready_input */
    NULL, /* ready_output */
    "ogre_driver", /* the name of the driver */
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    process, /* process */
    NULL, /* ready_async */
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER, /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING /* ERL_DRV_FLAGs */
};
extern "C" {
DRIVER_INIT(ogre_driver) /* must match name in driver_entry */
{
    return &ogre_driver_entry;
}
}

