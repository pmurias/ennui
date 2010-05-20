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
        return enif_make_atom(env, "error");
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
static ErlNifFunc nif_funcs[] =
{
    {"init_ogre", 0, init_ogre}
};
extern "C" {
ERL_NIF_INIT(ogre,nif_funcs,NULL,NULL,NULL,NULL)
}
