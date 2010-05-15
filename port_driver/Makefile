example_drv.so: example_drv.cpp
	g++ -fpic -shared example_drv.cpp `pkg-config --libs OIS` `pkg-config --libs OGRE` -o example_drv.so
