ogre_driver.so: ogre_driver.cpp
	g++ -fpic -I /usr/lib/erlang/erts-5.7.5/include/ -shared ogre_driver.cpp `pkg-config --libs OIS` `pkg-config --libs OGRE` -o ogre_driver.so
