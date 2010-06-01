all: ogre.beam ogre.so
ogre.beam: ogre.erl
	erlc `perl checksum` ogre.erl
ogre.so:  ogre.cpp
	gcc -fPIC -shared -I /usr/lib/erlang/erts-5.7.5/include/ -shared ogre.cpp `pkg-config --libs OIS` `pkg-config --libs OGRE` -o ogre.so
