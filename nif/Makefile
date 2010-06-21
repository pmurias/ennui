all: ogre.beam ogre.so bullet.so bullet.erl
ogre.beam: ogre.erl
	erlc `perl checksum` ogre.erl
ogre.so:  ogre.cpp
	gcc -fPIC -shared -I /usr/lib/erlang/erts-5.7.5/include/ -shared ogre.cpp `pkg-config --libs OIS` `pkg-config --libs OGRE` -o ogre.so

bullet.erl: list
	perl process list > bullet.erl
bullet.so:  bullet.cpp
	gcc -fPIC -shared -I /usr/lib/erlang/erts-5.7.5/include/ -I bullet/src -shared bullet.cpp  -o bullet.so -L bullet/src/BulletCollision -L bullet/src/BulletSoftBody -L bullet/src/BulletDynamics -lBulletCollision -lBulletSoftBody -LBulletDynamics
