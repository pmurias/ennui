all: bullet.erl ogre.beam bullet.beam ogre.so bullet.so 
ogre.beam: ogre.erl
	erlc `perl checksum` ogre.erl
bullet.beam: bullet.erl
	erlc bullet.erl
ogre.so:  ogre.cpp
	gcc -fPIC -shared -I /usr/lib/erlang/erts-5.7.5/include/ -I /usr/include/bullet -I /usr/include/bullet/BulletDynamics/Dynamics -I /usr/include/bullet/BulletCollision/CollisionShapes  -I bullet/src -I bullet/src/BulletDynamics/Dynamics -I /usr/lib/erlang/erts-5.7.5/include/ -L bullet/src/BulletCollision -L bullet/src/BulletSoftBody -L bullet/src/BulletDynamics -lBulletCollision -lBulletSoftBody -LBulletDynamics -shared ogre.cpp `pkg-config --libs OIS` `pkg-config --libs OGRE` -o ogre.so

bullet.erl: list
	perl process list > bullet.erl
bullet.so:  bullet.cpp
	gcc -fPIC -shared -I /usr/lib/erlang/erts-5.7.5/include/ -I /usr/include/bullet -I /usr/include/bullet/BulletDynamics/Dynamics -I /usr/include/bullet/BulletCollision/CollisionShapes  -I bullet/src -I bullet/src/BulletDynamics/Dynamics -shared bullet.cpp  -o bullet.so -L bullet/src/BulletCollision -L bullet/src/BulletSoftBody -L bullet/src/BulletDynamics -lBulletCollision -lBulletSoftBody -LBulletDynamics -I bullet/src/BulletCollision/CollisionShapes
