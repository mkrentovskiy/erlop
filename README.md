ErlOp is a simple Web-UI for executing shell scripts on remote servers. It can be used for automation and remote control for server 
clusers with OpenVZ containers.

Installation

0. make; cd deps/erlydtl; make; cd ../..; make
1. nano priv/sys.config
2. sudo su postgres -c 'psql'
```
 postgres=# CREATE DATABASE erlop ENCODING 'utf8';
 CREATE DATABASE
 postgres=# CREATE USER erlop WITH PASSWORD 'erlop';
 CREATE ROLE
 postgres=# GRANT ALL ON DATABASE erlop TO erlop;
 GRANT
```
3. cd priv; mkdir ssh; cd ssh; ssh-keygen -f ./id_rsa; ssh-copy-id -i ./id_rsa \[all servers\] (root by default)
4. cd ../..; make run
```
    (wa@localhost)1> persist:init_db(pgdb).
    [[{ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}},
      {ok,{[],[]}}]]
```
5. firefox http://localhost:8080/


