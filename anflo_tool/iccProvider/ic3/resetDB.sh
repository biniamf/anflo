mysql -h 127.0.0.1 -uroot -pmysql -e 'drop database cc; create database cc';
mysql -h 127.0.0.1 -uroot -pmysql cc < ../../iccta/res/schema.sql #../../res/schema.sql;
