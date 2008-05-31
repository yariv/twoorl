drop table if exists usr;
create table usr (
       id integer unsigned auto_increment primary key,
       username varchar(30) not null,
       password char(20) not null,
       email varchar(50) not null,
       num_msgs integer unsigned not null default 0,
       num_replies integer unsigned not null default 0,
       created_on timestamp,
       unique(username, email)
) engine=innodb;

drop table if exists following;
create table following (
       usr_id1 integer unsigned not null,
       usr_id2 integer unsigned not null,

       # denormalized usernames to reduce lookups
       usr_username1 varchar(30) not null,
       usr_username2 varchar(30) not null,

       created_on timestamp,
       primary key(usr_id1, usr_id2)
) engine=innodb;


drop table if exists msg;
create table msg (
       id integer unsigned auto_increment primary key,
       usr_id integer unsigned not null,

       # denormalized username
       usr_username varchar(30) not null,

       # raw data entered by the user
       body_raw varchar(140) not null,

       # with pre-escaped html entities and links
       body varchar(140) not null,

       created_on timestamp,
       index(usr_id, created_on)
) engine=innodb;

drop table if exists reply;
create table reply (
       usr_id integer unsigned not null,
       msg_id integer unsigned not null,
       created_on timestamp,
       primary key(usr_id, msg_id),
       index(created_on)
) engine=innodb;
