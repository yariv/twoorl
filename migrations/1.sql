alter table usr add column session_key char(20);
alter table usr add index(session_key);
alter table usr add column twitter_username varchar(30);
alter table usr add column twitter_password varchar(30);
alter table usr add column twitter_enabled tinyint default 0; # 0=true, 1=false
