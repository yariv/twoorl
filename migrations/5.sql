alter table usr add column gravatar_enabled tinyint unsigned not null
      default 0;

alter table msg add column usr_gravatar_enabled tinyint unsigned not null
      default 0;

alter table msg add column usr_gravatar_id char(32);
