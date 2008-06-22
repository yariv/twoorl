alter table usr add column spammer tinyint not null default 0;
alter table usr add index(spammer);

alter table msg add column spam tinyint not null default 0;
alter table msg add index(spam);