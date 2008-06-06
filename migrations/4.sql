#0: not sent, 1: pending, 2: sent_successfully, 3: send_failure
alter table msg add column twitter_status tinyint not null default 0;
alter table msg modify column body_raw text not null;
alter table msg modify column body text not null;

# fix created_on columns to not change on updates
alter table msg modify column created_on timestamp default current_timestamp;
alter table following modify column created_on timestamp default current_timestamp;
alter table reply modify column created_on timestamp default current_timestamp;
alter table usr modify column created_on timestamp default current_timestamp;
