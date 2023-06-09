-- User Management Tables
--

create table USER_TITLES (
    UST_ID integer generated by default as identity primary key,
    UST_DELETED integer default 0); 

create table USERS (
    USR_ID integer generated by default as identity primary key,
    USR_TITLE integer,
    USR_FIRSTNAME varchar(255), 
    USR_LASTNAME varchar(255), 
    USR_EMAIL varchar(255), 
    USR_DELETED integer default 0,
    USR_UPDATE timestamp default current_timestamp);

create index USR_NAME_INDEX on USERS (USR_FIRSTNAME, USR_LASTNAME);
create index USR_EMAIL_INDEX on USERS (USR_EMAIL);

create table PROFILES (
    PRF_ID integer generated by default as identity primary key,
    PRF_CODE varchar(50),
    PRF_NAME varchar(255),
    PRF_DELETED integer default 0,
    PRF_UPDDATE timestamp default current_timestamp); 

create table USER_PROFILES (
    UPF_ID integer generated by default as identity primary key,
    UPF_USR_ID integer,
    UPF_PRF_ID integer,
    constraint UPF_USR_PRF_UC unique (UPF_USR_ID, UPF_PRF_ID));

create index UPF_PRF_INDEX on USER_PROFILES (UPF_PRF_ID);

create table PROFILE_RIGHTS (
    PFR_ID integer generated by default as identity primary key,
    PFR_PRF_ID integer,
    PFR_RIGHT integer,
    PFR_PARAMETER integer,
    PFR_DELETED integer default 0,
    PFR_UPDDATE timestamp default current_timestamp);

create view USER_RIGHTS (URI_ID, URI_USER, URI_RIGHT, URI_PARAM) as
    select distinct
        PFR_ID,
        UPF_USR_ID,
        PFR_RIGHT,
        PFR_PARAMETER
    from USER_PROFILES left outer join PROFILE_RIGHTS on PFR_PRF_ID = UPF_PRF_ID; 

insert into USER_TITLES (UST_DELETED) values (0), (0);

-- Initialize the first user of the application.
insert into PROFILES (PRF_CODE, PRF_NAME) values ('ALL', 'Users with all rights on web-services');
insert into USERS (USR_FIRSTNAME, USR_LASTNAME) values ('', 'Administrator');
insert into USER_PROFILES (UPF_USR_ID, UPF_PRF_ID) values (1, 1);

alter table USER_PROFILES add constraint UPF_USR_ID_FK foreign key (UPF_USR_ID) references USERS (USR_ID) on delete cascade;
alter table USER_PROFILES add constraint UPF_PRF_ID_FK foreign key (UPF_PRF_ID) references PROFILES (PRF_ID) on delete cascade;
alter table PROFILE_RIGHTS add constraint PFR_PRF_ID_FK foreign key (PFR_PRF_ID) references PROFILES (PRF_ID) on delete cascade;

--
-- End of User Management
