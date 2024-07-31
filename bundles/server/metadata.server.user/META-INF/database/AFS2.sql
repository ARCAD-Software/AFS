-- User Management Tables
--

alter table USERS add constraint USR_UST_ID_FK foreign key (USR_TITLE) references USER_TITLES (UST_ID) on delete set null;

create index PRF_CODE_INDEX on PROFILES (PRF_CODE, PRF_DELETED);

create index PRF_CODE_INDEX on PROFILE_RIGHTS (PFR_PRF_ID, PFR_DELETED);

--
-- End of User Management
