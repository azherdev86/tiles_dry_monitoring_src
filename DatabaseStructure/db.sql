/******************************************************************************/
/***               Generated from IBExpert 12.03.2017 19:31:50                ***/
/******************************************************************************/

/******************************************************************************/
/***      Following SET SQL DIALECT is just for the Database Comparer       ***/
/******************************************************************************/
SET SQL DIALECT 3;

/******************************************************************************/
/***                               Generators                               ***/
/******************************************************************************/

CREATE GENERATOR GEN_SENSORS_ID;
CREATE GENERATOR GEN_TEMPVALUES_ID;
CREATE GENERATOR GEN_EVENTLOGS_ID;
CREATE GENERATOR GEN_MESSAGES_ID;

/******************************************************************************/
/***                                 Tables                                 ***/
/******************************************************************************/

CREATE TABLE SENSORS (
    SENSORID        INTEGER NOT NULL,
    CONVEYORNUMBER  INTEGER NOT NULL,
    SECTIONNUMBER   INTEGER NOT NULL,
    BOXNUMBER       INTEGER NOT NULL,
    SENSORPOSITION  VARCHAR(12) NOT NULL
);

CREATE TABLE TEMPVALUES (
    TEMPVALUEID  INTEGER NOT NULL,
    SENSORID     INTEGER NOT NULL,
    TEMPVALUE    FLOAT NOT NULL,
    TEMPTIME     TIMESTAMP NOT NULL
);

CREATE TABLE EVENTLOGS (
    EVENTLOGID       INTEGER NOT NULL,
    EVENTLOGTYPE     VARCHAR(20) NOT NULL,
    EVENTLOGTIME     TIMESTAMP NOT NULL,
    EVENTLOGDETAILS  VARCHAR(511)
);

CREATE TABLE MESSAGES (
    MESSAGEID                INTEGER NOT NULL,
    MESSAGETYPE              VARCHAR(3) CHARACTER SET WIN1251 NOT NULL,
    MESSAGERECIEVEDTIME      TIMESTAMP,
    MESSAGERECIEVEDPARTTIME  TIMESTAMP,
    MESSAGECREATIONTIME      TIMESTAMP,
    MESSAGESENTTIME          TIMESTAMP,
    MESSAGEDELIEVEREDTIME    TIMESTAMP,
    MESSAGESTATE             VARCHAR(20) CHARACTER SET WIN1251 NOT NULL,
    MESSAGEERROR             VARCHAR(20) CHARACTER SET WIN1251 NOT NULL,
    MESSAGEBYTES             VARCHAR(255) CHARACTER SET WIN1251 NOT NULL,
    MESSAGEUID               VARCHAR(20) CHARACTER SET WIN1251 NOT NULL
);


/******************************************************************************/
/***                              Primary Keys                              ***/
/******************************************************************************/

ALTER TABLE SENSORS ADD CONSTRAINT PK_SENSORS PRIMARY KEY (SENSORID);
ALTER TABLE TEMPVALUES ADD CONSTRAINT PK_TEMPVALUES PRIMARY KEY (TEMPVALUEID);
ALTER TABLE EVENTLOGS ADD CONSTRAINT PK_EVENTLOGS PRIMARY KEY (EVENTLOGID);
ALTER TABLE MESSAGES ADD CONSTRAINT PK_MESSAGES PRIMARY KEY (MESSAGEID);

/******************************************************************************/
/***                              Foreign Keys                              ***/
/******************************************************************************/

ALTER TABLE TEMPVALUES ADD CONSTRAINT FK_TEMPVALUES_SENSORS FOREIGN KEY (SENSORID) REFERENCES SENSORS (SENSORID);

/******************************************************************************/
/***                                Indices                                 ***/
/******************************************************************************/

CREATE INDEX "TEMPVALUES_IDX_TempTime" ON TEMPVALUES (TEMPTIME);
CREATE DESCENDING INDEX "TEMPVALUES_IDX_TempValues_DESC" ON TEMPVALUES (TEMPTIME);
CREATE UNIQUE DESCENDING INDEX SENSORS_CON_SEC_POS ON SENSORS (CONVEYORNUMBER, SECTIONNUMBER, SENSORPOSITION);

/******************************************************************************/
/***                                Triggers                                ***/
/******************************************************************************/

SET TERM ^ ;

/* Trigger: EVENTLOGS_BI */
CREATE OR ALTER TRIGGER EVENTLOGS_BI FOR EVENTLOGS
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.eventlogid is null) then
    new.eventlogid = gen_id(gen_eventlogs_id,1);
end
^

/* Trigger: TEMPVALUES_BI */
CREATE OR ALTER TRIGGER TEMPVALUES_BI FOR TEMPVALUES
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.tempvalueid is null) then
    new.tempvalueid = gen_id(gen_tempvalues_id,1);
end
^

/* Trigger: SENSORS_BI */
CREATE OR ALTER TRIGGER SENSORS_BI FOR SENSORS
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.sensorid is null) then
    new.sensorid = gen_id(gen_sensors_id,1);
end
^

CREATE OR ALTER TRIGGER MESSAGES_BI FOR MESSAGES
ACTIVE BEFORE INSERT POSITION 0
as
begin
  if (new.messageid is null) then
    new.messageid = gen_id(gen_messages_id,1);
end
^

SET TERM ; ^