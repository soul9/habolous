SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
CREATE SCHEMA habolous AUTHORIZATION habolous;
SET search_path = habolous, pg_catalog;
SET default_with_oids = false;
CREATE TABLE employee (
    employee_id integer NOT NULL,
    login_name text NOT NULL,
    password text NOT NULL,
    employee_group_id integer,
    employee_role_id integer,
    employee_person_id integer
);
CREATE SEQUENCE employee_employee_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE employee_employee_group_id_seq OWNED BY employee.employee_group_id;
SELECT pg_catalog.setval('employee_employee_group_id_seq', 1, true);
CREATE SEQUENCE employee_employee_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE employee_employee_id_seq OWNED BY employee.employee_id;
SELECT pg_catalog.setval('employee_employee_id_seq', 1, true);
CREATE SEQUENCE employee_employee_person_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE employee_employee_person_id_seq OWNED BY employee.employee_person_id;
SELECT pg_catalog.setval('employee_employee_person_id_seq', 1, true);
CREATE SEQUENCE employee_employee_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE employee_employee_role_id_seq OWNED BY employee.employee_role_id;
SELECT pg_catalog.setval('employee_employee_role_id_seq', 1, false);
CREATE TABLE "group" (
    group_id integer NOT NULL,
    group_name text NOT NULL
);
CREATE SEQUENCE group_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE group_group_id_seq OWNED BY "group".group_id;
SELECT pg_catalog.setval('group_group_id_seq', 1, false);
CREATE TABLE metadata (
    key text NOT NULL,
    value text,
    comment text
);
CREATE TABLE person (
    person_id integer NOT NULL,
    firstname text NOT NULL,
    lastname text NOT NULL,
    birthday date NOT NULL
);
CREATE SEQUENCE person_person_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE person_person_id_seq OWNED BY person.person_id;
SELECT pg_catalog.setval('person_person_id_seq', 1, false);
CREATE TABLE role (
    role_id integer NOT NULL,
    role_name text NOT NULL
);
CREATE SEQUENCE role_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
CREATE TABLE session (
  employee_id integer NOT NULL,
  session_uuid text NOT NULL,
  expires timestamp without time zone NOT NULL
);
ALTER SEQUENCE role_role_id_seq OWNED BY role.role_id;
SELECT pg_catalog.setval('role_role_id_seq', 1, true);
ALTER TABLE ONLY employee ALTER COLUMN employee_id SET DEFAULT nextval('employee_employee_id_seq'::regclass);
ALTER TABLE ONLY "group" ALTER COLUMN group_id SET DEFAULT nextval('group_group_id_seq'::regclass);
ALTER TABLE ONLY person ALTER COLUMN person_id SET DEFAULT nextval('person_person_id_seq'::regclass);
ALTER TABLE ONLY role ALTER COLUMN role_id SET DEFAULT nextval('role_role_id_seq'::regclass);
INSERT INTO metadata (key, value, comment) VALUES ('databaseVersion', '0', 'this is how we figure out what the database version is, to be able to update the database when needed, on application update');
ALTER TABLE ONLY employee
    ADD CONSTRAINT employee_login_name_key UNIQUE (login_name);
ALTER TABLE ONLY employee
    ADD CONSTRAINT employee_pkey PRIMARY KEY (employee_id);
ALTER TABLE ONLY "group"
    ADD CONSTRAINT group_group_id_key UNIQUE (group_id);
ALTER TABLE ONLY "group"
    ADD CONSTRAINT group_group_name_key UNIQUE (group_name);
ALTER TABLE ONLY "group"
    ADD CONSTRAINT group_pkey PRIMARY KEY (group_id);
ALTER TABLE ONLY metadata
    ADD CONSTRAINT metadata_pkey PRIMARY KEY (key);
ALTER TABLE ONLY person
    ADD CONSTRAINT person_pkey PRIMARY KEY (person_id);
ALTER TABLE ONLY role
    ADD CONSTRAINT role_pkey PRIMARY KEY (role_id);
ALTER TABLE ONLY role
    ADD CONSTRAINT role_role_id_key UNIQUE (role_id);
ALTER TABLE ONLY role
    ADD CONSTRAINT role_role_name_key UNIQUE (role_name);
ALTER TABLE ONLY employee
    ADD CONSTRAINT employee_employee_group_id_fkey FOREIGN KEY (employee_group_id) REFERENCES "group"(group_id) ON DELETE RESTRICT;
ALTER TABLE ONLY employee
    ADD CONSTRAINT employee_employee_person_id_fkey FOREIGN KEY (employee_person_id) REFERENCES person(person_id) ON DELETE RESTRICT;
ALTER TABLE ONLY employee
    ADD CONSTRAINT employee_employee_role_id_fkey FOREIGN KEY (employee_role_id) REFERENCES role(role_id) ON DELETE RESTRICT;
ALTER TABLE ONLY session
    ADD CONSTRAINT session_pkey PRIMARY KEY (session_uuid);
ALTER TABLE ONLY session
    ADD CONSTRAINT session_employee_id_fkey FOREIGN KEY (employee_id) REFERENCES employee (employee_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE NO ACTION;
ALTER TABLE ONLY session
    ADD CONSTRAINT session_session_uuid_key UNIQUE (session_uuid);
