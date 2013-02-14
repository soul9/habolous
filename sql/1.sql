SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET search_path = habolous, pg_catalog;
INSERT INTO role (role_name) VALUES ('superuser');
INSERT INTO role (role_name) VALUES ('simpleuser');
UPDATE metadata SET value='1' WHERE key='databaseVersion';
