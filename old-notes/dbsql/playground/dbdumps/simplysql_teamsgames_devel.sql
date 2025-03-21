--
-- PostgreSQL database dump
--

-- Dumped from database version 14.8 (Debian 14.8-1.pgdg120+1)
-- Dumped by pg_dump version 14.8 (Debian 14.8-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: simplysql_teamsgames_dev; Type: DATABASE; Schema: -; Owner: dev
--

CREATE DATABASE simplysql_teamsgames_dev WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.UTF-8';


ALTER DATABASE simplysql_teamsgames_dev OWNER TO dev;

\connect simplysql_teamsgames_dev

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: simplysql_teamsgames_dev; Type: DATABASE PROPERTIES; Schema: -; Owner: dev
--

ALTER DATABASE simplysql_teamsgames_dev CONNECTION LIMIT = 3;


\connect simplysql_teamsgames_dev

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: teams; Type: TABLE; Schema: public; Owner: dev
--

CREATE TABLE public.teams (
    id integer NOT NULL,
    name character varying(37) NOT NULL,
    conference character varying(2)
);


ALTER TABLE public.teams OWNER TO dev;

--
-- Data for Name: teams; Type: TABLE DATA; Schema: public; Owner: dev
--

COPY public.teams (id, name, conference) FROM stdin;
37	Havoc	F
63	Brewers	C
9	Riff Raff	E
\.


--
-- Name: teams teams_pkey; Type: CONSTRAINT; Schema: public; Owner: dev
--

ALTER TABLE ONLY public.teams
    ADD CONSTRAINT teams_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

