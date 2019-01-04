CREATE TABLE posts (
       id PRIMARY KEY,
       slug VARCHAR(291) UNIQUE NOT NULL,
       title VARCHAR(280) NOT NULL,
       created_at VARCHAR(32) NOT NULL,
       body TEXT NOT NULL
);

CREATE TABLE about (
       id PRIMARY KEY,
       title VARCHAR(280) NOT NULL,
       body TEXT NOT NULL
);

CREATE TABLE contact (
       id PRIMARY KEY,
       location VARCHAR(280),
       email VARCHAR(280),
       linked_in VARCHAR(280),
       facebook_messenger VARCHAR(280),
       instagram VARCHAR(280)
);

CREATE TABLE pgp (
       id PRIMARY KEY,
       pgp_key TEXT NOT NULL
);
