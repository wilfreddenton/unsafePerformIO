CREATE TABLE posts (
       id INTEGER PRIMARY KEY,
       slug VARCHAR(291) UNIQUE NOT NULL,
       title VARCHAR(280) NOT NULL,
       created_at VARCHAR(32) NOT NULL,
       body TEXT NOT NULL
);

CREATE TABLE about (
       id INTEGER PRIMARY KEY,
       title VARCHAR(280) NOT NULL,
       body TEXT NOT NULL
);

CREATE TABLE contact (
       id INTEGER PRIMARY KEY,
       location VARCHAR(280),
       email VARCHAR(280),
       linked_in VARCHAR(280),
       facebook_messenger VARCHAR(280),
       instagram VARCHAR(280)
);
