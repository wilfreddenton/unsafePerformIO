CREATE TABLE posts (
       id PRIMARY KEY,
       slug VARCHAR(289) UNIQUE NOT NULL,
       title VARCHAR(280) NOT NULL,
       created_at VARCHAR(32) NOT NULL,
       body TEXT NOT NULL
);
