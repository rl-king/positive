create schema if not exists positive;

drop table if exists positive.image cascade;
create table if not exists positive.image
  ( id serial primary key
  , filename text not null
  , rating int2 not null
  , orientation float
  , crop jsonb
  , gamma float
  , zones jsonb
  , blackpoint float
  , whitepoint float
  , expressions jsonb
  , created timestamptz default now()
  , modified timestamptz default now()
  , preview timestamptz

  , unique (id, filename)
  );

drop table if exists positive.filmroll cascade;
create table if not exists positive.filmroll
  ( id serial primary key
  , poster serial references positive.image(id)
  , directory_path text not null

  , unique (id, directory_path)
  );

-- create table positive.poster
--   ( filmroll_id serial references positive.filmroll id
--   , image_id serial references positive.image id
--   )

-- create table positive.collection
--   ( id serial primary key
--   , name text not null
--   , image_id serial references positive.image id
--   )
