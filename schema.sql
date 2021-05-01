create schema if not exists positive;

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

create table if not exists positive.film_roll
  ( id serial primary key
  , poster integer null references positive.image(id)
  , directory_path text not null

  , unique (directory_path)
  );

alter table positive.image
add film_roll_id integer references positive.film_roll(id);

-- create table positive.collection
--   ( id serial primary key
--   , name text not null
--   , image_id serial references positive.image id
--   )
