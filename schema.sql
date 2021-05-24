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

  , unique (id, filename)
  );

create table if not exists positive.image_metadata
  ( id serial primary key
  , image_id integer not null references positive.image(id)
  , preview_updated timestamptz
  , histogram int4[] default array[]::int4[]

  , unique (image_id)
  );

create table if not exists positive.film_roll
  ( id serial primary key
  , poster integer null references positive.image(id)
  , directory_path text not null

  , unique (directory_path)
  );

create table if not exists positive.collection
  ( id serial primary key
  , title text not null check (title <> '')
  , created timestamptz default now()
  , modified timestamptz default now()

  , unique (title)
  );

create table if not exists positive.image_collection
  ( image_id integer null references positive.image(id)
  , collection_id integer null references positive.collection(id)
  , created timestamptz default now()

  , unique (image_id, collection_id)
  );

alter table positive.image
add if not exists film_roll_id integer references positive.film_roll(id);

alter table positive.film_roll
add if not exists created timestamptz default now(),
add if not exists modified timestamptz default now();

alter table positive.collection
add if not exists target boolean not null default false;

create unique index on positive.collection (target)
where target = true;

-- FUN

create or replace function positive.update_modified_timestamp()
  returns trigger
  language plpgsql
  as $$
    begin
      if new <> old then
        new.modified = now();
      end if;
      return new;
    end
  $$;

-- TRIGGER

drop trigger if exists on_image_modified on positive.image;
create trigger on_image_modified
  before update
  on positive.image
  for each row
  execute procedure positive.update_modified_timestamp();

drop trigger if exists on_film_roll_modified on positive.film_roll;
create trigger on_film_roll_modified
  before update
  on positive.film_roll
  for each row
  execute procedure positive.update_modified_timestamp();

drop trigger if exists on_collection_modified on positive.collection;
create trigger on_collection_modified
  before update
  on positive.collection
  for each row
  execute procedure positive.update_modified_timestamp();
