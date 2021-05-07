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

-- TABLE

create table if not exists positive.film_roll
  ( id serial primary key
  , poster integer null references positive.image(id)
  , directory_path text not null

  , unique (directory_path)
  );

alter table positive.image
add film_roll_id integer references positive.film_roll(id);

alter table positive.film_roll
add created timestamptz default now(),
add modified timestamptz default now();

-- FUN

create or replace function positive.update_modified_timestamp()
  returns trigger
  language plpgsql
  as $$
    begin
      if new <> old then
        new.modified = now();
        return new;
      end if;
    end
  $$;

-- TRIGGER

create trigger on_image_modified
  before update
  on positive.image
  for each row
  execute procedure positive.update_modified_timestamp();

create trigger on_film_roll_modified
  before update
  on positive.film_roll
  for each row
  execute procedure film_roll.update_modified_timestamp();
