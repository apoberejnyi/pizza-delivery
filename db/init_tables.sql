CREATE TABLE IF NOT EXISTS users (
    id uuid NOT NULL,
    first_name text NOT NULL,
    last_name text,
    email text NOT NULL,
    phone_number text NOT NULL,
    addresses json [] NOT NULL,
    pwd_hash text NOT NULL,
    roles text [],

    CONSTRAINT users_email UNIQUE (email)
);

CREATE TABLE IF NOT EXISTS orders (
    id uuid NOT NULL,
    status varchar(50) NOT NULL,
    items uuid [] NOT NULL,
    address text NOT NULL,
    restaurant_id uuid NOT NULL,
    user_id uuid NOT NULL,
    placed_at timestamp NOT NULL,
    resolved_at timestamp,

    CONSTRAINT orders_pkey PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS orderoptions (
    id uuid NOT NULL,
    name text NOT NULL,
    sizes json NOT NULL,

    CONSTRAINT orderoptions_pkey PRIMARY KEY (id),
    CONSTRAINT name UNIQUE (name)
);

CREATE TABLE IF NOT EXISTS restaurants (
    id uuid NOT NULL,
    name text NOT NULL,
    lat real NOT NULL,
    lon real NOT NULL,

    CONSTRAINT restaurants_pkey PRIMARY KEY (id),
    CONSTRAINT restaurants_name UNIQUE (name)
);
