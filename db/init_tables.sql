CREATE TABLE users (
    id uuid NOT NULL,
    first_name text NOT NULL,
    last_name text,
    email text NOT NULL,
    phone_number text NOT NULL,
    addresses json [] NOT NULL,
    pwd_hash text NOT NULL,

    CONSTRAINT users_email UNIQUE (email)
);

CREATE TABLE orders (
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

CREATE TABLE orderoptions (
    id uuid NOT NULL,
    name text NOT NULL,
    sizes json NOT NULL,

    CONSTRAINT orderoptions_pkey PRIMARY KEY (id),
    CONSTRAINT name UNIQUE (name)
);

CREATE TABLE restaurants (
    id uuid NOT NULL,
    name text NOT NULL,
    lat real NOT NULL,
    lon real NOT NULL,

    CONSTRAINT restaurants_pkey PRIMARY KEY (id),
    CONSTRAINT restaurants_name UNIQUE (name)
);
