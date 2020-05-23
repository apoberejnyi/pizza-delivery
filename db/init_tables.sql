CREATE TABLE orders (
    id uuid NOT NULL,
    items uuid [] NOT NULL,
    address text NOT NULL,
    restaurantId uuid NOT NULL,

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
