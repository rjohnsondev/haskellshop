-- dbext:profile=psql_local:user=account

BEGIN;

DROP TABLE IF EXISTS product_categories;
DROP TABLE IF EXISTS product_details;
DROP TABLE IF EXISTS order_items;
DROP TABLE IF EXISTS order_addresses;
DROP TABLE IF EXISTS orders;
DROP TABLE IF EXISTS address_types;
DROP TABLE IF EXISTS categories;
DROP TABLE IF EXISTS product_variant_options;
DROP TABLE IF EXISTS product_images;
DROP TABLE IF EXISTS products;
DROP TABLE IF EXISTS variant_options;
DROP TABLE IF EXISTS variants;
DROP TABLE IF EXISTS snap_auth_user;

CREATE TABLE products (
    "product_id" SERIAL PRIMARY KEY,
    "name" VARCHAR(255) NOT NULL,
    "manufacturer" VARCHAR(255) NOT NULL,
    "base_price" DECIMAL(10, 2) NOT NULL,
    "enabled" BOOLEAN NOT NULL
);

CREATE TABLE product_details (
    "product_detail_id" SERIAL PRIMARY KEY,
    "product_id" INT REFERENCES products(product_id) NOT NULL,
    "order_by" SERIAL,
    "title" VARCHAR(255),
    "detail" TEXT
);

CREATE TABLE variants (
    "variant_id" SERIAL PRIMARY KEY,
    "name" VARCHAR(255) NOT NULL,
    "adjusts_price" BOOLEAN NOT NULL,
    "searchable" BOOLEAN NOT NULL
);
INSERT INTO variants (name, adjusts_price, searchable) VALUES ('colour', False, True), ('size', True, False);

CREATE TABLE variant_options (
    "variant_option_id" SERIAL PRIMARY KEY,
    "variant_id" INT REFERENCES variants(variant_id) NOT NULL,
    "option" VARCHAR(255) NOT NULL
);
INSERT INTO variant_options ("variant_id", "option") VALUES
    (1, 'Blue'),
    (1, 'Red'),
    (1, 'Black'),
    (1, 'Pink'),
    (1, 'Green'),
    (1, 'Brown'),
    (1, 'Orange'),
    (1, 'White'),
    (1, 'Purple'),
    (1, 'Yellow'),
    (1, 'Grey'),
    (1, 'Cream'),
    (1, 'Gold'),
    (1, 'Beige'),
    (1, 'Multi'),
    (1, 'Silver'),
    (1, 'Tan'),
    (2, 'Blah!');


CREATE TABLE product_images (
    "hash" CHAR(64),
    "product_id" INT REFERENCES products(product_id),
    "order_by" SERIAL,
    PRIMARY KEY ("product_id", "hash")
);
CREATE INDEX ON product_images ("order_by");

CREATE TABLE product_variant_options (
    "product_id" INT REFERENCES products(product_id) NOT NULL,
    "variant_option_id" INT REFERENCES variant_options(variant_option_id) NOT NULL,
    "price_adjustment" DECIMAL(10, 2) NOT NULL,
    PRIMARY KEY(product_id, variant_option_id)
);
CREATE INDEX ON product_variant_options ("variant_option_id");

CREATE TABLE categories (
    "category_id" SERIAL PRIMARY KEY,
    "name" VARCHAR(255),
    "parent_category_id" INT NULL REFERENCES categories(category_id),
    "order_by" SERIAL
);
CREATE INDEX categories_order_by ON categories ("order_by");
INSERT INTO categories (name, parent_category_id) VALUES
    ('Dogs', NULL),
    ('Cats', NULL),
    ('Kitten', NULL),
    ('Puppy', NULL),
    ('Gifts', NULL),
    ('Leads', 1);

CREATE TABLE product_categories (
    "product_id" INT REFERENCES products(product_id),
    "category_id" INT REFERENCES categories(category_id),
    PRIMARY KEY (product_id, category_id)
);
CREATE INDEX ON product_categories (category_id);

CREATE TABLE orders (
    "order_id" SERIAL PRIMARY KEY,
    "purchase_time" TIMESTAMP with time zone NOT NULL,
    "total" DECIMAL(10, 2),
    "email" VARCHAR(255),
    "phone" VARCHAR(255),
    "processed_time" TIMESTAMP with time zone NULL
);

CREATE TABLE address_types (
    "type" VARCHAR(20) PRIMARY KEY
);
INSERT INTO address_types VALUES ('Billing'), ('Shipping');

CREATE TABLE order_addresses (
    "order_address_id" SERIAL PRIMARY KEY,
    "order_id" INT REFERENCES orders(order_id) NOT NULL,
    "address_type" VARCHAR(20) REFERENCES address_types("type") NOT NULL,
    "full_name" VARCHAR(255),
    "line_1" VARCHAR(255),
    "line_2" VARCHAR(255),
    "city" VARCHAR(255),
    "state" VARCHAR(255),
    "postcode" VARCHAR(255),
    "country" VARCHAR(3),
    UNIQUE(order_id, address_type)
);

CREATE TABLE order_items (
    "order_item_id" SERIAL PRIMARY KEY,
    "order_id" INT REFERENCES orders(order_id) NOT NULL,
    "product_id" INT REFERENCES products(product_id) NOT NULL,
    "variant_options" VARCHAR(255),
    "line_total" DECIMAL(10, 2) NOT NULL,
    "quantity" INT NOT NULL
);

CREATE TABLE snap_auth_user (
    uid SERIAL PRIMARY KEY,
    login text NOT NULL,
    email text,
    password text,
    activated_at timestamp with time zone,
    suspended_at timestamp with time zone,
    remember_token text,
    login_count integer NOT NULL,
    failed_login_count integer NOT NULL,
    locked_out_until timestamp with time zone,
    current_login_at timestamp with time zone,
    last_login_at timestamp with time zone,
    current_login_ip text,
    last_login_ip text,
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    reset_token text,
    reset_requested_at timestamp with time zone,
    UNIQUE(login)
);
INSERT INTO snap_auth_user (login, email, password, login_count, failed_login_count, activated_at, created_at) VALUES (
    'test', 'test@test.com', 'sha256|12|PggIvPUMBbgFO7DHLE1jrw==|YfD72HEeBiu/o1sKbuicaXxsSpwbb1v2S08XHYNMfBo=',
    0,0, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);


COMMIT;

BEGIN;
    DO $$
    DECLARE pid INT;
    BEGIN
        INSERT INTO products (name, manufacturer, base_price, enabled) VALUES (
            'Test Product',
            'Test manufacturer',
            10.0,
            true) RETURNING product_id INTO pid;
        INSERT INTO product_variant_options VALUES (
            pid,
            1,
            10);
        INSERT INTO product_details (product_id, title, detail) VALUES (1, 'test', 'this is some test details');
    END$$;

COMMIT;

