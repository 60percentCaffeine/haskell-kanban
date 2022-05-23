drop database kanban;

drop user kuser;
create user kuser with encrypted password 'qwerty' superuser;
set role kuser;

drop database kanban;
create database kanban;
grant all privileges on database kanban to kuser;

\c kanban

CREATE TABLE employee (
    employee_id serial  PRIMARY KEY,
    employee_fullname varchar (50) NOT NULL
);

CREATE TABLE  card (
    card_id          serial PRIMARY KEY,
    card_title       varchar (50) NOT NULL,      
    card_worker_id      integer NOT NULL, 
    card_column      integer check (card_column in (1,2,3)) NOT NULL,

    card_kanban_id   integer NOT NULL, 
    FOREIGN KEY (card_worker_id) REFERENCES employee(employee_id)
);

INSERT INTO employee VALUES (0, 'Undefined');
INSERT INTO employee (employee_fullname) VALUES ('Bob');
INSERT INTO employee (employee_fullname) VALUES ('Alice');
INSERT INTO employee (employee_fullname) VALUES ('John');
INSERT INTO employee (employee_fullname) VALUES ('Valera');

INSERT INTO card (card_title, card_worker_id, card_column, card_kanban_id) VALUES ('Test Card 1', 0, 1, 0);
INSERT INTO card (card_title, card_worker_id, card_column, card_kanban_id) VALUES ('Test Card 2', 0, 1, 1);
INSERT INTO card (card_title, card_worker_id, card_column, card_kanban_id) VALUES ('Test Card 3', 0, 2, 0);
INSERT INTO card (card_title, card_worker_id, card_column, card_kanban_id) VALUES ('Test Card 4', 0, 2, 1);
INSERT INTO card (card_title, card_worker_id, card_column, card_kanban_id) VALUES ('Test Card 5', 0, 3, 0);

