DELETE FROM doge_name;
DELETE FROM wallet;
DELETE FROM pack_member;
DELETE FROM pack;
DELETE FROM doge;


INSERT INTO doge (id) VALUES (0);
INSERT INTO doge (id) VALUES (1);
INSERT INTO doge (id) VALUES (2);
INSERT INTO doge (id) VALUES (3);
INSERT INTO doge (id) VALUES (4);
INSERT INTO doge (id) VALUES (5);

INSERT INTO doge_name (doge_id, doge_name) VALUES (0, 'Biscuit');
INSERT INTO doge_name (doge_id, doge_name) VALUES (1, 'Coco');
INSERT INTO doge_name (doge_id, doge_name) VALUES (2, 'Gizmo');
INSERT INTO doge_name (doge_id, doge_name) VALUES (3, 'Molly');
INSERT INTO doge_name (doge_id, doge_name) VALUES (4, 'Smokey');
INSERT INTO doge_name (doge_id, doge_name) VALUES (5, 'MC Shibe');

INSERT INTO wallet (doge_id, coins) VALUES (0, 0);
INSERT INTO wallet (doge_id, coins) VALUES (0, 200);
INSERT INTO wallet (doge_id, coins) VALUES (1, 1);
INSERT INTO wallet (doge_id, coins) VALUES (1, 200);
INSERT INTO wallet (doge_id, coins) VALUES (1, 200);
INSERT INTO wallet (doge_id, coins) VALUES (2, 1);
INSERT INTO wallet (doge_id, coins) VALUES (2, 100);
INSERT INTO wallet (doge_id, coins) VALUES (3, 300);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);
INSERT INTO wallet (doge_id, coins) VALUES (5, 100);

INSERT INTO pack (id, pack_name) VALUES (0, 'Wow Cr3w');
INSERT INTO pack (id, pack_name) VALUES (1, 'no cats plz');

INSERT INTO pack_member (pack_id, doge_id) VALUES (0, 0);
INSERT INTO pack_member (pack_id, doge_id) VALUES (1, 0);
INSERT INTO pack_member (pack_id, doge_id) VALUES (0, 1);
INSERT INTO pack_member (pack_id, doge_id) VALUES (1, 1);
INSERT INTO pack_member (pack_id, doge_id) VALUES (1, 2);
INSERT INTO pack_member (pack_id, doge_id) VALUES (1, 3);
INSERT INTO pack_member (pack_id, doge_id) VALUES (1, 4);
