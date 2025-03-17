#!/usr/bin/python

import sqlite3
import psycopg2 as pg
import psycopg2.extras as pgextras


def from_postgres():
    conn_str = 'postgres://happy:orange@127.0.0.1/sample_db'
    conn = pg.connect(conn_str)
    cur = conn.cursor(cursor_factory=pgextras.DictCursor)
    cur.arraysize = 2  # fetch 2 rows at once from the db
    cur.execute('SELECT * FROM favorites')
    rows = cur.fetchmany()
    while len(rows) > 0:
        print('Fetched {} rows from db'.format(len(rows)))
        for row in rows:
            print('\nROW')
            for col in row.keys():
                print('{}: {}'.format(col, row[col]))
        rows = cur.fetchmany()


def from_sqlite():
    db = sqlite3.connect("test.db")
    db.row_factory = sqlite3.Row
    db.execute("drop table if exists test")
    db.execute("create table test (t1 text, i1 int)")
    db.execute("insert into test (t1, i1) values (?, ?)", ('one', 1))
    db.execute("insert into test (t1, i1) values (?, ?)", ('two', 2))
    db.execute("insert into test (t1, i1) values (?, ?)", ('three', 3))
    db.execute("insert into test (t1, i1) values (?, ?)", ('four', 4))
    db.commit()
    print("Database created")
    cursor = db.execute("select * from test order by t1")
    for row in cursor:
        print(dict(row))
        print(row["i1"])


if __name__ == "__main__": from_postgres()
