import sqlglot
import sqlglot.executor

database = { "my_table": [{ "my_column": 42 }] }

my_statement = sqlglot.parse_one("SELECT * from my_table")

print(sqlglot.executor.execute(my_statement, tables=database))