import sqlglot
import sqlglot.expressions as exp

sample_query1 = """
SELECT department, sum(salary) as total_salary
FROM artists
GROUP BY department
HAVING total_salary > 100000
"""

sample_database = {
    "artists":
        [{
            "name": "michael",
            "department": "computing",
            "salary": 1000
        },
         {
            "name": "john",
            "department": "computing",
            "salary": 2000
        },
        {
            "name": "jane",
            "department": "art",
            "salary": 500
        },
        {
            "name": "matthias",
            "department": "art",
            "salary": 2000000
        }]
}
