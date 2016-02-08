#/usr/local/bin/python3
# Add boundaryType column to entropy table in swbd db
# Yang Xu

import MySQLdb

# get db connection
def db_conn(db_name):
    # db init: ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
    conn = MySQLdb.connect(host = "127.0.0.1",
                    user = "yang",
                    port = 3306,
                    passwd = "05012014",
                    db = db_name)
    return conn

# define the func that does the job
def add_boundary_type():
    
    pass
