#!/usr/local/bin/python3
# compute td and bf columns of entropy table, swbd db
# Yang Xu
# 2/23/2016

from nltk.tree import *

import MySQLdb
import sys
import numpy

# get db connection
def db_conn(db_name):
    # db init: ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
    conn = MySQLdb.connect(host = "127.0.0.1",
                    user = "yang",
                    port = 3306,
                    passwd = "05012014",
                    db = db_name)
    return conn

# main
if __name__ == '__main__':
    