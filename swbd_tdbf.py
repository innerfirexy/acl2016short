#!/usr/local/bin/python3
# compute td and bf columns of entropy table, swbd db
# Yang Xu
# 2/23/2016

from nltk.tree import *

import MySQLdb
import sys
import numpy
import time

import multiprocessing
from multiprocessing import Pool, Manager


# get db connection
def db_conn(db_name):
    # db init: ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
    conn = MySQLdb.connect(host = "127.0.0.1",
                    user = "yang",
                    port = 3306,
                    passwd = "05012014",
                    db = db_name)
    return conn

# compute td and bf columns
def compute_tdbf():
    conn = db_conn('swbd')
    cur = conn.cursor()
    # select keys and parsed from table
    sql = 'SELECT convID, globalID, parsed FROM entropy'
    cur.execute(sql)
    data = cur.fetchall()
    # initialize
    pool = Pool(multiprocessing.cpu_count())
    manager = Manager()
    queue = manager.Queue()
    # mp
    args = [(d, queue) for d in data]
    result = pool.map_async(compute_tdbf_worker, args, chunksize=5000)
    # manager loop
    while True:
        if result.ready():
            print('\n all rows processed')
            break
        else:
            sys.stdout.write('\r{}/{} processed'.format(queue.qsize(), len(args)))
            sys.stdout.flush()
            time.sleep(1)
    # update
    processed_results = result.get()
    for i, res in enumerate(processed_results):
        c_id, g_id, td, bf = res
        sql = 'UPDATE entropy SET td = %s, bf = %s WHERE convID = %s AND globalID = %s'
        cur.execute(sql, (td, bf, c_id, g_id))
        if i % 999 == 0 and i > 0:
            sys.stdout.write('\r{}/{} updated'.format(i+1, len(processed_results)))
            sys.stdout.flush()
    conn.commit()

# worker func for compute_tdbf
def compute_tdbf_worker(args):
    (c_id, g_id, parsed_str), queue = args
    # construct the full tree, and get sub_tree
    full_tree = Tree.fromstring(parsed_str)
    sub_tree = full_tree[0] if full_tree.label() == 'ROOT' else full_tree
    # compute td and bf
    td = sub_tree.height()
    bf = numpy.mean([len(t) for t in sub_tree.subtrees()])
    # capsulate and return
    queue.put(1)
    return (c_id, g_id, td, bf)


# main
if __name__ == '__main__':
    compute_tdbf()
