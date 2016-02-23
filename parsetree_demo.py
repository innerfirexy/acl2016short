#!
# Code for generating Figure1, showcasing the difference in tree depth and branching factor
# between simple vs. complex sentences
# Yang Xu
# 2/23/2016

from nltk.tree import *
import numpy as np

# main
if __name__ == '__main__':
    # parsed sentences
    ss = '(S (NP (PRP I)) (VP (MD can) (VP (VB imagine))) (. .))'
    ls = '(S (NP (PRP I)) (VP (VBP \'m) (RB not) (ADJP (JJ sure)) (SBAR (WHADVP (RB exactly) (WRB where)) (S (NP (NNP DANCES) (NNP WITH) (NNP WOLVES)) (VP (VBD was) (VP (VBN filmed)))))))'

    # to tree
    t1 = Tree.fromstring(ss)
    t2 = Tree.fromstring(ls)

    # print latex qtree format
    print(t1.pformat_latex_qtree())
    print(t2.pformat_latex_qtree())

    # tree depth
    print(t1.height())
    print(t2.height())

    # branching factor
    print(np.mean([len(t) for t in t1.subtrees()]))
    print(np.mean([len(t) for t in t2.subtrees()]))
