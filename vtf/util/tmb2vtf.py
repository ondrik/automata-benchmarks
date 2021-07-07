#!/usr/bin/env python3
import sys

import TMBParser
import VTFParser

###########################################
def print_use(prog_name):
    '''print_use(prog_name) -> ()

Prints the program use
'''
    print('use: ' + prog_name + ' (--fa|--ta) [<input>]\n')


###########################################
if __name__ == '__main__':
    argc = len(sys.argv)
    is_input_stdin = False
    if argc == 2:
        fd = sys.stdin
        is_input_stdin = True
    elif argc == 3:
        fd = open(sys.argv[2], "r")
    else:
        print_use(sys.argv[0])
        sys.exit(1)

    is_ta = False
    if sys.argv[1] == '--fa':
        pass
    elif sys.argv[1] == '--ta':
        is_ta = True
    else:
        print_use(sys.argv[0])
        sys.exit(1)

    parsed_aut = TMBParser.parsetimbuk(fd)
    vtf_aut = VTFParser.ParsedVTF()


    vtf_aut.type = 'NTA' if is_ta else 'NFA'
    if is_ta:
        vtf_aut.dict['Root'] = parsed_aut.final
    else:
        vtf_aut.dict['Initial'] = set()
        vtf_aut.dict['Final'] = parsed_aut.final

    vtf_aut.dict['States'] = parsed_aut.states
    vtf_aut.dict['Alphabet'] = parsed_aut.alphabet

    for trans in parsed_aut.trans:
        (children, symb, parent) = trans
        if is_ta:
            if children == None:
                children = ''
            # children = [child.replace(' ', '') for child in children]
            line = [parent, symb, '(', children, ')']
            vtf_aut.body.append(line)
        else:
            if children == None:
                vtf_aut.dict['Initial'].add(parent)
            else:
                vtf_aut.body.append(trans)

    print(vtf_aut)

    if not is_input_stdin:
        fd.close()
