#!/usr/bin/env python3
import sys

from VTFParser import parsevtf

###############################
def escape_str(s):
    '''escape_str(str) -> str

Escapes a string for GraphViz dot tool.
'''
    s = s.replace('"', '\\"')

    return s

###########################################
def to_dot(fd):
    '''to_dot(fd) -> _|_


'''
    parsed = parsevtf(fd)

    ########################################
    state_map = { }                        #
    state_cnt = 0                          #
    def get_state_id(name):                #
        nonlocal state_cnt                 #
        if name not in state_map:          #
            state_map[name] = state_cnt    #
            state_cnt += 1                 #
                                           #
        return str(state_map[name])        #
    ########################################

    print('Digraph G {\n')
    for trans in parsed.body:
        if len(trans) != 3:
            raise 'Invalid transition: ' + str(trans)

        str_trans = ''
        str_trans += get_state_id(trans[0])
        str_trans += ' -> '
        str_trans += get_state_id(trans[2])
        str_trans += ' [label="' + escape_str(trans[1]) + '"];'
        print(str_trans)

    init_cnt = 0
    for state in parsed.dict['Initial']:
        init_state_name = 'init' + str(init_cnt)
        init_cnt += 1

        str_init_state = ''
        str_init_state += init_state_name
        str_init_state += ' [label="",shape=plaintext];'
        print(str_init_state)

        str_init_trans = ''
        str_init_trans += init_state_name
        str_init_trans += ' -> '
        str_init_trans += get_state_id(state)
        str_init_trans += ';'
        print(str_init_trans)

    for state in state_map:
        str_state = ''
        str_state += get_state_id(state)
        str_state += ' [label="' + escape_str(state) + '"'

        if state in parsed.dict['Final']:
            str_state += ',peripheries=2'

        str_state += '];'
        print(str_state)

    print('}')


###############################
if __name__ == '__main__':
    argc = len(sys.argv)
    if argc == 1:
        fd = sys.stdin
    elif argc == 2:
        fd = open(sys.argv[1], "r")
    else:
        print("Invalid number of arguments: either 0 or 1 required")
        sys.exit(1)

    to_dot(fd)
    if argc == 2:
        fd.close()
