#!/usr/bin/env python3
import sys

############################
class ParsedAut:
    '''ParsedAut

Result of an automaton parser.
'''
    def __init__(self):
        self.final = set()
        self.trans = set()
        self.alphabet = set()
        self.states = set()
        self.name = ''

    def __str__(self):
        return self.serialize()

    def serialize(self):
        result = 'Ops ' + ' '.join(self.alphabet)
        result += '\n\n'
        result += 'Automaton ' + self.name + '\n'
        result += 'States ' + ' '.join(self.states) + '\n'
        result += 'Final States ' + ' '.join(self.final) + '\n'
        result += 'Transitions\n'
        for (children, symb, parent) in self.trans:
            if children == None:
                result += symb + ' -> ' + parent + '\n'
            else:
                result += symb + '(' + children + ') -> ' + parent + '\n'

        return result


###########################################
def parse_trans(line):
    '''parse_trans(line) -> (src, symb, tgt)

Parses a transition.
'''
    lhs, rhs = line.split('->')
    lhs = lhs.strip()
    rhs = rhs.strip()

    ind = lhs.find('(')
    if ind == -1:
        return (None, lhs, rhs)

    if lhs[-1] != ')':
        raise Exception('Invalid left-hand side: ' + line)

    symb = lhs[:ind]
    src = lhs[ind+1:-1]
    src = src.strip()
    if not src:  # no child state
        return (None, symb, rhs)

    src_split = src.split(',')
    src_split = [state.strip() for state in src_split]

    return (tuple(src_split), symb, rhs)


##########################################
def parsetimbuk(fd):
    '''parsetimbuk(fd) -> ParsedAut

Parses the input from the file descriptor 'fd'.
'''
    result = ParsedAut()

    parsed_transitions = False
    while True:
        line = fd.readline()
        if not line:
            # end of file
            if not parsed_transitions:
                raise Exception("Incomplete automaton")
            else:
                return result

        line = line.strip()
        if not line:
            # empty string
            continue

        if parsed_transitions:
            # a transition
            trans = parse_trans(line)
            result.trans.add(trans)
        elif line == 'Transitions':
            parsed_transitions = True
        elif line.startswith('Automaton '):
            _, result.name = line.split(' ', 1)
        elif line.startswith('Ops '):
            _, ops = line.split(' ', 1)
            ops_list = [x.strip() for x in ops.split(' ')]
            result.alphabet.update(ops_list)
        elif line.startswith('States '):
            _, states = line.split(' ', 1)
            states_list = [x.strip() for x in states.split(' ')]
            result.states.update(states_list)
        elif line.startswith('Final States '):
            _, _, fin_states = line.split(' ', 2)
            fin_states_list = [x.strip() for x in fin_states.split(' ')]
            result.final.update(fin_states_list)
        else:
            print('Not processed: ' + line)


##########################
if __name__ == '__main__':
    argc = len(sys.argv)
    if argc == 1:
        fd = sys.stdin
    elif argc == 2:
        fd = open(sys.argv[1], "r")
    else:
        print("Invalid number of arguments: either 0 or 1 required")
        sys.exit(1)

    parsed_aut = parsetimbuk(fd)
    print(parsed_aut)

    if argc == 2:
        fd.close()
