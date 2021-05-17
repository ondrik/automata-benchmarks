#!/usr/bin/env python3
import re
import sys

from enum import Enum

###########################################
def serialize_token(token):
    '''serialize_token(token) -> str

Serializes the token (including adding necessary escape sequences).
'''
    result = ''
    if re.search('["# \t]', token):
        result += '"'
        for ch in token:
            if ch == '"':
                result += '\\' + ch
            else:
                result += ch

        result += '"'
    else:
        result += token

    return result

##########################################
# TODO: only parses a single segment in a file


###########################################
class ParsedVTF:
    '''ParsedVTF

Result of a VTF format parser.
'''
    def __init__(self):
        self.type = ''
        self.dict = { }
        self.body = []

    def __str__(self):
        return self.serialize()

    def serialize(self):
        result = '@' + self.type + '\n'
        for k,v in self.dict.items():
            result += '%' + k

            for item in v:
                result += ' ' + serialize_token(item)

            result += '\n'

        result += '\n'
        for line in self.body:
            result += ' '.join([serialize_token(item) for item in line])
            result += '\n'

        return result



###############################
def tokenize(line):
    assert line
    assert line == line.strip()
    # TODO: fix this

    class ParserState(Enum):
        INIT = 0,
        UNQUOTED = 1,
        QUOTED = 2,
        QUOTED_ESCAPE = 3

    state = ParserState.INIT
    token_list = []
    token = ''
    for ch in line:
        if state == ParserState.INIT:
            if ch in { ' ', '\t'}:
                continue
            elif ch == '(':
                token_list.append(ch)
                continue;
            elif ch == '"':
                state = ParserState.QUOTED
                continue
            elif ch == '#':
                break
            else:
                token += ch
                state = ParserState.UNQUOTED
                continue
        elif state == ParserState.UNQUOTED:
            if ch in { ' ', '\t', '#' }:
                token_list.append(token)
                token = ''
                if ch == '#':
                    break
                else:
                    state = ParserState.INIT
                    continue
            elif ch == '"':
                raise Exception("Invalid format: " + line)
            else:
                token += ch
                continue
        elif state == ParserState.QUOTED:
            if ch == '\\':
                state = ParserState.QUOTED_ESCAPE
                continue
            elif ch == '"':
                token_list.append(token)
                token = ''
                state = ParserState.INIT
                continue
            else:
                token += ch
        elif state == ParserState.QUOTED_ESCAPE:
            if ch == '"':
                token += ch;
            else:
                token += '\\' + ch

            state = ParserState.QUOTED
            continue
        else:
            assert "Invalid parser state: " + str(state)

    if state in { ParserState.QUOTED, ParserState.QUOTED_ESCAPE }:
        raise Exception("Missing end of quotes " + line)

    if token:
        token_list.append(token)

    return token_list

###############################
def getline(fd):
    '''getline(fd) -> [str]

Reads a line from the file descriptor 'fd' and parses it into an array of tokens.
'''
    while True:
        line = fd.readline()
        if not line:
            return None

        while line.endswith('\\\n'):
            line = line[:-2]
            newline = fd.readline()
            if not newline:
                raise Exception('Cannot continue beyond the end of file')
            line += newline

        line = line.strip()
        if not line:
            # empty string
            continue

        if re.match("^#", line) is not None:
            # whole-line comments
            continue

        return tokenize(line)

###########################################
def parsevtf(fd):
    '''parsevtf(fd) -> ParsedVTF

Parses the input from the file descriptor 'fd'.
'''
    result = ParsedVTF()

    type_parsed = False
    while True:
        line = getline(fd)
        if not line:
            # end of file
            break

        if line[0][0] == '@':
            if type_parsed:
                raise Exception('Type already parsed before: "' + \
                    result.type + '"; new type: ' + line[0][1:])
            if len(line) > 1 or len(line[0]) == 1:
                raise Exception('Invalid type: ' + line)

            result.type = line[0][1:]
            type_parsed = True

        elif line[0][0] == '%':
            if len(line[0]) == 1:
                raise Exception("Invalid key: " + line)

            key = line[0][1:]

            if (key in result.dict):
                result.dict[key].extend(line[1:])
            else:
                result.dict[key] = line[1:]
        else:
            result.body.append(line)

    if not type_parsed:
        raise Exception("Could not find a @TYPE directive")

    return result


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

    parsed_aut = parsevtf(fd)
    print(parsed_aut)

    if argc == 2:
        fd.close()
