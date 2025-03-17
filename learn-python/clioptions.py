import argparse


def main():
    parser = argparse.ArgumentParser()

    # Required positional (i.e., un-named) arguments. It is not possible to have positional arguments that are optional
    parser.add_argument('reqpos', metavar='REQPOS', help='This is a required positional argument.')

    # Optional named arguments
    parser.add_argument('-o', '--optnamed', help='This is an optional named argument.')

    # Getting int arguments with the optional default param
    parser.add_argument('-n', '--number', type=int, default=0, help='This is an integer argument.')

    # Getting bool arguments defaulting to true
    parser.add_argument('-f', '--flag', action='store_true', help='This is a boolean argument.')

    # Setting valid values for arguments
    parser.add_argument('color', metavar='COLOR', choices=['red', 'green', 'blue'], help='This is a scoped argument.')

    args = parser.parse_args()
    print('reqpos = {}'.format(args.reqpos))
    print('optnamed = {}'.format(args.optnamed))
    print('type of number = {}  number = {}'.format(type(args.number), args.number))
    print('type of flag = {}  flag = {}'.format(type(args.flag), args.flag))
    print('color = {}'.format(args.color))


if __name__ == '__main__': main()
