from uuid import uuid4
import sys


def super_fancy_main():
    from haikunator import Haikunator
    from snippets.color_printer import print_code

    name = Haikunator().haikunate()
    print_code(f"The name of this program is {name}")


def fancy_main():
    from snippets.color_printer import print_code

    name = uuid4()
    print_code(f"The name of this program is {name}")


def main():
    name = uuid4()
    print(f"The name of this program is {name}")


if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1] == "fancy":
            fancy_main()
        elif sys.argv[1] == "super-fancy":
            super_fancy_main()
        else:
            raise ValueError("Unknown option!")
    else:
        main()
