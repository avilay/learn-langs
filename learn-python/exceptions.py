def catching(filename):
    try:
        for line in throwing(filename):
            print(line.strip())
    except IOError as e:
        print("This is the catch block")
        print("Could not open file: ", e)
    except ValueError as e:
        print("This is the catch block")
        print("Bad filename: ", e)
    else:
        print("This is the finally block")


def throwing(filename):
    if filename.endswith(".txt"):
        fh = open(filename)
        return fh.readlines()
    else:
        # Todo: need to find out how to wrap exceptions before throwing
        raise ValueError("Filename must end with .txt")


def main():
    catching("tp.txt")
    catching("tp1.doc")


if __name__ == "__main__": main()
