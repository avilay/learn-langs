def simple_read(filename):
    fh = open(filename)  # defaults to read mode

    all_lines = fh.readlines()
    print(all_lines)

    # Line by line
    for line in fh:
        print(line, end="")
    print()

    # Another way to read line by line
    for line in fh.readlines():
        print(line, end="")
    print()


def write(filename):
    fh = open(filename, "wt")
    # fh = open(filename, "w") _t_ext mode is default
    poem = [
        "Oh the grand old Duke Of York",
        "He had ten thousand men.",
        "He led them up to the top of the hill",
        "And he led them down again.",
    ]
    for line in poem:
        print(line, file=fh)
    print("Done")


def buffered_read_write(infile, outfile):
    infh = open(infile, "rb")
    outfh = open(outfile, "wb")
    buffer_size = 1000  # read the file in memory in chunks of 1000 bytes
    buffr = infh.read(buffer_size)
    while len(buffr):
        outfh.write(buffr)
        print(".", end="")
        buffr = infh.read(buffer_size)
    print()
    print("Done")


def main():
    simple_read("tp.txt")
    write("duke_of_york.txt")
    buffered_read_write("informion_logo.png", "new.png")


if __name__ == "__main__":
    main()
