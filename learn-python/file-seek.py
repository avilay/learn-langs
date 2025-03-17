import os


def main():
    currdir = os.path.dirname(os.path.abspath(__file__))
    filename = os.path.join(currdir, 'resources', 'duke_of_york.txt')
    with open(filename) as f:
        f.readline()
        for line in f:
            print(line.strip())


if __name__ == '__main__': main()
