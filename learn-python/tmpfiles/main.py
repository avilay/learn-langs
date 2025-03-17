"""
TemporaryDirectory: This is the best option. I can create regular files in the directory and read them.
As soon as the function in which this directory was created goes out of scope, the directory gets deleted.

TemporaryFile: It is not visible to the rest of the filesystem. It seems the only use for this is
to write some data and then immeidately read it back while the file is still open for writes. Because
as soon as the file is closed, it gets deleted.

NamedTemporaryFile: Behaves like a regular file if set the `delete=False` option in the ctor. The
drawback with using this option is that I have to remember to delete the file. Which defeats the purpose
of using something like this in the first place.
"""
import tempfile
import os.path as path


def read_from_tempfile(fh):
    fh.seek(0)  # Have to move the file pointer to the first byte
    print("reading from tempfile")
    contents = fh.read()
    print(contents.decode("utf-8"))


def learn_tempfile():
    with tempfile.TemporaryFile() as f:
        print("Opened temporary file, but there is no name associated with it!")
        f.write(b"And the damn thing can only write bytes!\n")
        f.write(b"Not string.\n")
        f.write(b"I'd say this is pretty darn useless!\n")
        read_from_tempfile(f)

    print("TemporaryFile has now been deleted")


def read_from_named_tempfile(filename):
    print(f"Reading from {filename}")
    with open(filename, "rb") as fh:
        contents = fh.read()
    print(contents.decode("utf-8"))


def learn_named_tempfile():
    with tempfile.NamedTemporaryFile(delete=False) as f:
        print(f"Opened named temporary file {f.name}.")
        input("Check if you can see this on the filesystem.")
        f.write(b"Still only bytes!\n")
        f.write(b"Not string.\n")

    read_from_named_tempfile(f.name)


def read_file(filename):
    print(f"Reading file {filename}")
    with open(filename, "rt") as f:
        for line in f:
            print(line)


def learn_tempdir():
    tmpdir = tempfile.TemporaryDirectory()
    input(f"Check if dir {tmpdir.name} exists.")
    filename = path.join(tmpdir.name, "mytempfile.txt")
    with open(filename, "wt") as f:
        print("this seems like it will work", file=f)
        print("but will it?", file=f)
    read_file(filename)


def create_temp_dir():
    tmpdir = tempfile.TemporaryDirectory()
    input(f"Verify that the directory {tmpdir.name} got created.")


def verify_tmpdir_scope():
    dirname = create_temp_dir()
    input(f"Now the directory {dirname} should not exist anymore.")


# learn_tempfile()
# learn_named_tempfile()
# learn_tempdir()
verify_tmpdir_scope()

