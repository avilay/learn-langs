echo "Compile the Utils.cpp file"
clang++ -std=c++20 -c -Wall -I. -o ../bin/Utils.o Utils.cpp

echo "Compile the Main.cpp file"
clang++ -std=c++20 -c -Wall -I. -o ../bin/Main.o Main.cpp

echo "Link the object files to produce the final executable"
clang++ -std=c++20 -Wall -I. -o ../bin/linkage-demo ../bin/Utils.o ../bin/Main.o

echo "Run the newly built executable\n"
../bin/linkage-demo
