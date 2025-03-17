#include <chrono>
#include <iostream>
#include <thread>

using namespace std;

// typedef the function pointer as a new type alias so I can use it easily
// everywhere
using Callback = void (*)(const string&);

// Using the funcptr type alias to declare my arg
void workAsync(int load, Callback callback) {
  cout << "Starting..." << endl;
  this_thread::sleep_for(chrono::seconds(load));
  cout << "Done" << endl;
  callback("my job is done");
}

// Not using the funcptr type alais
int pipeline1(int load, int (*nextFunc)(int)) {
  cout << "Pipeline1: Starting with load " << load << endl;
  this_thread::sleep_for(chrono::seconds(load));
  int result = rand() % 5;
  cout << "Pipeline1: Done with me." << endl;
  return nextFunc(result);
}

// Callbacks
void alert(const string& msg) {
  cout << "ALERT: " << msg << endl;
}

int pipeline2(int load) {
  cout << "Pipeline2: Starting with load " << load << endl;
  this_thread::sleep_for(chrono::seconds(load));
  int result = rand() % 5;
  cout << "Pipeline2: Done with me." << endl;
  return result;
}

int main() {
  // Seems like a more "modern" way of passing function pionters
  workAsync(2, alert);

  // The old school way of doing things
  int (*nextFunc)(int) = pipeline2;
  int result = pipeline1(2, nextFunc);
  cout << "Got result " << result << " from pipeline." << endl;
}
