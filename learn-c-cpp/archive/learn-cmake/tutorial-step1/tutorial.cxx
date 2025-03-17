#include <iostream>
#include <string>
#include "TutorialConfig.h"
#include "MathFunctions.h"

int main(int argc, char **argv) {
	if (argc < 2) {
		std::cout << argv[0] << " Version " << Tutorial_VERSION_MAJOR << "." << Tutorial_VERSION_MINOR << std::endl;
		std::cout << "Usage: " << argv[0] << " number" << std::endl;
		return 1;
	}
	
	const double input_val = std::stod(argv[1]);
	// const double output_val = sqrt(input_val);
	const double output_val = mysqrt(input_val);
	std::cout << "The square root of " << input_val << " is " << output_val << std::endl;
	return 0;
}
