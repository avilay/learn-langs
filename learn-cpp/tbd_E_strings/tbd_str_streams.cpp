/*
 * Use of string stream as a StringBuilder
 * Use a better alternative to std::to_string
 * Iterator through a string stream with ss.fail() being the stopping condition
 * But then it will print the last element twice so use ss >> a in the condition
 */