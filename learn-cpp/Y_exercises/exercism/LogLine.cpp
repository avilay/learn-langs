// clang++ -std=c++20 -Wall -o ~/Desktop/temp/excercism LogLine.cpp

#include "Utils.h"
#include <iostream>
#include <string>

using namespace std;

namespace LogLine {

string _parse(string line, string (*callback)(string, string))
{
    if (line.empty()) {
        throw runtime_error("Empty log line!");
    }

    string logLevels[] {
        "ERROR",
        "WARNING",
        "INFO"
    };

    for (string logLvl : logLevels) {
        string level = "[" + logLvl + "]:";
        if (line.find(level) == 0) {
            return callback(logLvl, line);
        }
    }

    throw runtime_error("Unknown log level!");
}

string _message(string logLvl, string line)
{
    string level = "[" + logLvl + "]:";
    return strip(line.substr(level.length()));
}

string message(string line)
{
    return _parse(line, _message);
}

string _logLevel(string logLvl, string line)
{
    return logLvl;
}

string logLevel(string line)
{
    return _parse(line, _logLevel);
}

string _reformat(string logLvl, string line)
{
    string level = "[" + logLvl + "]:";
    string msg = strip(line.substr(level.length()));
    return msg + " (" + logLvl + ")";
}

string reformat(string line)
{
    return _parse(line, _reformat);
}
}

void testMessage()
{
    string logs[] {
        "[ERROR]: Invalid operation",
        "[WARNING]: Maybe invalid op",
        "[INFO]: Perfectly valid op",
        "[INFO]:No spaces between level and message",
        "[INFO]: Message has : in them",
        "[INFO]: Message has [square brackets] in them",
        "[INFO]: Message: has [brackets] and :s in them",
        "[INFO]: Message with trailing spaces   ",
        "[INFO]:"
    };

    string expMsgs[] {
        "Invalid operation",
        "Maybe invalid op",
        "Perfectly valid op",
        "No spaces between level and message",
        "Message has : in them",
        "Message has [square brackets] in them",
        "Message: has [brackets] and :s in them",
        "Message with trailing spaces",
        ""
    };

    int len = end(logs) - begin(logs);

    string actMsg {};
    for (int i = 0; i < len; i++) {
        string log = logs[i];
        string expMsg = expMsgs[i];
        actMsg = LogLine::message(log);
        assert(expMsg == actMsg, "Expected: " + expMsg + "\nGot: " + actMsg);
    }

    string badLogs[] {
        "No log level",
        "[INFO] No separator between level and message",
        "[CRITICAL]: Wrong level",
        "",
        "INFO: No square brackets around level",
        "INFO No square brackets or separator between level and message"
    };

    for (string badLog : badLogs) {
        try {
            LogLine::message(badLog);
            assert(false, "No error for: " + badLog);
        } catch (runtime_error err) {
            assert(true, "");
        }
    }
}

void testLogLevel()
{
    pair<string, string> testCases[] {
        pair<string, string> {
            "[ERROR]: Invalid operation",
            "ERROR" },
        pair<string, string> {
            "[WARNING]: Maybe invalid op",
            "WARNING" },
        pair<string, string> {
            "[INFO]: Perfectly valid op",
            "INFO" },
        pair<string, string> {
            "[INFO]:No spaces between level and message",
            "INFO" },
        pair<string, string> {
            "[INFO]:",
            "INFO" }
    };

    for (auto testCase : testCases) {
        auto log = testCase.first;
        auto expLevel = testCase.second;
        string actLevel = LogLine::logLevel(log);
        assert(expLevel == actLevel, "Expected: " + expLevel + "\nGot: " + actLevel);
    }

    string badLogs[] {
        "No log level",
        "[INFO] No separator between level and message",
        "[CRITICAL]: Wrong level",
        "",
        "INFO: No square brackets around level",
        "INFO No square brackets or separator between level and message"
    };

    for (string badLog : badLogs) {
        try {
            LogLine::logLevel(badLog);
            assert(false, "No error for: " + badLog);
        } catch (runtime_error err) {
            assert(true, "");
        }
    }
}

void testReformat()
{
    pair<string, string> testCases[] {
        pair<string, string> {
            "[ERROR]: Invalid operation",
            "Invalid operation (ERROR)" },
        pair<string, string> {
            "[WARNING]: Maybe invalid op",
            "Maybe invalid op (WARNING)" },
        pair<string, string> {
            "[INFO]: Perfectly valid op",
            "Perfectly valid op (INFO)" },
        pair<string, string> {
            "[INFO]:No spaces between level and message",
            "No spaces between level and message (INFO)" },
        pair<string, string> {
            "[INFO]:",
            " (INFO)" }
    };

    for (auto testCase : testCases) {
        auto log = testCase.first;
        auto expLevel = testCase.second;
        string actLevel = LogLine::reformat(log);
        assert(expLevel == actLevel, "Expected: " + expLevel + "\nGot: " + actLevel);
    }
}

void testLogLine()
{
    testMessage();
    testLogLevel();
    testReformat();
}