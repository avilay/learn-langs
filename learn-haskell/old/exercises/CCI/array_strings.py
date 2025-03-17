import functools as fn
from collections import Counter  # used in is_palindrome_perm
import re  # used in is_palindrome_perm
import itertools as it


def test(f, cases, exp):
    act = list(map(f, cases))
    if exp == act:
        return f"{f.__name__}: Pass."
    failed = list(filter(lambda x: x[1] != x[2], zip(cases, exp, act)))
    return f"{f.__name__}: FAIL - {failed}"


def is_unique(string):
    return len(set(string)) == len(string)


def test_is_unique():
    cases = ["", "aa", "abc", "b", "aabbccc", "aabc"]
    exp = [True, False, True, True, False, False]
    return test(is_unique, cases, exp)


def are_perms(string1, string2):
    return sorted(string1) == sorted(string2)


def test_are_perms():
    cases = [("", ""), ("dog", "god"), ("hello", "world")]
    exp = [True, True, False]
    return test(lambda tpl: are_perms(tpl[0], tpl[1]), cases, exp)


def urlify(string):
    def _urlify(acc, char):
        return acc + "%20" if char == " " else acc + char

    return fn.reduce(_urlify, string.strip(), "")


def test_urlify():
    cases = ["hello world", "hello", "    hello    ", "", "  hello  world  "]
    exp = ["hello%20world", "hello", "hello", "", "hello%20%20world"]
    return test(urlify, cases, exp)


def is_palindrome_perm(string):
    canon = re.sub(r"\s+", "", string.lower())
    counts = Counter(canon).most_common()
    odd_chars = len(list(filter(lambda x: x[1] % 2 == 1, counts)))
    if len(canon) % 2 == 0:
        return odd_chars == 0
    return odd_chars == 1


def test_is_palindrome_perm():
    cases = ["Tact Coa", "Hello World"]
    exp = [True, False]
    return test(is_palindrome_perm, cases, exp)


def compress(string):
    counts = [(c, len(list(grp))) for c, grp in it.groupby(string)]
    strng = fn.reduce(lambda acc, x: f"{acc}{x[0]}{x[1]}", counts, "")
    return strng if len(string) > len(strng) else string


def test_compress():
    cases = ["aabcccccaaa", "aaaa", "abcd", "aabb"]
    exp = ["a2b1c5a3", "a4", "abcd", "aabb"]
    return test(compress, cases, exp)


def main():
    print(test_is_unique())
    print(test_are_perms())
    print(test_urlify())
    print(test_is_palindrome_perm())
    print(test_compress())


if __name__ == "__main__":
    main()
