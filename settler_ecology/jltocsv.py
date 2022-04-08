#!/usr/bin/env python

import csv
import json
import sys
import argparse
import dataclasses as dc


@dc.dataclass(frozen=True)
class Field:
    target: str
    header: str
    default: str

    @classmethod
    def parse(cls, defn):
        parts = defn.strip().split(":")
        if len(parts) == 1:
            return Field(parts[0], parts[0], "")
        if len(parts) == 2:
            return Field(parts[0], parts[1], "")
        if len(parts) != 3:
            raise ValueError(f"invalid field definition: {defn}")
        return Field(parts[0], parts[1] or parts[0], parts[2])


def parse_args(args=None, parse=True):
    """Parse command line arguments."""

    parser = argparse.ArgumentParser(description="Convert json lines file to CSV")
    parser.add_argument(
        "file", help="Path to json lines file", type=argparse.FileType("r")
    )
    parser.add_argument(
        "--output",
        "-o",
        help="Path to output file",
        default=sys.stdout,
        type=argparse.FileType("w"),
    )
    parser.add_argument("--field", "-f", action="append", help="field specifier")

    return parser.parse_args(args) if parse else None, parser


def extract_field(field, data):
    """Extract a value from `data` as specified by `field`."""

    return data.get(field.target, field.default)


def extract_data(fields, data):
    """Extract all values from `data` as specified by `fields`."""

    return {f.header: extract_field(f, data) for f in fields}


def main():
    args, _parser = parse_args()

    fields = [Field.parse(f) for f in args.field]

    header = [f.header for f in fields]
    writer = csv.DictWriter(args.output, header, delimiter=",", quoting=csv.QUOTE_ALL, extrasaction="ignore")
    writer.writeheader()

    for line in args.file:
        if not line.strip():
            continue
        data = json.loads(line)
        writer.writerow(extract_data(fields, data))


if __name__ == "__main__":
    main()
else:
    import unittest

    class Tests(unittest.TestCase):
        def test_field_parse(self):
            tests = [
                (["foo", "foo", ""], "foo"),
                (["foo", "bar", ""], "foo:bar"),
                (["foo", "bar", "baz"], "foo:bar:baz"),
                (["foo", "foo", "baz"], "foo::baz"),
            ]

            for expected, input in tests:
                self.assertEqual(Field(*expected), Field.parse(input))

        def test_extract_field(self):
            data = {"foo": 1, "bar": 2, "baz": 3}
            tests = [
                (1, "foo"),
                (2, "bar:bar2"),
                ("missing", "spam::missing"),
            ]
            for expected, input in tests:
                self.assertEqual(expected, extract_field(Field.parse(input), data))

        def test_extract_data(self):
            data = {"foo": 1, "bar": 2, "baz": 3}
            fs = [
                Field.parse("foo"),
                Field.parse("bar:bar2"),
                Field.parse("spam::missing"),
            ]
            self.assertEqual(
                {"foo": 1, "bar2": 2, "spam": "missing"}, extract_data(fs, data)
            )
