# Jbon: Javascript Binary Object Nottion

A command line tool to encode and decode json value into a more compact binary format.

## Usage

Jbon can take input from a file, or from standard input. If the `-i` flag is used then the specified filename is used as input, otherwise standard input. If the `-o` flag is used then the specified filename is used to output to otherwise standard output.

Examples:
```bash
$ jbon encode -i test.json -o test.jbon
$ cat test.json | jbon decode -o test.jbon
$ jbon decode -i test.jbon -f
```

For more information on how how to use it, use `jbon help`, or about a specific command: `jbon help <command>`.

## Format

Todo.
