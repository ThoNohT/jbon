# Jbon: Javascript Binary Object Notation

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

A Jbon file is divided into different sections, these sections are explained under the following headings.

### Identifier header

The jbon file format starts with a header with the text "JBON".

### Settings header

After that are three bytes containing the encoding settings, that specify how many bytes are used for specific numbers. Each setting is a pair of two bits that can have the following meaning:

```
00: 1 byte for the number (8-bit word).
01: 2 bytes for the number (16-bit word).
10: 4 bytes for the number (32-bit word).
11: 8 bytes for the number (64 bit word).
```

The bytes are laid out as follows: `AABBCCDDEEFFGGHH______II`, where the letters indicate the positions of the settings for the following types of numbers in a Jbon file.

- `AA`: The number of references, and identifiers of references.
- `BB`: The size of the decimal parts of numbers.
- `CC`: The size of the integer parts of numbers.
- `DD`: The length of object names.
- `EE`: The length of strings.
- `FF`: The length of arrays.
- `GG`: The number of fields in objects, and identifiers of fields.
- `HH`: The number of objects definitions, and identifiers of object definitions,
- `II`: The size of the exponent parts of numbers.

### Object definitions header

The next part is the object definitions header. It starts with a number (size `HH`) that indicates the number of object definitions that are defined. After that follow the same number of object definitions. The indexes of definitions are not included, the first definition will get index 1, the next 2, and so on.

Each object definition follows the following format:
- First an optional index (size `HH`) of an object definition from which to inherit. If the object doesn't inherit, this number is 0.

- Second is a number (size `GG`) indicating the number of fields in the object.

- After that follow the same number of field definitions. Each field definition consists of two parts.
  - A field index number (size `GG`), indicating at which point to insert the field in the object definition from which is being inherited. `0` means that the field is inserted at the start, higher numbers indicate that the fields are inserted after the field with this index in the object definition on which this one depends. If multiple field indexes are the same, they are appended at the appropriate place in order.
  - The name of this field, starting with a number (size `DD`) indicating the length of the string, followed by the same number of bytes that form the string representing the name of the field.

### References header

Some values occur multiple times in a Json document, so rather than repeating them, this header contains them once, with an index, so this value can be referenced in the actual value. Values in the references header can also contain references, and the order of the indexes in this header is undefined.

The header starts with a number (size `AA`) indicating the number of references that there are. After that follow the same number of references. The indexes of the references are included.

Each reference follows the following format:
- First a number (size `AA`) indicating the index of the reference.

- Then the value for the reference. The format of values is specified in the next section.

### Json value

The last section is the actual value. Every Json document contains a single value. So this section starts off with encoding this value. The format described in this section is also used for the values in the references header.

The first byte specifies what type of value follows. The following types are recognized:

#### Primitives

Some types do not need any extra data and are directly complete by their type specifier.

- 0x0: null
- 0x1: false (boolean)
- 0x2: true (boolean)

#### Values with extra data

Other types need some more information to be complete.

- 0x3 to 0xE: numbers. The type of number can be determined by subtracting 3 from the type value. Let's call this value `x`. The type of the number then follows the following rules.
  - `x % 2 = 0`: The number is positive.
  - `x % 2 = 1`: The number is negative.
  - `x % 4 < 2`: The number is an integer.
  - `x % 4 >= 2`: The number is a decimal.
  - `x >= 4 & x < 8`: The number has an exponent, which is positive.
  - `x >= 8 & x < 12`: The number has an exponent, which is negative.

  Following this first number is a number (size `CC`), which is the integer part of the number.
  If the number is a decimal, another number follows (size `BB`), which is the decimal part of the number.
  If the number has an exponent, another number follows (size `II`), which is the exponent of the number.
- 0xF: A string, starting with a number (size `EE`) indicating the length of the string, followed by the same number of bytes that form the string.
- 0x12: A reference. Contains a number (size `AA`), that indicates the index of the reference.

#### Composite types.

Finally the values that are composed of other values, the information of the contained values is nested in the data.

- 0x10: An array. The first number (size `FF`) indicates the number of elements in the array. Afer this number, the same number of values are encoded.
- 0x13: A counted array. The first number (size `FF`) indicates the number of elements in the array. Afer this number, the same number of numbers (size `FF`) followed by values are encoded. Each pair of number and value indicate this number of times this value consecutively in the array.
- 0x11: An object.
  - The first nuber (size `HH`) specifies the index of the object definition, pointing to a value from the object definitions header (1-based).
  - The definition indicates how many fields there are, the fields are filled in, by order of their index. The same number of values are encoded after the index.

### Remaining data

Every section specifies upfront how long its data is going to be. Parsing will stop after the end of the main value is reached, even if there is data after it. A Jbon value will not be encoded with data after the Json value section, but if data is manually added there, it will be ignored.
