# Predoc

Predoc is both a Markdown flavour specifically designed to for writing manpages
and a command-line tool for converting a Predoc document into a manpage written
in mdoc.

> [!WARNING]
> Predoc is in an alpha stage of development. There are likely to be bugs and
> gaps in its implementation.

## Installation

Clone the repository into `./predoc` and run `janet --install predoc`.

## Usage

To convert a manpage for the program `example` from Predoc to mdoc:

```
pandoc --output example.1 example example.1.predoc
```

## Bugs

Found a bug? I'd love to know about it. The best way is to report your bug in
the [Issues][] section on GitHub.

[Issues]: https://github.com/pyrmont/predoc/issues

## Licence

Predoc is licensed under the MIT Licence. See [LICENSE][] for more details.

[LICENSE]: https://github.com/pyrmont/predoc/blob/master/LICENSE
