# Changelog

Each release lists what changed since the release before it. The changes made
since the most recent release are under Unreleased.

## Unreleased

- Add this changelog. The notes of each GitHub release are its section of
  `CHANGELOG.md`, and the archives of a release include the file.

## 0.4.0 (2026-10-08)

- Port Predoc from Janet to Wattle. The output of the `mdoc` and `html`
  formats is byte-identical to the Janet version. The `jdn` format is now
  `edn`. Predoc is built from source with Zig and Wattle, and the FreeBSD
  release builds are dropped.
- Treat a command at the start of a line in SYNOPSIS as a name, whatever its
  text, and write it with the `Nm` macro. A page for a subcommand can now name
  the program that is invoked (`wattle build`) in SYNOPSIS while NAME gives the
  name of the page (`wattle-build`). An invocation can be wrapped if its
  continuation lines are indented, and no blank lines are needed between
  invocations. A command elsewhere in SYNOPSIS is written with the `Ic` macro,
  so a command that equals the name of the page but is not at the start of a
  line no longer breaks the line.
- Treat a command in NAME as a name too, and let the name be used for the rest
  of the page. Outside SYNOPSIS, a command that matches the name of the page or
  a name from NAME or SYNOPSIS is written with `Nm`. A name that contains a
  space, such as `pkg delete`, is passed to mdoc as one quoted argument. Neither
  way of naming a page needs `--name`.
- Add `pkg-which(8)` to the examples, converted from the `pkg` project's page.
- Install the executable and the two man pages with `wattle pkg install`.
- Run the browser demo as a Wattle web program in place of an embedded Janet
  interpreter.
- Require Zig 0.17.0 to build Predoc from source, because Wattle now needs it.
  Zig 0.16.0 no longer builds Wattle.

## 0.3.0 (2026-08-08)

- Add `--css`. With a path, the `html` format produces a complete page that
  links to a stylesheet, which is written to the path. Without it, the format
  produces a fragment as before.
- Fix the HTML renderer. A licence named in the frontmatter is found relative
  to the input file, not to the working directory, and is written before the
  doctype. Section cross-references are links that work. A block of mdoc is a
  `pre` element with its contents escaped, and an entry in a tagged list whose
  text contains markup characters no longer produces malformed markup.
- Improve the appearance of HTML man pages by using a monospace font, space
  above section headings and links in sienna instead of blue.
- Render the examples to HTML as well as mdoc.
- Build distinct x86-64 and aarch64 executables for the FreeBSD release
  archives. Both archives had contained the x86-64 executable.

## 0.2.6 (2026-08-07)

- Print the version with `-v` or `--version`.
- Add escaped spaces. A backslash before a space keeps a full stop after an
  abbreviation from being taken as the end of a sentence.
- Keep a raw delimiter inside the literal quotes it belongs to. Before, an
  opening parenthesis in a raw value could be moved in front of the quotes.

## 0.2.5 (2025-11-11)

- Fix the parsing of dates in the frontmatter in the `html` format.
- Improve the handling of escaped characters.

## 0.2.4 (2025-11-05)

- Fix the parsing of frontmatter dates in October.
- Fix the curling of single quotes next to punctuation in smart punctuation.
- Fix the `jdn` output of structs.

## 0.2.3 (2025-10-16)

- Add the `json` format.

## 0.2.2 (2025-10-16)

- Save the output in the directory of the input by default. It had been saved
  in the working directory.
- Fix an error when the input is stdin.
- Improve the mdoc output so that lines of text are broken more carefully,
  extraneous spaces are removed and options of more than one character are
  quoted.
- Fix the insertion of the AUTHORS section and make the insertion of a licence
  more robust.
- Fix author problems in the `html` format.

## 0.2.1 (2025-09-13)

- Fix the insertion of authors and the rendering of commas and other trailing
  punctuation in the mdoc output.
- Move the man pages to `man/man1` and `man/man7`, and improve the
  installation instructions.

## 0.2.0 (2025-09-07)

- Add the `html` format.
- Add a project page with a browser demo.

## 0.1.1 (2025-09-05)

- Build release archives for FreeBSD on x86-64 and aarch64.

## 0.1.0 (2025-09-03)

- Initial release.
