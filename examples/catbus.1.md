---
Title: CATBUS(1)
Authors: Michael Camilleri <mike@inqk.net>
Date: 2025-08-10
Project: catbus
Version: 0.1-dev
---

NAME
====

**catbus** - a happy IRC client

SYNOPSIS
========

**catbus** [**-Relqv**] [**-C** _value_] [**-H** _seed_] [**-I** _pattern_]
           [**-N** _value_] [**-O** _value_] [**-S** _addr_]
           [**-T**[_format_]] [**-a** _user:pass_] [**-c** _path_]
           [**-i** _pattern_] [**-j** _channels_ [_keys_]] [**-k** _path_]
           [**-m** _modes_] [**-n** _nick_ [...]] [**-p** _port_]
           [**-r** _name_] [**-s** _path_] [**-t** _path_] [**-u** _username_]
           [**-w** _pass_] [**-h** _host_] [_config_]

**catbus** **-o** [**-S** _addr_] [**-p** _port_] [**-h** _host_] [_config_]

**catbus** **-g** _cert_

DESCRIPTION
===========

The **catbus** IRC client provides a curses interface for sending and receiving
messages using the Internet Relay Chat protocol over TLS. It is a fork of the
catgirl IRC client by June McEnroe. The key change is the addition of a "scroll
mode".

Like catgirl, **catbus** is an intentionally simple client. It connects to only
one server at a time, requires TLS and offers almost no customisation.

Unless generating a TLS client certificate with the **-g** option, the user must
specify either (1) the **-h** option or (2) a _config_ that includes a host
value.

Options
-------

- **-C** _value_ | **copy** _value_:
  Set the path for the utility used by the **/copy** command to _value_. If the
  option is provided multiple times, each _value_ is passed as an argument to
  the utility. The default is the first available of `pbcopy(1)`, `wl-copy(1)`,
  `xclip(1)` or `xsel(1)`.

- **-H** _seed_ | **hash** _seed_:
  Set the seed for assigning the nick and channel colours. **catbus** uses the
  username associated with a nick to assign the colour so that colours remain
  stable across nick changes. The same seed will result in the same colours
  being assigned. To change colours, choose a different seed.

- **-I** _pattern_ | **highlight** _pattern_:
  Add a case-insensitive message highlight pattern. The pattern may contain `*`,
  `?` and `[]` wildcards as in `glob(7)`. The format of the pattern is as follows:

  ````
  nick[!user@host [command [channel [message]]]]
  ````

  The commands which can be matched are: **invite**, **join**, **nick**,
  **notice**, **part**, **privmsg**, **quit**, **setname**. For example, to
  highlight when somebody joins a channel include the following in your
  configuration file:

  ````
  highlight somebody!*@* join #channel
  ````

- **-N** _value_ | **notify** _value_:
  Set the path for the utility used to send notifications. If the option is
  provided multiple times, each _value_ is passed as an argument to the utility.
  The window name and message are provided to the utility as two additional
  arguments, appropriate for `notify-send(1)`.

- **-O** _value_ | **open** _value_:
  Set the path for the utility used by the **/open** command to _value_. If the
  option is provided multiple times, each _value_ is passed as an argument to
  the utility. The URL to open is provided to the utility as an additional
  argument. The default is the first available of `open(1)` or `xdg-open(1)`.

- **-R** | **restrict**:
  Disable the **/copy**, **/exec** and **/open** commands, the notify option,
  and viewing this manual with **/help**.

- **-S** _addr_ | **bind** _addr_:
  Bind to source address _addr_ when connecting to the server. To connect from
  any IPv4 address, use 0.0.0.0. To connect from any IPv6 address, use ::.

- **-T**[_format_] | **timestamp** _format_:
  Show timestamps by default. The optional format string is interpreted by
  `strftime(3)`. The string may contain raw IRC formatting codes, if you can
  figure out how to enter them.

- **-a** _user:pass_ | **sasl-plain** _user:pass_:
  Authenticate with NickServ during connection using SASL PLAIN. **catbus** will
  disconnect if authentication fails. Leave _pass_ blank to prompt for the
  password when **catbus** starts.

- **-c** _path_ | **cert** _path_:
  Connect using a TLS client certificate loaded from _path_. If a bare filename
  is provided, **catbus** will search in `$XDG_CONFIG_DIRS/catbus/`. If the
  private key is in a separate file, additionally specify it with the **-k**
  option.

  To use this certificate to authenticate to NickServ using CertFP, use
  the **-e** option. See CertFP Configuration below. Client certificates
  can be generated with the **-g** flag.

- **-e** | **sasl-external**:
  Authenticate with NickServ during connection using CertFP via SASL EXTERNAL.
  **catbus** will disconnect if authentication fails. The client certificate
  must be specified with the **-c** option. See CertFP Configuration below.

- **-g** _path_:
  Generate a TLS client certificate using `openssl(1)` and write it to _path_.

- **-h** _host_ | **host** _host_:
  The hostname of the IRC server.

- **-i** _pattern_ | **ignore** _pattern_:
  Add a case-insensitive message ignore pattern, which may contain `*`, `?`
  and `[]` wildcards as in `glob(7)`. The format of the pattern is as follows:

  ````
  nick[!user@host [command [channel [message]]]]
  ````

  The commands which can be matched are: **invite**, **join**, **nick**,
  **notice**, **part**, **privmsg**, **quit**, **setname**.

  Visibility of ignored messages can be toggled using **M--** and **M-+**.

- **-j** _channels_ [_keys_] | **join** _channels_ [_keys_]:
  Join the comma-separated list of channels with the optional comma-separated
  list of channel keys. No spaces may appear in either list.

- **-k** _path_ | **priv** _path_:
  Load the TLS client private key for a certificate loaded with the **-c**
  option from _path_. If a bare filename is provided, **catbus** will search in
  `$XDG_CONFIG_DIRS/catbus/`.

- **-l** | **log**:
  Log messages to files in `$XDG_DATA_HOME/catbus/log/` (usually
  `~/.local/share/catbus/log/`). Directories are created for each network and
  channel, and files are created for each date in the format `YYYY-MM-DD.log`.

- **-m** _modes_ | **mode** _modes_:
  Set user modes as soon as possible after connecting.

- **-n** _nick_ [...] | **nick** _nick_ [...]:
  Set the nickname with optional fallbacks, should one nick be unavailable. Each
  nick is treated as a highlight word. The default nickname is the value of
  `USER` in the current environment.

- **-o**:
  Connect to the server only to obtain its certificate chain and write it to
  standard output in PEM format.

- **-p** _port_ | **port** _port_:
  The port of the IRC server. The default port is 6697.

- **-q** | **quiet**:
  Raise the default message visibility threshold for new windows, hiding general
  events (**joins**, **quits**, etc.). The threshold can be lowered with
  **M--**.

- **-r** _name_ | **real** _name_:
  Set the "real name" which appears in **/whois**. The default is the same as
  the nickname. This is a good place to add your pronouns.

- **-s** _path_ | **save** _path_:
  Persist windows and their scrollback in a file located at _path_. If a bare
  filename is provided, **catbus** will use `$XDG_DATA_HOME/catbus/` (usually
  `~/.local/share/catbus/`).

- **-t** _path_ | **trust** _path_:
  Trust the self-signed certificate at _path_. If a bare filename is provided,
  **catbus** will search in `$XDG_CONFIG_DIRS/catbus/` (usually
  `~/.config/catbus/`). Will also disable server name verification. See
  Self-signed Certificate Configuration below.

- **-u** _username_ | **user** _username_:
  Set the username. This is almost entirely irrelevant, except that it's more
  likely to remain stable, and **catbus** uses it to assign nick colours. The
  default is to use the nickname.

- **-v** | **debug**:
  Log raw IRC protocol to the \<debug\> window, as well as `STDERR` if it is
  not a terminal.

- **-w** _pass_ | **pass** _pass_:
  Connect using a server password. Leave _pass_ blank (using an `=`) to prompt
  for the password when **catbus** starts.

CONFIGURATION
=============

Configuration Files
-------------------

Options can be loaded from _config_, a configuration file. **catbus** searches
for the specified configuration file in `$XDG_CONFIG_DIRS/catbus/` (usually
`~/.config/catbus/`).

Each option must be placed on a new line. Lines that begin with `#` are ignored.
An optional `=` may appear between an option name and its value. The options are
listed above following their corresponding flags.

Flags and options in files are processed in the order they appear on the command
line, so later values generally override earlier values (for an exception, see
**-C**).

CertFP Configuration
--------------------

CertFP is an authentication method that uses a certificate fingerprint rather
than an account password to authenticate with NickServ. Support varies by IRC
network.

1. Generate a new TLS client certificate:

   ````
   $ catbus -g ~/.config/catbus/example.client.pem
   ````

2. Connect to the server using the certificate by adding the following to the
   relevant configuration file:

   ````
   cert example.client.pem
   ````

3. Identify with NickServ, then add the certificate fingerprint to your account:

   ````
   /ns CERT ADD
   ````

4. Enable SASL EXTERNAL in your configuration file to require successful
   authentication when connecting:

   ````
   cert example.client.pem
   sasl-external
   ````

Self-signed Certificate Configuration
-------------------------------------

If connecting to a server fails with a certificate verification error due to a
self-signed certificate, it needs to be trusted manually.

1. Connect to the server and write its certificate to a file:

   ````
   $ catbus -o -h irc.example.org > ~/.config/catbus/example.server.pem
   ````

2. Configure **catbus** to trust the certificate:

   ````
   trust example.server.pem
   ````

INTERFACE
=========

The **catbus** interface is split into three main areas.

Status Line
-----------

The top line of the terminal shows window statuses. Only the currently active
window and windows with activity are shown. The status line for a window might
look like this:

````
1+ #catbus +3 ~7 @
````

The number on the left is the window number. Following it may be one of `-`, `+`
or `++`, as well as `=`. These indicate the message visibility threshold and
mute status of the window.

On the right side, the number following `+` indicates the number of unread
messages. The number following `~` indicates how many lines are below the
current scroll position. An `@` indicates that there is unsent input waiting in
the window's Input Line.

**catbus** will also set the terminal title, if possible, to the name of the
network and active window, followed by the unread count for that window, and the
unread count for all other windows in parentheses.

Chat Area
---------

The Chat Area shows messages and events. Regular messages are shown with the
nick between `<>` angle brackets. Actions are shown with the nick preceded by
`*`. Notices are shown with the nick between `-` hyphens.

Blank lines are inserted into the chat as unread markers whenever there are
messages in a window that is not active or the terminal is not focused (in some
terminal emulators).

While scrolling, the most recent five lines of chat are kept visible below a
marker line.

Input Line
----------

The bottom line of the window is where messages and commands are entered. When
entering a message, action or notice, your nick appears on the left, as it would
in the Chat Area. When entering a command, no nick is shown. In scroll mode, the
Input Line is coloured grey.

Formatting codes are shown in the Input Line as reverse-video uppercase letters.
These will not appear in the sent message.

Input that is too long to send as a single message will have a red background
starting at the point where the input will be split into a second message.

COMMANDS
========

Commands can be abbreviated if no other command shares the same prefix. For
example, **/join** can be typed **/j**, and **/window** can be typed **/wi**.

Chat Commands
-------------

- **/away** [_message_]:
  Set or clear your away status. This is sent in reply to private messages and
  shown in **/whois**.

- **/cs** _command_:
  Send a command to ChanServ, the service for managing registered channels.

- **/invite** _nick_:
  Invite someone to the channel.

- **/join** [_channel_ [_key_]]:
  Join the named channel, the current channel (if you've left), or the channel
  to which you've been invited.

- **/list** [_search_]:
  List channels, their user counts and their topics. The _search_ can usually
  contain `glob(7)`-style wildcards.

- **/me** [_action_]:
  Send an action message. These are used to write messages in the third person.

- **/msg** _nick_ _message_:
  Send a private message to someone.

- **/names**:
  List the users in the channel.

- **/nick** _nick_:
  Change your nickname.

- **/notice** _message_:
  Send a notice. It's best not to do this.

- **/ns** _command_:
  Send a command to NickServ, the service for managing your account.

- **/ops**:
  List channel operators. They can kick or ban someone from the channel.

- **/part** [_message_]:
  Leave the channel. Use **/close** if you want to close the window afterward.

- **/query** _nick_:
  Start a private conversation with someone.

- **/quit** [_message_]:
  Disconnect from IRC and close **catbus**. You can do this even quicker with
  **C-c**.

- **/quote** _command_:
  Send a raw IRC command. Often **catbus** will not know how to interpret the
  results. You can use **M--** to show unknown server responses in the
  **<network>** or channel windows.

- **/say** _message_:
  Send a regular message. This is useful if the message you want to send begins
  with a slash.

- **/setname** _name_:
  Update your "real name" if the server supports it. This may be broadcast to
  other users with clients that support it.

- **/topic** [_topic_]:
  Show or set the topic of the channel. Press **Tab** twice immediately after
  **/topic** to copy the current topic.

- **/whois** [_nick_]:
  Query information about a user.

- **/whowas** _nick_:
  Query past information about a user.

Interface Commands
------------------

- **/close** [_name_ | _num_]:
  Close the named, numbered or current window.

- **/copy** [_nick_ | _substring_]:
  Copy the most recent URL from nick or matching substring.

- **/debug**:
  Toggle logging in the \<debug\> window.

- **/exec** _command_:
  Run _command_ with `SHELL` and interpret its output as input to the current
  window, including as commands.

- **/help**:
  View this manual. Type `q` to return to **catbus**.

- **/help** _topic_:
  List the server help for a topic. Try **/help** **index** for a list of topics.

- **/highlight** [_pattern_]:
  List message highlight patterns or temporarily add a pattern. To permanently
  add a pattern, use the **highlight** option.

- **/ignore** [_pattern_]:
  List message ignore patterns or temporarily add a pattern. To permanently add
  a pattern, use the ignore option.

- **/move** [_name_] num:
  Move the named or current window to number.

- **/open** [count]:
  Open each of _count_ most recent URLs.

- **/open** _nick_ | _substring_:
  Open the most recent URL from nick or matching substring.

- **/unhighlight** _pattern_:
  Temporarily remove a message highlight pattern.

- **/unignore** _pattern_:
  Temporarily remove a message ignore pattern.

- **/window**:
  List all windows.

- **/window** _name_ | _num_ | _substring_:
  Switch to window by name, num or matching substring. Note that it is possible
  to switch to a window by typing `/num` where `num` is a number.

Operator Commands
-----------------

- **/ban** [_mask_ ...]:
  List or ban masks from the channel.

- **/deop** [_nick_ ...]:
  Revoke channel operator status from users or yourself.

- **/devoice** [_nick_ ...]:
  Revoke voice from users or yourself in the channel.

- **/except** [_mask_ ...]:
  List or add masks to the channel ban exception list.

- **/invex** [_mask_ ...]:
  List or add masks to the channel invite list.

- **/kick** _nick_ [_message_]:
  Kick a user from the channel.

- **/mode** [_modes_] [_param_ ...]:
  Show or set channel modes. In the <network> window, show or set user modes.

- **/op** [_nick_ ...]:
  Grant users or yourself channel operator status.

- **/unban** _mask_ [...]:
  Unban masks from the channel.

- **/unexcept** _mask_ [...]:
  Remove masks from the channel ban exception list.

- **/uninvex** _mask_ [...]:
  Remove masks from the channel invite list.

- **/voice** [_nick_ ...]:
  Grant users or yourself voice in the channel.

KEY BINDINGS
============

The **catbus** interface supports `emacs(1)`-like line editing and `vim(1)`-like
window scrolling (an ability some consider to be unnatural). To scroll a window,
the user must put **catbus** into scroll mode.

**catbus** also supports key bindings for IRC formatting. In the lists below,
the prefixes **C-** and **M-** represent the control and meta (alt) modifiers
respectively.

Line Editing
------------

- **C-a**:
  Move to beginning of line.
- **C-b**:
  Move left.
- **C-d**:
  Delete next character.
- **C-e**:
  Move to end of line.
- **C-f**:
  Move right.
- **C-k**:
  Delete to end of line.
- **C-t**:
  Transpose characters.
- **C-u**:
  Delete to beginning of line.
- **C-w**:
  Delete previous word.
- **C-x**:
  Expand a text macro beginning with '\\'.
- **C-y**:
  Paste previously deleted text.
- **M-Enter**:
  Insert a newline without sending a command.
- **M-b**:
  Move to previous word.
- **M-d**:
  Delete next word.
- **M-f**:
  Move to next word.
- **M-q**:
  Collapse all whitespace.
- **Tab**:
  Complete nick, channel, command or macro.

Window Keys
-----------

- **C-l**:
  Redraw the UI.
- **C-n**:
  Switch to next window.
- **C-p**:
  Switch to previous window.
- **C-v**:
  Enter scroll mode.
- **M-+**:
  Raise message visibility threshold, hiding ignored messages, general events
  (joins, quits, etc.), or non-highlighted messages.
- **M--**:
  Lower message visibility threshold, showing ignored messages and unknown
  replies.
- **M-=**:
  Toggle mute. Muted windows do not appear in the status line unless you are
  mentioned.
- **C-/**:
  Switch to previously selected window.
- **M-n**:
  Switch to window by number 0–9.
- **M-a**:
  Cycle through unread windows.
- **M-l**:
  List the contents of the window without word-wrapping and with timestamps.
  Press **Enter** to return to **catbus**.
- **M-m**:
  Insert a blank line in the window.
- **M-s**:
  Reveal spoiler text.
- **M-t**:
  Toggle timestamps.

Scroll Mode Keys
----------------

- **j**:
  Scroll down a line.
- **k**:
  Scroll up a line.
- **q**:
  Leave scroll mode.
- **u**:
  Scroll to first unread line.
- **C-u**:
  Scroll up a page.
- **C-d**:
  Scroll down a page.
- **C-f**:
  Scroll forward half a page.
- **C-b**:
  Scroll back half a page.

IRC Formatting
--------------

- **C-z C-v**:
  Insert the next input character literally.
- **C-z b**:
  Toggle bold.
- **C-z c**:
  Set or reset colour.
- **C-z i**:
  Toggle italics.
- **C-z o**:
  Reset formatting.
- **C-z p**:
  Manually toggle paste mode.
- **C-z r**:
  Toggle reverse colour.
- **C-z s**:
  Set spoiler text (black on black).
- **C-z u**:
  Toggle underline.

Some colour codes can be entered using the following bindings:

```
----------|---------|-----------|--------------------
**C-z A** | gray    | **C-z N** | brown (dark red)
**C-z B** | blue    | **C-z O** | orange (dark yellow)
**C-z C** | cyan    | **C-z P** | pink (light magenta)
**C-z G** | green   | **C-z R** | red
**C-z K** | black   | **C-z W** | white
**C-z M** | magenta | **C-z Y** | yellow
----------|---------|-----------|--------------------
```

To set other colours, follow **C-z c** by one or two digits for the foreground
colour, optionally followed by a comma and one or two digits for the background
colour. To reset colour, follow **C-z c** by a non-digit.

The colour numbers are as follows:

```
---|----------------------|----|---------------------
 0 | white                |  8 | yellow
 1 | black                |  9 | light green
 2 | blue                 | 10 | cyan
 3 | green                | 11 | light cyan
 4 | red                  | 12 | light blue
 5 | brown (dark red)     | 13 | pink (light magenta)
 6 | magenta              | 14 | gray
 7 | orange (dark yellow) | 15 | light gray
99 | default              |    |  
---|----------------------|----|---------------------
```

ENVIRONMENT
===========

- `SHELL`:
  The shell used by **/exec**. The default is `/bin/sh`.

- `USER`:
  The default nickname.

FILES
=====

- `$XDG_CONFIG_DIRS/catbus`:
  Configuration files are searched for first in `$XDG_CONFIG_DIRS` (usually
  `./config/`) followed by the colon-separated list of paths, `$XDG_CONFIG_DIRS`
  (usually `/etc/xdg/`).

- `~/.config/catbus`:
  The most likely location of configuration files.

- `$XDG_DATA_DIRS/catbus`:
  Save files are searched for first in `$XDG_DATA_DIRS` (usually
  `~/.local/share/`) followed by the colon-separated list of paths,
  `$XDG_DATA_DIRS` (usually `/usr/local/share:/usr/share`).

- `~/.local/share/catbus`:
  The most likely location of save files.

EXIT STATUS
===========

The **catbus** utility exits 0 if requested by the user, 69 if the
connection is lost and >0 if any other error occurs.

EXAMPLES
========

Join the #catbus channel on the Libera IRC server from the command line:

`````
catbus -h irc.libera.chat -j "#catbus"
`````

Create a configuration file in `~/.config/catbus/libera`:

````
host irc.libera.chat
join #catbus
````

Load the configuration file:

````
$ catbus libera
````

EXTENSIONS
==========

Like catgirl, the **catbus** client implements the
__causal.agency/consumer__ vendor-specific IRCv3 capability offered by
`pounce(1)`. The consumer position is stored in the save file.

BUGS
====

Report issues at <https://github.com/pyrmont/catbus> or join #catbus on
irc.libera.chat.
