# pacifica-robinhood-migration

## Prerequisites

* [Git](https://git-scm.com) (the `git` command.)
* [The Haskell Tool Stack](https://www.haskellstack.org/) (the `stack` command.)

## Build

```sh
git clone https://github.com/pacifica/pacifica-robinhood-migration.git
cd pacifica-robinhood-migration
stack setup
stack build
```

## Usage

The complete command is as follows:

```sh
stack exec pacifica-robinhood-migration-exe -- \
   --limit=1024 --offset=0 \
   < pacifica-robinhood-migration/config.json > out.txt 2> error.txt
```

### Command (break-down)

Execute `pacifica-robinhood-migration-exe` (specified by `pacifica-robinhood-migration/pacifica-robinhood-migration.cabal`, implemented by `pacifica-robinhood-migration/app/Main.hs`.)

```sh
stack exec pacifica-robinhood-migration-exe -- \
```

Use double-hyphen `--` to separate command-line arguments for the `stack` command and the executable, and backspace `\` to split long commands into multiple lines.

Specify the size `--limit` and initial `--offset` for the sliding window for the stream of [Robinhood Policy Engine](https://github.com/cea-hpc/robinhood) entries.

```sh
   --limit=1024 --offset=0 \
```

Read the configuration file `pacifica-robinhood-migration/config.json` from the standard input stream; write the standard output stream to the file `out.txt`; and, write the standard error stream to the file `error.txt`.

```sh
   < pacifica-robinhood-migration/config.json > out.txt 2> error.txt
```

### Configuration file

An example configuration file is given in `pacifica-robinhood-migration/config.json` (reproduced below).

The configuration file is encoded using [JavaScript Object Notation (JSON)](https://www.json.org/) format.

The configuration file is divided into two sections: authentication `"auth"` and file paths `"filepath"`.

The `"auth"` section is a has three sub-sections:
* `"curl-client"` The [cURL](https://curl.haxx.se/) client authentication settings for Pacifica Metadata Services.
* `"ldap-client"` The LDAP client authentication settings for Active Directory.
* `"mysql"` The [MySQL](https://www.mysql.com/) authentication settings for [Robinhood Policy Engine](https://github.com/cea-hpc/robinhood).

The `"filepath"` section is a tree-map, where paths from the root of the tree represent file path. Each node in the tree-map has a list of `"rules"` and a dictionary of `"children"`.

The key for each child node is a path fragment.
The path for a node is the concatenation of the path fragments for its ancestors in the tree and itself.
Paths are matched using the [POSIX glob() function](http://man7.org/linux/man-pages/man3/glob.3.html).

```json
{
  "auth": {
    "curl-client": {
      "command_path": "curl",
      "command_arguments": ["--user", "root:root"],
      "protocol": "http",
      "host": "my.pacifica.metadata.host",
      "port": 80
    },
    "ldap-client": {
      "host": "my.ldap.host",
      "port": 389
    },
    "mysql": {
      "host": "my.mysql.host",
      "username": "root",
      "password": "root",
      "database_name": "rbh"
    }
  },
  "filepath": {
    "rules": [
      {
        "__name__": "print",
        "message": "%haystack%"
      }
    ],
    "children": {
      "/path/to/excluded/files/": {
        "rules": [
          {
            "__name__": "break"
          }
        ],
        "children": {}
      },
      "/path/to/included/files/**/*.zip": {
        "rules": [
          {
            "__name__": "logger.info",
            "message": "Successfully matched zip archive \"%haystack%\" using \"%needle%\"."
          }
        ],
        "children" : {}
      }
    }
  }
}
```

In the above example:
* The root node in the tree-map has one rule, `print`, which prints the absolute path to the file for the current entry, `%haystack%`, to the standard output stream.
* The root node has two child nodes:
  * `"/path/to/excluded/files/"` uses the `break` rule to short-circuit the evaluation and skip all matching files.
  * `"/path/to/included/files/**/*.zip"` uses the `logger.info` rule to print a string to the standard error stream via the informational-level logger.

### Rules

The structure of a rule is as follows:

```json
{
  "__name__": "example",
  "arg1": "Hello, world!",
  "arg2": true,
  "arg3": [1, 2, 3, 4, 5]
}
```

The (fictional) rule in the above example is named `example` and has three arguments: `arg1`, `arg2`, and `arg3`.

| Rule name | Description | Arguments |
|-|-|-|
| `break` | Short-circuit the evaluation (skip current entry). | None |
| `pass` | Do nothing. | None |
| `print` | Format `message` (see format string arguments) and print to standard output stream. | `message:String` |
| `logger.{debug,info,warn,error}` | Format `message` (see format string arguments) and print to standard error stream via logger at specified level. | `message:String` |

| Format string argument name | Description |
|-|-|
| `%needle%` | The path to the current node in the tree-map. |
| `%haystack` | The absolute path to the file for the current entry. |
| `%__name__%` | The name of the current rule. |
