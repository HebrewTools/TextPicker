# TextPicker

This tool lets you manage a set of vocabulary lists and find texts from the
Hebrew Bible that use that vocabulary. This is useful for teachers, to find
texts on the right level.

## Installation

The easiest way to use it is using [Docker](https://www.docker.com/):

1. Install Docker.
2. Choose a directory to keep your settings, e.g. `~/text-picker-settings`.
3. Create the directory: `mkdir ~/text-picker-settings`.

To run the app, use:

```console
docker run --rm -it -p 8080:8080 -v ~/text-picker-settings:/usr/src/app/src/TextPicker-data camilstaps/text-picker
```

The first time, this will take some time to download the image. Once the app
outputs

```txt
*** TextPicker HTTP server ***

Running at http://localhost:8080/
```

you can open this URL in your browser.

## Development

To build the app yourself, download [Clean](https://clean-lang.org/) and run:

```console
nitrile update
nitrile fetch --arch=x86
nitrile build --arch=x86
```

You can then run the app as `./src/TextPicker.exe`. Settings will be stored in
`TextPicker-data`.
