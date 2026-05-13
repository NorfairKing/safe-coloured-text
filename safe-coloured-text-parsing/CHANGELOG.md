# Changelog for safe-coloured-text-parsing

## [0.1.0.0] - 2026-05-13

### Added

* `OscCommand` type representing the payload of an OSC escape sequence,
  with `OscHyperlink` for OSC 8 hyperlinks and `OscOther` for all other
  OSC commands
* `OscSequence` token now carries an `OscCommand` value instead of being
  a unit constructor
* OSC sequences are now parsed and tokenized rather than leaking as plain
  text (fixes rendering of GHC warning messages that include OSC 8
  hyperlinks to error documentation)

## [0.0.0.0] - 2026-04-12

### Added

* Initial release
* Two-layer ANSI escape code parsing: tokenization and interpretation
* Strict and lazy Text support
* SGR parameter interpretation for all standard attributes
* Parser threads `ChunkStyle` for SGR state
