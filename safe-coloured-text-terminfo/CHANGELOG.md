# Changelog for safe-coloured-text-terminfo

## [0.3.0.0] - 2024-06-23

### Changed

* `getTerminalCapabilitiesFromEnv` now supports the `NO_COLOR` env variable,
  see https://no-color.org/

## [0.2.0.0] - 2024-03-26

### Removed

* Removed deprecated functions

## [0.1.0.0] - 2022-06-19

### Changed

* The default output of the library is now `Text`, so that the encoding is not chosen for the user.
  Existing functions for rendering to `ByteString`s have been deprecated but not removed.
