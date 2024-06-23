# Changelog for safe-coloured-text

## [0.3.0.2] - 2024-03-26

### Added

* `unwordsChunks`
* `chunkWidth`

## [0.3.0.1] - 2024-03-26

### Added

* `unlinesChunks`

## [0.3.0.0] - 2024-03-26

### Removed

* Removed deprecated functions

## [0.2.0.2] - 2024-03-26

### Changed

* Fixed a rendering problem in iTerm.

## [0.2.0.1] - 2022-06-29

### Added

* Support blinking text

## [0.2.0.0] - 2022-06-19

### Changed

* The default output of the library is now `Text`, so that the encoding is not chosen for the user.
  Existing functions for rendering to `ByteString`s have been deprecated but not removed.
* Internal character constants have been changed from `Word8` to `Char`.
