# Changelog for safe-coloured-text

## [0.6.0.0] - 2026-05-13

### Added

* `chunkStyleHyperlink :: !(Maybe Text)` field in `ChunkStyle` for OSC 8 hyperlink URLs
* `renderOsc8Open` and `renderOsc8Close` for emitting OSC 8 terminal hyperlink sequences
* `renderChunkBuilder` now emits OSC 8 sequences when `chunkStyleHyperlink` is set

## [0.5.0.0] - 2026-04-12

### Changed

* Separated styling from text: `Chunk` now has `chunkText` and `chunkStyle` fields
* New `ChunkStyle` type holds all styling attributes (italic, bold, colours, etc.)
* Styling field names now use `chunkStyle` prefix (e.g. `chunkStyleItalic`)
* Added `noStyle` constructor and `plainStyle`, `styleSGR` functions
* Renamed `chunkSGR` to `styleSGR`

## [0.4.0.0] - 2026-04-12

### Added

* Strikethrough support (`chunkStrikethrough`, `strikethrough`, SGR 9/29)
* Reverse video support (`chunkSwapForegroundBackground`, `swapForegroundBackground`, SGR 7/27)
* Concealed/hidden text support (`chunkConcealed`, `concealed`, SGR 8/28)
* Overline support (`chunkOverlined`, `overlined`, SGR 53/55)

### Fixed

* `chunkBlinking` is now rendered in `chunkSGR` (was stored but never emitted)

## [0.3.0.2] - 2024-06-23

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
