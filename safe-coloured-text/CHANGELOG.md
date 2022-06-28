# Changelog for appendful

## [0.2.0.0] - 2022-06-19

### Changed

* The default output of the library is now `Text`, so that the encoding is not chosen for the user.
  Existing functions for rendering to `ByteString`s have been deprecated but not removed.
* Internal character constants have been changed from `Word8` to `Char`.

## [0.1.0.0] - 2022-06-19

### Added

* Autodocodec support

