# Safe Coloured Text

## Requirements / Design goals

* Use `String`/`Text`/`ByteString` correctly: Some things are strings of characters, some things are text, some things are bytes. Those are not the same.
* No partial functions
* Represent chunks of coloured text purely
* Be able to print chunks of coloured text quickly
* Be able to turn chunks of coloured text into non-coloured text for when terminals don't support (certain) colours.
* As few dependencies as possible.
* Well-tested
* British spelling (because I can)
