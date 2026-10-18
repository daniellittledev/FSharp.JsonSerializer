# Changelog

## 0.1.0

### Fixed

* Deserializing `null` into a union that can't be null now throws a `JsonException` instead of silently returning `null` (tagged and untagged unions).
* A missing value-type field (e.g. `int`) with `DefaultIgnoreCondition = WhenWritingNull` now throws a `JsonException` instead of a `NullReferenceException`.
* Duplicate properties can no longer hide a missing field.
* Missing field errors name the case and field, e.g. `Missing field for union type MyUnion: Rename.name`.
* Unknown numeric or boolean case tags, and unmatched untagged tokens, report the value instead of failing with an `InvalidOperationException`.
* `[<JsonName>]` can be applied to union cases with fields under F# 9 and later, which validate attribute targets.
* `Nullable<T>` fields accept `null`, and read as empty when missing and nulls are ignored.
* With `WhenWritingNull`, a missing field of an F# type (record, union, list, tuple) now throws a `JsonException` instead of silently becoming `null`. Missing `option` fields still read as `None` and .NET reference types such as `string` as `null`.
* Untagged unions build their reflection-based readers once per type instead of on every read and write.

### Changed

* Minimum `System.Text.Json` raised to 6.0.10 to avoid a known vulnerability in 6.0.0.
* Package project URL now points at the documentation site, and the README is included in the package.

### Added

* Documentation site with guides and an API reference, published to GitHub Pages.
* CI workflow that builds, tests, packs and builds the docs.
* Release workflow that publishes to NuGet when a version tag is pushed.
* Test suite organised by converter, with FsCheck round-trip properties.

## 0.0.3

* Serialize Unions
