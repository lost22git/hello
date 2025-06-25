# ISSUES

## Error on building with `--static-swift-stdlib`

- OS: Termux on Android

```
./.build/checkouts/swift-nio/Sources/NIOFileSystem/Internal/System Calls/Syscalls.swift:459:14: error: value of optional type 'UnsafeMutablePointer<CInterop.PlatformChar>?' (aka 'Optional<UnsafeMutablePointer<Int8>>') must be unwrapped to a value of type 'UnsafeMutablePointer<CInterop.PlatformChar>' (aka 'UnsafeMutablePointer<Int8>')
457 |     _ options: CInt
458 | ) -> UnsafeMutablePointer<CInterop.FTS> {
459 |     fts_open(path, options, nil)!
    |              |- error: value of optional type 'UnsafeMutablePointer<CInterop.PlatformChar>?' (aka 'Optional<UnsafeMutablePointer<Int8>>') must be unwrapped to a value of type 'UnsafeMutablePointer<CInterop.PlatformChar>' (aka 'UnsafeMutablePointer<Int8>')
    |              |- note: coalesce using '??' to provide a default when the optional value contains 'nil'
    |              `- note: force-unwrap using '!' to abort execution if the optional value contains 'nil'
460 | }
461 |
[2/7] Compiling _NIOFileSystem Array+FileSystem.swift
error: Recipe `build` failed on line 13 with exit code 1
```
