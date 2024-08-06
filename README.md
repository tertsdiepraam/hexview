# Hexview

<p>
    <a href="https://crates.io/crates/hexview"><img src="https://img.shields.io/crates/v/hexview.svg?colorB=319e8c" alt="Version info"></a><br>
    <img src="https://img.shields.io/crates/l/hexview.svg" alt="license">
</p>

A TUI Hex viewer written in Rust.

**Features**:
- View the hex and ASCII for a binary file
- Color-coded output
- Both keyboard and mouse navigation
- Search for strings in octal, hexadecimal, decimal, UTF-8 and UTF-16
- Inspect data as several data types
- Switch endianness of inspected and search data
- Go to arbitrary offsets and percentages of the file
- Customizable width and grouping of output

## Installation

```
cargo install hexview
```

## Usage

```
hexview path/to/file
```

Press `?` for an overview of the keybindings.

## License

Hexview is licensed under both the MIT and the Apache License (Version 2.0).

See the [`LICENSE-MIT`](./LICENSE-MIT) and [`LICENSE-APACHE`](./LICENSE-APACHE)
files for more information.
