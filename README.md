# Iris

`iris` is a command line tool for CSV validation and processing.

The main purpose of the project was to explore data streaming techniques in Haskell, and in this respect, it was a success. Haskell is a delightful language to work with, although there's some friction when it comes to building and distributing binaries (that's why you'll only find `aarch64-darwin` binary).

I would not recommend using it in production.
However, it's still a useful CLI tool for CSV processing, if your problems fit the features this tool provides. In my case, those were mostly data validation on a per-row basis, and column transformations.

## Features

- [x] Constant memory processing, suitable for large files
- [x] Fast - ~100 Mbps on 2021 MacBook Pro M1
- [x] Configuration based (TOML)
- [x] Filter columns by column names
- [x] Rename columns
- [x] Add validations
- [x] Error reporting

## Installation

<details open><summary>Download release and unpack</summary>

```sh
curl -L https://github.com/alenkis/iris/releases/download/v0.1.0.0/iris-0.1.0.0.tar.gz | tar xz

./iris --version
```

</details>

<details><summary>Build locally</summary>
If you have `just` installed, you can run

```sh
just build-docker
```

Otherwise, you can run docker commands manually:

```sh
docker build -t iris:latest .
docker create --name iris-container iris:latest
docker cp iris-container:/usr/local/bin/iris .
docker rm iris-container
```

You can then run `./iris` to see the help menu.

If you have Haskell toolchain and `just` setup, you can run

```sh
just transform
```

to see a simple example (`examples/simple.toml` and `examples/simple.txt`). On completion, it will generate an output file `examples/simple.out.csv`

</details>

## Configuration

You can configure processing jobs through TOML config files

<details open><summary>TOML config</summary>

```toml
title = "Simple Transformation"
group_by = "item_group_id"

[[columns]]
name = "item_group_id"
rename = "group_id"

[[columns]]
name = "name"

[[columns]]
name = "price"
rename = "sale_price"
```

</details>

<details open><summary>Input file</summary>

```text
item_group_id,id,name,price
0,1,apple,1.00
0,2,orange,2.00
1,3,shoes,10.00
1,4,socks,5.00
```

</details>

Once you have your configuration (`.toml`) and input file (`.txt`, `.csv`, `.tsv`), you can run

```sh
iris transform --config simple.toml --file simple.txt --output simple.out.csv
```

It will generate the following file contents:

```text
group_id,name,sale_price
0,apple,1.00
0,orange,2.00
1,shoes,10.00
1,socks,5.00
```

## Field Validation Configuration

You can add validations to the fields in your TOML configuration file to enforce specific rules on the data being processed. Below are the available validation rules you can use:

### `non_empty`

Ensures that a value is not empty.

**Example:**

```toml
[[columns]]
name = "title"
validation = ["non_empty"]
```

### `min_length:<length>`

Ensures that a value has a minimum length.

**Example:**

```toml
[[columns]]
name = "name"
validation = ["min_length:2"]
```

### `max_length:<length>`

Ensures that a value has a maximum length.

**Example:**

```toml
[[columns]]
name = "brand"
validation = ["max_length:70"]
```

### `one_of:<value1,value2,...>`

Ensures that a value matches one of the specified literals.

**Example:**

```toml
[[columns]]
name = "gender"
validation = ["one_of:female,male,unisex"]
```

### `regex:<pattern>`

Ensures that a value matches the specified regular expression pattern.

**Example:**

```toml
[[columns]]
name = "product_id"
validation = ["regex:\\d{2}-\\d{3}"]
```

## Combining Rules

You can combine multiple validation rules for a single field by adding multiple strings to the `validation` array.

**Example:**

```toml
[[columns]]
name = "name"
validation = ["non_empty", "min_length:2", "max_length:20"]
```

These rules are applied in the order they appear, and the data must satisfy all of the specified validations.

## Error Reporting

If a field fails validation, the error will be reported in the error output file (`errors.txt`), specifying the row number, field name, and error message.

```tex
Error at row 3: [ name ] Value must not be empty
Error at row 4: [ gender ] Value must be one of ["m","f"] instead got: "female" [ ident ] Value must be at least 2 characters long. Instead, got: "2"
Error at row 5: [ gender ] Value must be one of ["m","f"] instead got: "M" [ ident ] Value must be at least 2 characters long. Instead, got: ""
Error at row 6: [ gender ] Value must be one of ["m","f"] instead got: "male"
```
