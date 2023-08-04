# Iris

`iris` is a command line tool for CSV processing.

## Features

- [x] Constant memory processing (streams)
- [x] Fast - ~100 Mbps
- [x] Configuration based (TOML)
- [x] Filter by column names
- [x] Rename column names
- [ ] Add validation using TOML comment based schema references and JSON schema files
- [ ] Output to .parquet
- [ ] Distribute to homebrew

## Installation

If you have `just` installed, you can run

```sh
just build-docker
```

Otherwise, you can run docker commands manually:

```sh
@docker build -t iris:latest .
@docker create --name iris-container iris:latest
@docker cp iris-container:/usr/local/bin/iris .
@docker rm iris-container
```

You can then run `./iris` to see the help menu.

If you have Haskell toolchain and `just` setup, you can run

```sh
just transform
```

to see a simple example (`examples/simple.toml` and `examples/simple.txt`). On completion, it will generate an output file `examples/simple.out.csv`

## Configuration

You can configure processing jobs through TOML config files

```toml
[job]
title = "Simple Transformation"
group_by = "item_group_id"

[[job.field]]
name = "item_group_id"
rename = "group_id"

[[job.field]]
name = "name"

[[job.field]]
name = "price"
rename = "sale_price"
```

```text
item_group_id,id,name,price
0,1,apple,1.00
0,2,orange,2.00
1,3,shoes,10.00
1,4,socks,5.00
```

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
[[job.field]]
name = "title"
validation = ["non_empty"]
```

### `min_length:<length>`

Ensures that a value has a minimum length.

**Example:**

```toml
[[job.field]]
name = "name"
validation = ["min_length:2"]
```

### `max_length:<length>`

Ensures that a value has a maximum length.

**Example:**

```toml
[[job.field]]
name = "brand"
validation = ["max_length:70"]
```

### `one_of:<value1,value2,...>`

Ensures that a value matches one of the specified literals.

**Example:**

```toml
[[job.field]]
name = "gender"
validation = ["one_of:female,male,unisex"]
```

### `regex:<pattern>`

Ensures that a value matches the specified regular expression pattern.

**Example:**

```toml
[[job.field]]
name = "product_id"
validation = ["regex:\\d{2}-\\d{3}"]
```

## Combining Rules

You can combine multiple validation rules for a single field by adding multiple strings to the `validation` array.

**Example:**

```toml
[[job.field]]
name = "name"
validation = ["non_empty", "min_length:2", "max_length:20"]
```

These rules are applied in the order they appear, and the data must satisfy all of the specified validations.
