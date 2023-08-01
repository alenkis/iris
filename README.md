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

## Instructions

If you have `cabal` and `just` setup, you can run

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
