# Folsomline

### Missing persistence for folsom

## Synopsis

[Folsom](https://github.com/boundary/folsom) is Erlang metrics aggregator from Boundary. It's a perfect tool for a realtime metrics collection and simple statistical analysis, but I was missing persistence layer that'd allow me to follow the *dynamics* of the changes. So I wrote Folsomline.

## Installation

### With [rebar](https://github.com/basho/rebar)

Include the following in your rebar config's deps section:

```erlang
{deps, [
  {folsomline, "0.1.1", {git, "git://github.com/eiri/folsomline.git"}}
]}
```
### With [erlang.mk](https://github.com/extend/erlang.mk)

Include the following in your `Makefile`

```Make
DEPS = folsomline

dep_folsomline = https://github.com/eiri/folsomline.git 0.1.1
```

## Usage

Start folsomline as `application:start(folsomline)` and just store folsom metrics as usually. Folsomline is running on a background, storing a snapshot of all the registered folsom's metrics every minute (by default).

To get a history run `folsomline:read().`

[More detailed example on usage](https://github.com/eiri/folsomline/wiki/How-to-use-folsomline)

## Configuration

Default storing interval is one minute. Default storage file kept in _/tmp/folsom.db_

Configuration could be changed by specifying parameters _dbfile_ and _interval_ in application's config file under _folsomline_ term, e.g.:

```erlang
{folsomline, [
  {dbfile, "./logs/stats.log"},
  {interval, 180000}
]},

```

## Notes

  - Folsomline starts to record the metrics not immediately on a start, but from a first second of a next minute.
  - Folsomline by any means is not a complete monitoring solutions. If you need a proper EVM monitoring on top of folsom check out [Folsomite](https://github.com/campanja/folsomite), though I myself haven't tried it.

## Changelog

  - 0.1.1 - Example app. Fixes on storage and re-openening of the metrics file.
  - 0.1.0 - Initial naive version.

## License

MIT. See [License](https://github.com/eiri/grass/blob/master/License "MIT License")
