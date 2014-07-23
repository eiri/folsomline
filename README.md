# Folsomline

### Missing persistence for folsom

## Synopsis

[Folsom](https://github.com/boundary/folsom) is Erlang metrics aggregator from Boundary. It's a perfect tool for realtime metrics collection and simple statistical analysis, but I was missing persistence layer that'd allow me to follow the *dynamics* of the changes. So I wrote Folsomline.

## Installation

### With [rebar](https://github.com/basho/rebar)

Include the following in rebar config's deps section:

```erlang
{deps, [
  {'folsomline', '.*', {git, "git://github.com/eiri/folsomline.git"}}
]}
```
### With [erlang.mk](https://github.com/extend/erlang.mk)

Include the following in `Makefile`

```Make
DEPS = folsomline

dep_folsomline = https://github.com/eiri/folsomline.git master
```

## Usage

Start folsomline as `application:start(folsomline)` and just store metrics with folsom as usual. Folsomline running in background, storing all the registered metrics every minute (by default).

To get history just run `folsomline:read().`

## Configuration

Default storing interval is one minute. Default storage file kept in local _log_ directory as _folsom.db_

Configuration could be changed by specifying parameters _file_ and _interval_ in application's config file, i.e.:

```erlang
{folsomline, [
  {file, RelativePath/FileName},
  {interval, Microseconds}
]},

```

## Notes

   - Folsomline starts to record metrics not immediately on a start but from a first second of a next minute.
   - Folsomline by any mean not a monitoring solution. If you need a complete EVM monitoring check out [Folsomite](https://github.com/campanja/folsomite), though I haven't tried it myself.

## Changelog

   - 0.1.0 - Initial naive version.

## License

MIT. See [License](https://github.com/eiri/grass/blob/master/License "MIT License")
