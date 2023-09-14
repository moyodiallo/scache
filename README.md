# Scache

Scache is a __simple module__ of (key,value) __cache system using SQlite3__ to persists some information on disk. It remains simple as we could see the interface.
```ocaml
module Cache: sig
  type t
  val get: t -> key:string -> string option
  val set: t -> key:string -> value:string -> unit
  val remove: t -> key:string -> unit
  val start: name:string -> t
end
```

Scache use some component of [ocurrent](https://github.com/ocurrent/ocurrent) about __SQlite3__. The aims of Scache is to light and simplify as much as possible the (key,value) caching.

# Licensing
Scache is licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for the full license text.