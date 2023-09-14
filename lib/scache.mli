val remove_cache : name:string -> unit
(* [remove_cache ~name] remove all the key value in the disk memory, stored on [name] cache *)

(** Usefull to store on disk memory some informations that could be reused after restarting the program.
 *  *)
module Cache: sig
  type t

  val get: t -> key:string -> string option
  (** [get t ~key] read the key value from memory, return [None] if not found. *)

  val set: t -> key:string -> value:string -> unit
  (** [set t ~key ~value] store [value] with [key] in the memory. *)

  val remove: t -> key:string -> unit
  (** [remove t ~key] remove the [key] value in the memory. *)

  val start: name:string -> t
  (** [start ~name] start [name] cache. It will reuse the [name] cache in memory, if already started
   * before. *)

end
