(* (c) 2017 Hannes Mehnert, all rights reserved *)

type 'a t
(** The type of a domain name, a sequence of labels separated by dots.  Each
    label may contain any bytes. The length of each label may not exceed 63
    characters.  The total length of a domain name is limited to 253 (its byte
    representation is 255), but other protocols (such as SMTP) may apply even
    smaller limits.  A domain name label is case preserving, comparison is done
    in a case insensitive manner.  Every [t] is a fully qualified domain name,
    its last label is the [root] label. The specification of domain names
    originates from {{:https://tools.ietf.org/html/rfc1035}RFC 1035}.

    The invariants on the length of domain names are preserved throughout the
    module - no [t] will exist which violates these.

    Phantom types are used for further name restrictions, {!host} checks for
    host names: only letters, digits, and hyphen allowed, hyphen not first
    character of a label, the last label must contain at least on letter.
    {!service} checks for a service name: first label is a service name:
    1-15 characters, no double-hyphen, hyphen not first or last charactes, only
    letters, digits and hyphen allowed, and the second label is a protocol
    (_tcp or _udp or _sctp).

    Constructing a [t] (via {!of_string}, {!of_string_exn}, {!of_strings} etc.)
    does not require a trailing dot.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}
*)

(** {2 Constructor} *)

val root : [ `domain ] t
(** [root] is the root domain ("."), the empty label. *)

(** {2 String representation} *)

val of_string : string -> ([ `domain ] t, [> `Msg of string ]) result
(** [of_string name] is either [t], the domain name, or an error if the provided
    [name] is not a valid domain name. A trailing dot is not requred. *)

val of_string_exn : string -> [ `domain ] t
(** [of_string_exn name] is [t], the domain name. A trailing dot is not
    required.

    @raise Invalid_argument if [name] is not a valid domain name. *)

val to_string : ?trailing:bool -> 'a t -> string
(** [to_string ~trailing t] is [String.concat ~sep:"." (to_strings t)], a
    human-readable representation of [t].  If [trailing] is provided and
    [true] (defaults to [false]), the resulting string will contain a trailing
    dot. *)

(** {2 Predicates and basic operations} *)

val canonical : 'a t -> 'a t
(** [canonical t] is [t'], the canonical domain name, as specified in RFC 4034
    (and 2535): all characters are lowercase. *)

val host : 'a t -> ([ `host ] t, [> `Msg of string ]) result
(** [host t] is a [`host t] if [t] is a hostname: the contents of the domain
    name is limited: each label may start with a digit or letter, followed by
    digits, letters, or hyphens. *)

val service : 'a t -> ([ `service ] t, [> `Msg of string ]) result
(** [service t] is [`service t] if [t] contains a service name, the following
    conditions have to be met:
    The first label is a service name (or port number); an underscore preceding
    1-15 characters from the set [- a-z A-Z 0-9].
    The service name may not contain a hyphen ([-]) following another hyphen;
    no hyphen at the beginning or end.

    The second label is the protocol, one of [_tcp], [_udp], or [_sctp].
    The remaining labels must form a valid hostname.

    This function can be used to validate RR's of the types SRV (RFC 2782)
    and TLSA (RFC 7671). *)

val domain : 'a t -> [ `domain ] t
(** [domain t] is the [`domain t]. *)

val sub : subdomain:'a t -> domain:'a t -> bool
(** [sub ~subdomain ~domain] is [true] if [subdomain] contains any labels
    prepended to [domain]: [foo.bar.com] is a subdomain of [bar.com] and of
    [com], [sub ~subdomain:x ~domain:root] is true for all [x]. *)

(** {2 Label addition and removal} *)
val prepend : 'a t -> string -> ([ `domain ] t, [> `Msg of string ]) result
(** [prepend name pre] is either [t], the new domain name, or an error. *)

val prepend_exn : 'a t -> string -> [ `domain ] t
(** [prepend_exn name pre] is [t], the new domain name.

    @raise Invalid_argument if [pre] is not a valid domain name. *)

val drop_labels : ?back:bool -> ?amount:int -> 'a t ->
  ([ `domain ] t, [> `Msg of string ]) result
(** [drop_labels ~back ~amount t] is either [t], a domain name with [amount]
    (defaults to 1) labels dropped from the beginning (unless [back] is provided
    and [true], defaults to [false]).  [drop_labels] applied to [foo.com] is
    [com]. *)

val drop_labels_exn : ?back:bool -> ?amount:int -> 'a t -> [ `domain ] t
(** [drop_labels_exn ~back ~amount t] is either [t], a domain name with [amount]
    (defaults to 1) labels dropped from the beginning (unless [back] is provided
    and [true], defaults to [false]).  [drop_labels] applied to [foo.com] is
    [com].

    @raise Invalid_argument if there are not sufficient labels. *)

val concat : 'a t -> 'b t -> ([ `domain ] t, [> `Msg of string ]) result
(** [concat pre post] is [pre ^ "." ^ post]. *)

val concat_exn : 'a t -> 'b t -> [ `domain ] t
(** [concat_exn pre post] is [pre ^ "." ^ post].

    @raise Invalid_argument if the result would violate length restrictions. *)

(** {2 Comparison} *)

val equal : ?case_sensitive:bool -> 'a t -> 'b t -> bool
(** [equal ~case t t'] is [true] if all labels of [t] and [t'] are equal.
    If [case_sensitive] is provided and [true], the cases of the labels are
    respected (default: [false]). *)

val compare : 'a t -> 'b t -> int
(** [compare t t'] compares the domain names [t] and [t'] using a case
    insensitive string comparison. *)

val compare_sub : string -> string -> int
(** [compare_sub t t'] compares the labels [t] and [t'] using a case
    insensitive string comparison. *)

(** {2 Collections} *)

module Host_map : sig
  include Map.S with type key = [ `host ] t

  (** [find key t] is [Some a] where a is the binding of [key] in [t]. [None] if
      the [key] is not present. *)
  val find : key -> 'a t -> 'a option
end
(** The module of a host name map *)

module Host_set : Set.S with type elt = [ `host ] t
(** The module of a host name set *)

module Service_map : sig
  include Map.S with type key = [ `service ] t

  (** [find key t] is [Some a] where a is the binding of [key] in [t]. [None] if
      the [key] is not present. *)
  val find : key -> 'a t -> 'a option
end
(** The module of a service name map *)

module Service_set : Set.S with type elt = [ `service ] t
(** The module of a service name set *)

module Map : sig
  include Map.S with type key = [ `domain ] t

  (** [find key t] is [Some a] where a is the binding of [key] in [t]. [None] if
      the [key] is not present. *)
  val find : key -> 'a t -> 'a option
end
(** The module of a domain name map *)

module Set : Set.S with type elt = [ `domain ] t
(** The module of a domain name set *)

(** {2 String list representation} *)

val of_strings : string list -> ([ `domain ] t, [> `Msg of string ]) result
(** [of_strings labels] is either [t], a domain name, or an error if
    the provided [labels] violate domain name constraints. A trailing empty
    label is not required. *)

val of_strings_exn : string list -> [ `domain ] t
(** [of_strings_exn labels] is [t], a domain name.  A trailing empty
    label is not required.

    @raise Invalid_argument if [labels] are not a valid domain name. *)

val to_strings : ?trailing:bool -> 'a t -> string list
(** [to_strings ~trailing t] is the list of labels of [t].  If [trailing] is
    provided and [true] (defaults to [false]), the resulting list will contain
    a trailing empty label. *)

(** {2 Pretty printer} *)

val pp : 'a t Fmt.t
(** [pp ppf t] pretty prints the domain name [t] on [ppf]. *)

(**/**)
(* exposing internal structure, used by udns (but could as well use Obj.magic *)

val of_array : string array -> [ `domain ] t
(** [of_array a] is [t], a domain name from [a], an array containing a reversed
    domain name. *)

val to_array : 'a t -> string array
(** [to_array t] is [a], an array containing the reversed domain name of [t]. *)
