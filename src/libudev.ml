open Ctypes
open Foreign
open Stdint
    
type error =
  | Null
  | Errno of int

let string_of_error = function
  | Null -> "Null"
  | Errno e -> "Errno " ^ (string_of_int e)

exception Error of error

let _ = Printexc.register_printer (function
  | Error e -> Some (string_of_error e)
  | _ -> None)

let is_null ty p = ptr_compare (coerce ty (ptr void) p) null = 0

let check_null ty p =
  if is_null ty p then
    raise (Error Null)
  else
    p

let checked_ptr ty = view (ptr ty)
    ~read:(check_null (ptr ty))
    ~write:(fun x -> x)

let string = view (ptr char)
    ~read:(fun p -> check_null (ptr char) p |> coerce (ptr char) string)
    ~write:(coerce string (ptr char))

let with_gc unref x =
  Gc.finalise (fun x -> ignore (unref x)) x;
  x

let check_err = function
  | x when x < 0 -> raise (Error (Errno x))
  | _ -> ()

module Context = struct
  type t_s
  let t_s : t_s structure typ = structure "udev"

  type t = t_s structure ptr
  let t = checked_ptr t_s
  
  let unref =
    foreign "udev_unref"
      (t @-> returning void)

  let create () =
    foreign "udev_new"
      (void @-> returning t) ()
    |> with_gc unref
end

module ListEntry = struct
  type t_s
  let t_s : t_s structure typ = structure "udev_list_entry"

  type t = t_s structure ptr
  let t = ptr t_s

  let get_next =
    foreign "udev_list_entry_get_next"
      (t @-> returning t)

  let get_name =
    foreign "udev_list_entry_get_name"
      (t @-> returning string)

  let get_value =
    foreign "udev_list_entry_get_value"
      (t @-> returning string)

  let assoc p =
    let rec loop l p =
      if is_null t p then List.rev l
      else loop ((get_name p, get_value p) :: l) (get_next p)
    in
    loop [] p

  let names p =
    let rec loop l p =
      if is_null t p then List.rev l
      else loop ((get_name p) :: l) (get_next p)
    in
    loop [] p
end

module Device = struct
  type t_s
  let t_s : t_s structure typ = structure "udev_device"

  type t = t_s structure ptr
  let t = checked_ptr t_s

  type descr =
    | Syspath of string
    | Subsystem_sysname of string * string
    | Device_id of string
    | Environment

  let unref =
    foreign "udev_device_unref"
      (t @-> returning void)

  let context =
    foreign "udev_device_get_udev"
      (t @-> returning Context.t)

  let create_from_syspath u path =
    foreign "udev_device_new_from_syspath"
      (Context.t @-> string @-> returning t)
      u path
    |> with_gc unref

  let create_from_subsystem_sysname u a b =
    foreign "udev_device_new_from_subsystem_sysname"
      (Context.t @-> string @-> string @-> returning t)
      u a b
    |> with_gc unref

  let create_from_device_id u id =
    foreign "udev_device_new_from_device_id"
      (Context.t @-> string @-> returning t)
      u id
    |> with_gc unref

  let create_from_environment u =
    foreign "udev_device_new_from_environment"
      (Context.t @-> returning t) u
    |> with_gc unref

  let create ctx = function
    | Syspath s -> create_from_syspath ctx s
    | Subsystem_sysname (a, b) -> create_from_subsystem_sysname ctx a b
    | Device_id s -> create_from_device_id ctx s
    | Environment -> create_from_environment ctx

  let parent =
    foreign "udev_device_get_parent"
      (t @-> returning (ptr_opt t_s))

  let find_parent d ?devtype ~subsystem =
    foreign "udev_device_get_parent_with_subsystem_devtype"
      (t @-> string @-> string_opt @-> returning (ptr_opt t_s))
      d subsystem devtype

  let devpath =
    foreign "udev_device_get_devpath"
      (t @-> returning string)

  let subsystem =
    foreign "udev_device_get_subsystem"
      (t @-> returning string)

  let devtype =
    foreign "udev_device_get_devtype"
      (t @-> returning string_opt)

  let syspath =
    foreign "udev_device_get_syspath"
      (t @-> returning string)

  let sysname =
    foreign "udev_device_get_sysname"
      (t @-> returning string)

  let sysnum =
    foreign "udev_device_get_sysnum"
      (t @-> returning string_opt)

  let devnode =
    foreign "udev_device_get_devnode"
      (t @-> returning string_opt)

  let is_initialized dev =
    foreign "udev_device_get_is_initialized"
      (t @-> returning int) dev
    |> (function
      | 0 -> false
      | x when x > 0 -> true
      | x -> raise (Error (Errno x)))

  let get_devlinks_list_entry =
    foreign "udev_device_get_devlinks_list_entry"
      (t @-> returning ListEntry.t)

  let devlinks d =
    ListEntry.names (get_devlinks_list_entry d)

  let get_properties_list_entry =
    foreign "udev_device_get_properties_list_entry"
      (t @-> returning ListEntry.t)

  let properties d =
    ListEntry.assoc (get_properties_list_entry d)

  let get_tags_list_entry =
    foreign "udev_device_get_tags_list_entry"
      (t @-> returning ListEntry.t)

  let tags d =
    ListEntry.names (get_tags_list_entry d)

  let get_sysattr_list_entry =
    foreign "udev_device_get_sysattr_list_entry"
      (t @-> returning ListEntry.t)

  let sysattrs d =
    ListEntry.assoc (get_sysattr_list_entry d)

  let property =
    foreign "udev_device_get_property_value"
      (t @-> string @-> returning string_opt)

  let int_property d p =
    match property d p with
    | None -> None
    | Some s -> Some (int_of_string s)

  let bool_property d p =
    match property d p with
    | None -> None
    | Some "1" -> Some true
    | Some "0" -> Some false
    | _ -> raise (Invalid_argument "bool_property")

  let driver =
    foreign "udev_device_get_driver"
      (t @-> returning string_opt)

  type action =
    | Add
    | Remove
    | Change
    | Online
    | Offline
    | Other of string

  let string_of_action = function
    | Add -> "add"
    | Remove -> "remove"
    | Change -> "change"
    | Online -> "online"
    | Offline -> "offline"
    | Other s -> s

  let action d =
    foreign "udev_device_get_action"
      (t @-> returning string_opt) d
    |> (function
      | None -> None
      | Some "add" -> Some Add
      | Some "remove" -> Some Remove
      | Some "change" -> Some Change
      | Some "online" -> Some Online
      | Some "offline" -> Some Offline
      | Some a -> Some (Other a))

  let seqnum d =
    foreign "udev_device_get_seqnum"
      (t @-> returning uint64_t) d
    |> (fun i -> Uint64.of_string (Unsigned.UInt64.to_string i))

  let usec_since_initialized d =
    foreign "udev_device_get_usec_since_initialized"
      (t @-> returning uint64_t) d
    |> (fun i -> Uint64.of_string (Unsigned.UInt64.to_string i))

  let sysattr =
    foreign "udev_device_get_sysattr_value"
      (t @-> string @-> returning string_opt)

  let int_sysattr d a =
    match sysattr d a with
    | None -> None
    | Some s -> Some (int_of_string s)

  let bool_sysattr d a =
    match sysattr d a with
    | None -> None
    | Some "1" -> Some true
    | Some "0" -> Some false
    | _ -> raise (Invalid_argument "bool_attribute")

  let set_sysattr dev sysattr v =
    foreign "udev_device_set_sysattr_value"
      (t @-> string @-> string @-> returning int)
      dev sysattr v
    |> check_err

  let has_tag =
    foreign "udev_device_has_tag"
      (t @-> string @-> returning bool)
end

module Monitor = struct
  type t_s
  let t_s : t_s structure typ = structure "udev_monitor"

  type t = t_s structure ptr
  let t = checked_ptr t_s

  type filter =
    | Tag of string
    | Subsystem_devtype of string * string option

  type netlink_source = Udev | Kernel

  let unref =
    foreign "udev_monitor_unref"
      (t @-> returning void)

  let context =
    foreign "udev_monitor_get_udev"
      (t @-> returning Context.t)

  let create ?(source = Udev) u =
    let source = match source with
      | Udev -> "udev"
      | Kernel -> "kernel" in
    foreign "udev_monitor_new_from_netlink"
      (Context.t @-> string @-> returning t) u source
    |> with_gc unref

  let enable_receiving m =
    foreign "udev_monitor_enable_receiving"
      (t @-> returning int) m
    |> check_err

  let set_receive_buffer_size m sz =
    foreign "udev_monitor_set_receive_buffer_size"
      (t @-> int @-> returning int) m sz
    |> check_err

  let fd m: Unix.file_descr =
    foreign "udev_monitor_get_fd"
      (t @-> returning int) m
    |> Obj.magic

  let receive_device_c =
    foreign "udev_monitor_receive_device"
      (t @-> returning (ptr_opt Device.t_s))

  let filter_add_match_subsystem_devtype m a b =
    foreign "udev_monitor_filter_add_match_subsystem_devtype"
      (t @-> string @-> string_opt @-> returning int) m a b
    |> check_err

  let filter_add_match_tag m n =
    foreign "udev_monitor_filter_add_match_tag"
      (t @-> string @-> returning int) m n
    |> check_err

  let filter_update m =
    foreign "udev_monitor_filter_update"
      (t @-> returning int) m
    |> check_err

  let filter_remove m =
    foreign "udev_monitor_filter_remove"
      (t @-> returning int) m
    |> check_err

  let set_filter m f =
    (* [filter_remove] fails if the monitor hasn't started yet *)
    (try filter_remove m with Error _ -> ());
    List.iter (function
      | Tag n -> filter_add_match_tag m n
      | Subsystem_devtype (a, b) ->
        filter_add_match_subsystem_devtype m a b
    ) f;
    filter_update m

  let start = enable_receiving

  exception Timeout
  let rec receive_device ?timeout m =
    let select_to = match timeout with
      | None -> -1.
      | Some tm -> tm in
    let now = Unix.gettimeofday () in
    let ret, _, _ = Unix.select [fd m] [] [] select_to in

    if ret = [] then
      (* timeout *)
      raise Timeout
    else
      (* there should be some data *)
      match receive_device_c m with
      | None ->
        (* some error occured *)
        begin match timeout with
          | None ->
            (* no timeout *)
            receive_device m
          | Some timeout ->
            let elapsed = (Unix.gettimeofday ()) -. now in
            let timeout_remaining = timeout -. elapsed in
            if timeout_remaining <= 0. then
              (* timeout *)
              raise Timeout
            else
              receive_device ~timeout:timeout_remaining m
        end
      | Some dev -> dev
end

module Enumerate = struct
  type t_s
  let t_s : t_s structure typ = structure "udev_enumerate"

  type t = t_s structure ptr
  let t = checked_ptr t_s

  type filter =
    | Subsystem of string
    | NoSubsystem of string
    | Sysattr of string * string
    | NoSysattr of string * string
    | Property of string * string
    | Sysname of string
    | Tag of string
    | Parent of Device.t
    | Is_initialized

  let unref =
    foreign "udev_enumerate_unref"
      (t @-> returning void)

  let get_udev =
    foreign "udev_enumerate_get_udev"
      (t @-> returning Context.t)

  let create u =
    foreign "udev_enumerate_new"
      (Context.t @-> returning t) u
    |> with_gc unref

  let add_match_subsystem e n =
    foreign "udev_enumerate_add_match_subsystem"
      (t @-> string @-> returning int) e n
    |> check_err

  let add_nomatch_subsystem e n =
    foreign "udev_enumerate_add_nomatch_subsystem"
      (t @-> string @-> returning int) e n
    |> check_err

  let add_match_sysattr e n v =
    foreign "udev_enumerate_add_match_sysattr"
      (t @-> string @-> string @-> returning int) e n v
    |> check_err

  let add_nomatch_sysattr e n v =
    foreign "udev_enumerate_add_nomatch_sysattr"
      (t @-> string @-> string @-> returning int) e n v
    |> check_err

  let add_match_property e n v =
    foreign "udev_enumerate_add_match_property"
      (t @-> string @-> string @-> returning int) e n v
    |> check_err

  let add_match_sysname e n =
    foreign "udev_enumerate_add_match_sysname"
      (t @-> string @-> returning int) e n
    |> check_err

  let add_match_tag e n =
    foreign "udev_enumerate_add_match_tag"
      (t @-> string @-> returning int) e n
    |> check_err

  let add_match_parent e p =
    foreign "udev_enumerate_add_match_parent"
      (t @-> Device.t @-> returning int) e p
    |> check_err

  let add_match_is_initialized e =
    foreign "udev_enumerate_add_match_tag"
      (t @-> returning int) e
    |> check_err

  let add_syspath e s =
    foreign "udev_enumerate_add_syspath"
      (t @-> string @-> returning int) e s
    |> check_err

  let scan_devices e =
    foreign "udev_enumerate_scan_devices"
      (t @-> returning int) e
    |> check_err
  
  let scan_subsystems e =
    foreign "udev_enumerate_scan_subsystems"
      (t @-> returning int) e
    |> check_err
  
  let get_list_entry =
    foreign "udev_enumerate_get_list_entry"
      (t @-> returning ListEntry.t)

  let of_scan scan ctx f =
    let e = create ctx in
    List.iter (function
      | Subsystem s -> add_match_subsystem e s
      | NoSubsystem s -> add_nomatch_subsystem e s
      | Sysattr (a, b) -> add_match_sysattr e a b
      | NoSysattr (a, b) -> add_nomatch_sysattr e a b
      | Property (k, v) -> add_match_property e k v
      | Sysname s -> add_match_sysname e s
      | Tag s -> add_match_tag e s
      | Parent p -> add_match_parent e p
      | Is_initialized -> add_match_is_initialized e
    ) f;
    scan e;
    ListEntry.names (get_list_entry e)

  let devices ctx f =
    of_scan scan_devices ctx f
    |> List.map (fun path -> Device.create ctx (Device.Syspath path))

  let subsystems ctx f =
    of_scan scan_subsystems ctx f
    |> List.map (fun path -> Device.create ctx (Device.Syspath path))
end
