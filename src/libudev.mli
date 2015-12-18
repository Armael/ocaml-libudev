(** OCaml bindings to [libudev].

    Part of the documentation has been derived from the {{:
    http://pyudev.readthedocs.org/en/latest/api/pyudev.html } pyudev
    documentation }.
*)

open Stdint

(** {1 Error management} *)

(** Unexpected errors, that may be raised by the functions of the binding. *)
type error =
  | Null
  | Errno of int

val string_of_error : error -> string

exception Error of error

(** {1 Udev} *)

(** UDev database context. *)
module Context : sig
  (** A value of type [Context.t] represents a connection to the udev device
      database. *)
  type t
  val create : unit -> t
end

(** Accessing device information. *)
module Device : sig
  (** A [Device.t] corresponds to a system device. *)
  type t

  (** A type describing different ways of creating a [Device.t] using
      [Device.create]. *)
  type descr =
    | Syspath of string
    (** A device path in [/sys], including the mount point. 

    Example: [Device.create (Device.Syspath "/sys/devices/platform")]. *)
    | Subsystem_sysname of string * string
    (** A given subsystem, and a given sys name.

        Example: [Device.create (Device.Subsystem_sysname ("block", "sda"))]. *)
    | Device_id of string
    (** A device id, which is a special string in one of the following four forms:

        - block device major:minor; example: ["b8:2"]
        - char device major:minor; example: ["c128:1"]
        - network device ifindex; example: ["n3"]
        - kernel driver core subsystem:device name; example: ["+sound:card29"]
    *)
    | Environment
    (** The process environment.

        This only works reliably if the current process is called from an udev
        rule, and is usually used for tools executed from [IMPORT=]
        rules. Use this method to create device objects in OCaml programs called
        from udev rules.  *)

  (** Create a [Device.t] from a [Device.descr]. *)
  val create : Context.t -> descr -> t

  (** {1 General attributes } *)

  (** The [Context.t] to which the device is bound. *)
  val context : t -> Context.t

  (** Absolute path of this device in [sysfs] including the [sysfs]
      mount point, as a unicode string. *)
  val syspath : t -> string

  (** Device file name inside [sysfs] as unicode string. *)
  val sysname : t -> string

  (** The trailing number of the [sysname] as unicode string, if any.

      Note: the number is returned as a unicode string to preserve the exact
      format of the number, especially any leading zeros. *)
  val sysnum : t -> string option

  (** Kernel device path as a unicode string. This path uniquely identifies a
      single device.

      Unlike [Device.syspath], this path does not contain the [sysfs] mount
      point. However, the path is absolute and starts with a slash ["/"]. *)
  val devpath : t -> string

  (** The tags attached to the device. Tags are arbitrary classifiers that can
      be attached to devices by udev scripts and daemons. For instance, {{:
      http://freedesktop.org/wiki/Software/systemd } systemd } uses tags for {{:
      http://www.freedesktop.org/wiki/Software/systemd/multiseat } multi-seat }
      support. *)
  val tags : t -> string list

  (** Check is the device has some tag. *)
  val has_tag : t -> string -> bool

  (** {1 Device driver and subsystem } *)

  (** Name of the subsystem the device is part of, as a unicode string. *)
  val subsystem : t -> string

  (** The driver name as a unicode string, if any. *)
  val driver : t -> string option

  (** The device type as a unicode string, if it is known. *)
  val devtype : t -> string option

  (** {1 Device nodes } *)

  (** Absolute path to the device node of this device, as a unicode string, if
      the device has a device node. 

      This path always points to the actual device node associated with this device,
      and never to any symbolic links to this device node. See [Device.devlinks] to
      get a list of symbolic links to this device node. *)
  val devnode : t -> string option

  (** The absolute paths of all symbolic links pointing to the [Device.devnode]
      of this device. The paths are unicode strings. 

      UDev can create symlinks to the original device node (see
      [Device.devnode]) inside the device directory. This is often used to
      assign a constant, fixed device node to devices like removeable media,
      which technically do not have a constant device node, or to map a single
      device into multiple device hierarchies. This function provides access to
      all such symbolic links, which were created by UDev for this device.
  *)
  val devlinks : t -> string list

  (** {1 Device initialization time } *)

  (** Checks if the device is initialized

      A device is initialized if udev has already handled this device and has
      set up device node permissions and context, or renamed a network device.

      Such a distinction is meaningful only for devices with a device node, or
      for network devices. For all other devices, the function returns [true].

      Using uninitialized devices is {i not} recommended.
  *)
  val is_initialized : t -> bool

  (** The time elapsed since initialization, as a number of microseconds.

      This is only implemented on devices which need to store properties in the
      udev database. For all other devices, [Uint64.zero] is returned. *)
  val usec_since_initialized : t -> Uint64.t

  (** {1 Device hierarchy } *)

  (** The parent of the device, if any. *)
  val parent : t -> t option

  (** Find the parent device within the given [subsystem] and optionally with
      the given [devtype], if any.

      [subsystem] contains the name of the subsystem, in which to search for the
      parent. [devtype] optionally holds the expected device type of the parent.  *)
  val find_parent : t -> ?devtype:string -> subsystem:string -> t option

  (** {1 Device events } *)

  (** A device event action, for devices received from a [Monitor.t]. *)
  type action =
    | Add
    | Remove
    | Change
    | Online
    | Offline
    | Other of string

  val string_of_action : action -> string
  
  (** The device event action. Returns [None] if the device was not received
      from a [Monitor.t] *)
  val action : t -> action option

  (** The device event sequence number, or [Uint64.zero] if this device has no
      sequence number, i.e. was not received from a [Monitor.t] *)
  val seqnum : t -> Uint64.t
    
  (** {1 Device properties } *)

  (** Return the properties defined for a device, as a [(key, value)] list. *)
  val properties : t -> (string * string) list

  (** Return the value of a property, if present. *)
  val property : t -> string -> string option
  val int_property : t -> string -> int option
  val bool_property : t -> string -> bool option

  (** {1 Sysfs attributes } *)

  (** Return the system attributes of the device, as a [(key, value)] list.

      System attributes are basically normal files inside the device
      directory. These files contain all sorts of information about the device,
      which may not be reflected by properties. These attributes are commonly
      used for matching in udev rules, and can be printed using [udevadm info
      --attribute-walk].  *)
  val sysattrs : t -> (string * string) list

  (** Return the value of an attribute, if present. *)
  val sysattr : t -> string -> string option
  val int_sysattr : t -> string -> int option
  val bool_sysattr : t -> string -> bool option

  (** Update the value of a device attribute. *)
  val set_sysattr : t -> string -> string -> unit
end

(** Device monitoring. *)
module Monitor : sig
  (** A [Monitor.t] monitor listens for changes to the device list. A monitor is
      created by connecting to the kernel daemon through netlink (see
      [Monitor.create]). *)
  type t

  (** Once the monitor is created, the devices that it listens to can be
      filtered using [Monitor.set_filter], specifying a list of [filter] values. *)
  type filter =
    | Tag of string
    (** [Tag tag] matches devices which have the tag [tag] attached. *)
    | Subsystem_devtype of string * string option
    (** [Subsystem_devtype (subsystem, devtype)] matches devices of [subsystem],
        with optionally device type [devtype]. *)

  (** Netlink source from which to listen to. See [Monitor.create]. *)
  type netlink_source = Udev | Kernel

  (** Create a monitor by connecting to the kernel daemon through netlink.

      By default, [source] is [Udev]: the monitor listens to the events emitted
      after udev has registered and configured the device. This is the absolutely
      recommended source for applications.

      A [source] of [Kernel] means that the monitor receives the events directly
      after the kernel has seen the device. The device has not yet been configured
      by udev and might not be usable at all. Do {b not} use this, unless you know
      what you are doing.
  *)
  val create : ?source:netlink_source -> Context.t -> t
    
  (** The context to which a monitor is bound. *)
  val context : t -> Context.t

  (** Set the receive buffer size, in bytes. *)
  val set_receive_buffer_size : t -> int -> unit

  (** Return the file descriptor associated with the monitor. *)
  val fd : t -> Unix.file_descr

  (** Filter the devices that are listened by a monitor. *)
  val set_filter : t -> filter list -> unit

  (** Start a monitor. A monitor [m] will not receive events until
      [Monitor.start m] is called. This function does nothing if called on an
      already started monitor. *)
  val start : t -> unit

  (** See [Monitor.receive_device]. *)
  exception Timeout

  (** Poll for a device event. [Monitor.receive_device ~timeout m] will block
      until a device event is received, or after [timeout] seconds, in which case
      the [Timeout] exception is raised. 

      If no [timeout] value is specified, the function blocks until a device
      event is available. A [timeout] of [0.] means that the function just polls
      and will never block.
  *)
  val receive_device : ?timeout:float -> t -> Device.t
end

(** Device enumeration and filtering. *)
module Enumerate : sig
  
  (** The devices enumerated using [Enumerate.devices] and
      [Enumerate.subsystems] can be filtered, specifying a list of [filter]
      values. *)
  type filter =
    | Subsystem of string
    (** [Subsystem subsystem] matches devices that are part of [subsystem]. *)
    | NoSubsystem of string
    (** [NoSubsystem subsystem] matches devices that are not part of
        [subsystem]. *)
    | Sysattr of string * string
    (** [Sysattr (attribute, value)] matches devices whose [attribute] has the
        given [value]. *)
    | NoSysattr of string * string
    (** [Sysattr (attribute, value)] matches devices whose [attribute] doesn't
        have the given [value]. Devices that do not have [attribute] at all are
        also included. *)
    | Property of string * string
    (** [Property (prop, value)] matches devices whose [prop] has the given
        [value]. *)
    | Sysname of string
    (** [Sysname name] matches devices with the given [name]. *)
    | Tag of string
    (** [Tag tag] matches devices which have the given [tag] attached. *)
    | Parent of Device.t
    (** [Parent parent] matches devices on the subtree of the given [parent]
        device. *)
    | Is_initialized
    (** [Is_initialized] matches devices which are initialized. 

        Initialized devices have properly set device node permissions and
        context, and are (in case of network devices) fully renamed.

        Currently this will not affect devices which do not have device dones
        and are not network interfaces.
    *)

  (** List the devices matching the given filter list. *)
  val devices : Context.t -> filter list -> Device.t list

  (** List the subsystems matching the given filter list. *)
  val subsystems : Context.t -> filter list -> Device.t list
end
