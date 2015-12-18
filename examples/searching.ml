open Libudev

let str out = function
  | None -> Printf.fprintf out "-"
  | Some s -> Printf.fprintf out "%s" s

let _ =
  (* Create the udev context *)
  let ctx = Context.create () in

  (* Create a list of the devices in the 'hidraw' subsystem. *)
  Enumerate.(devices ctx [Subsystem "hidraw"]) |>

  (* For each item enumerated, print out its information. *)
  List.iter (fun dev ->

    (* [Device.devnode] returns the path to the device node itself in /dev. *)
    Printf.printf "Device node path: %a\n" str (Device.devnode dev);

    (* The device pointed to by [dev] contains information about the hidraw
       device. In order to get information about the USB device, get the parent
       device with subsystem/devtype pair of "usb"/"usb_device". This will be
       several levels up the tree, but the function will find it. *)
    match Device.find_parent dev ~subsystem:"usb" ~devtype:"usb_device" with
    | None ->
      Printf.printf "Unable to find parent usb device.\n"; exit 1
    | Some dev ->

      (* From here, we can call [Device.sysattr] for each file in the device's
         /sys entry. The strings passed into these functions (idProduct, idVendor,
         serial, etc.) correspond directly to the files in the directory which
         represents the USB device. Note that USB strings are Unicode, UCS2
         encoded, but the strings returned from [Device.sysattr] are UTF-8
         encoded. *)
      Printf.printf "  VID/PID: %a %a\n"
        str (Device.sysattr dev "idVendor")
        str (Device.sysattr dev "idProduct");
      Printf.printf "  %a\n  %a\n"
        str (Device.sysattr dev "manufacturer")
        str (Device.sysattr dev "product");
      Printf.printf "  serial: %a\n"
        str (Device.sysattr dev "serial");
  )
