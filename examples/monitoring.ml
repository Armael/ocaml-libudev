open Libudev

let rec sleep t =
  if t > 0. then
    let now = Unix.gettimeofday () in
    (try ignore (Unix.select [] [] [] t) with
     | _ -> ());
        sleep (t -. ((Unix.gettimeofday ()) -. now))

let opt p out = function
  | None -> Printf.fprintf out "-"
  | Some x -> Printf.fprintf out "%s" (p x)

let str = opt (fun x -> x)

let _ =
  let ctx = Context.create () in

  (* Set up a monitor to monitor hidraw devices *)
  let mon = Monitor.create ctx in
  Monitor.set_filter mon [Monitor.Subsystem_devtype ("hidraw", None)];
  Monitor.start mon;

  while true do
    begin try
        (* A timeout of 0 will cause [Monitor.receive_device] to not block.
           [Monitor.Timeout] will be raised if no device is available. *)
        let dev = Monitor.receive_device ~timeout:0. mon in
        Printf.printf "Got Device\n";
        Printf.printf "   Node: %a\n" str (Device.devnode dev);
        Printf.printf "   Subsystem: %s\n" (Device.subsystem dev);
        Printf.printf "   Devtype: %a\n" str (Device.devtype dev);
        Printf.printf "   Action: %a\n%!"
          (opt Device.string_of_action)
          (Device.action dev)
      with Monitor.Timeout ->
        Printf.printf ".%!"
    end;
    sleep 0.250
  done
