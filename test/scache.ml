module Cache = Scache.Cache

let cache = Cache.start ~name:"testing001"
let () = Scache.remove_cache ~name:"testing001"


let recording () =
  let expected = Unit.(()) in
  let result = Cache.set cache ~key:"key1" ~value:"value1" in
  Alcotest.(check unit) "without internal fails" expected result

let reading () =
  Cache.set cache ~key:"key1" ~value:"value1";
  let expected = Some "value1" in
  let result = Cache.get cache ~key:"key1" in
  Alcotest.(check (option string)) "same value" expected result

let removing () =
  Cache.set cache ~key:"key2" ~value:"value1";
  Cache.remove cache ~key:"key2";
  let expected = None in
  let result = Cache.get cache ~key:"key2" in
  Alcotest.(check (option string)) "same value" expected result

let modifying () =
  Cache.set cache ~key:"key3" ~value:"value1";
  let _ = Cache.get cache ~key:"key3" in
  Cache.set cache ~key:"key3" ~value:"value2";
  let expected = Some "value2" in
  let result = Cache.get cache ~key:"key3" in
  Alcotest.(check (option string)) "same value" expected result


let () =
  let open Alcotest in
  run "Cache" [
    "simple", [
      test_case "recording" `Quick recording;
      test_case "reading"   `Quick reading;
      test_case "removing"  `Quick removing;
      test_case "modifying" `Quick modifying
    ]
  ]
