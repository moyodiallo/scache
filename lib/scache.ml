module Sqlite = Sqlite3
module Names = Set.Make(String)

module Db = struct

  type t = {
    get_value : Sqlite.stmt;
    record_value : Sqlite.stmt;
    drop_value : Sqlite.stmt;
    record_name : Sqlite.stmt;
    drop_name : Sqlite.stmt;
    mutable names: Names.t;
  }

  let or_fail label x =
    match x with
    | Sqlite3.Rc.OK -> ()
    | err -> Fmt.failwith "Sqlite3 %s error: %s" label (Sqlite3.Rc.to_string err)

  let format_timestamp time =
    let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = time in
    Fmt.str "%04d-%02d-%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

  let db = lazy (
    let db = Lazy.force (Db.v ()) in
    Sqlite.exec db "CREATE TABLE IF NOT EXISTS cache ( \
                    name      TEXT NOT NULL, \
                    key       BLOB, \
                    value     BLOB, \
                    time      DATETIME, \
                    PRIMARY KEY (name, key))" |> or_fail "create table cache";
    Sqlite.exec db "CREATE TABLE IF NOT EXISTS names AS SELECT DISTINCT name FROM cache" |> or_fail "create table names";
    let get_value = Sqlite.prepare db "SELECT value FROM cache WHERE name = ? AND key = ? LIMIT 1" in
    let drop_name = Sqlite.prepare db "DELETE FROM cache WHERE name = ?" in
    let drop_value = Sqlite.prepare db "DELETE FROM cache WHERE name = ? AND key = ?" in
    let record_name = Sqlite.prepare db "INSERT OR IGNORE INTO names (name) VALUES (?)" in
    let record_value = Sqlite.prepare db "INSERT OR REPLACE INTO cache (name, key, value, time) VALUES (?, ?, ?, ?)" in
    let names =
      let stmt = Sqlite.prepare db "SELECT name FROM names" in
      Db.query stmt [] |> List.map (function
          | Sqlite.Data.[TEXT name] -> name
          | row -> Fmt.failwith "Invalid names result: %a" Db.dump_row row
        )
      |> Names.of_list
    in
    { get_value; record_value; drop_value; record_name; drop_name; names }
  )

  let record_value ~name ~key ~value ~time =
     let t = Lazy.force db in
     if not (Names.mem name t.names) then (
       t.names <- Names.add name t.names;
       Db.exec t.record_name Sqlite.Data.[ TEXT name]
     );
     Db.exec t.record_value Sqlite.Data.[ TEXT name; BLOB key; BLOB value; TEXT (format_timestamp time) ]

  let get_value ~name key =
    let t = Lazy.force db in
    Db.query_some t.get_value Sqlite.Data.[ TEXT name; BLOB key] |> function
    | None -> None
    | Some Sqlite.Data.[ BLOB value] -> Some value
    | Some row -> Fmt.failwith "Invalid get_value result: %a" Db.dump_row row

  let drop_value ~name key =
    let t = Lazy.force db in
    Db.exec t.drop_value Sqlite.Data.[ TEXT name; BLOB key]

  let drop_name ~name =
    let t = Lazy.force db in
    Db.exec t.drop_name Sqlite.Data.[ TEXT name]

end

let remove_cache ~name = Db.drop_name ~name

module Cache = struct

  type t = { name:string }

  let get t ~key :string option = 
    match Db.get_value ~name:t.name key with
    | None -> None
    | Some value -> Some value

  let set t ~key ~value : unit =
    let time = Unix.gmtime (Unix.time ()) in
    Db.record_value ~name:t.name ~key ~value ~time

  let remove t ~key =
    Db.drop_value ~name:t.name key

  let start ~name = { name }
end
