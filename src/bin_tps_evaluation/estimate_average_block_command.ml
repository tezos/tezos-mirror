(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** The different kinds of Tezos operations we're analyzing. *)
type transaction_kind = Contract | Regular | Origination

(** The rows returned by the summary query. *)
type summary_row = {transaction_kind : transaction_kind; operation_count : int}

(** The rows returned by the contract query. *)
type contract_row = {contract_address : string; total_contract_operations : int}

module Encoding = struct
  (** Helper functions to convert from and to the Caqti_type used to interface with the database. *)
  let transaction_kind =
    let encode = function
      | Contract -> Ok "contract"
      | Regular -> Ok "regular"
      | Origination -> Ok "origination"
    in
    let decode = function
      | "contract" -> Ok Contract
      | "regular" -> Ok Regular
      | "origination" -> Ok Origination
      | v ->
          Error ("value of " ^ v ^ " could not be decoded as a transaction type")
    in
    let rep = Caqti_type.string in
    Caqti_type.custom ~encode ~decode rep

  let summary_row =
    let encode {transaction_kind; operation_count} =
      Ok (transaction_kind, operation_count)
    in
    let decode (transaction_kind, operation_count) =
      Ok {transaction_kind; operation_count}
    in
    let rep = Caqti_type.(t2 transaction_kind int) in
    Caqti_type.custom ~encode ~decode rep

  let contract_row =
    let encode {contract_address; total_contract_operations} =
      Ok (contract_address, total_contract_operations)
    in
    let decode (contract_address, total_contract_operations) =
      Ok {contract_address; total_contract_operations}
    in
    let rep = Caqti_type.(t2 string int) in
    Caqti_type.custom ~encode ~decode rep
end

module Db = struct
  (** Establish a connection pool that will be used to make the database queries. *)
  let mk_pool conn_str =
    match
      Caqti_lwt_unix.connect_pool
        ~pool_config:(Caqti_pool_config.create ~max_size:10 ())
        (Uri.of_string conn_str)
    with
    | Ok pool -> pool
    | Error err -> Stdlib.failwith (Caqti_error.show err)

  (** Retrieve the top N contracts used as destination in transactions. *)
  let top_n_contracts =
    let query =
      Printf.sprintf
        {|
      WITH _group AS (
        SELECT
          total.tx_dst_addr,
          count(total.tx_dst_addr) AS ops_count
        FROM
          (
            -- included subquery on a new line to avoid breaking single line comments
            %s
          ) AS total
        WHERE
          total.transaction_kind = 'contract'
        GROUP BY
          total.tx_dst_addr
      )

      SELECT _final.tx_dst_addr, _final.ops_count FROM (
        SELECT *,
               COALESCE(CAST(1.0 * ops_count / nullif (sum(ops_count) OVER (), 0) AS numeric(6, 5)), 0) AS percentage
        FROM _group
      ) AS _final
      WHERE _final.percentage >= ?
    |}
        Sql.get_all_operations_sql
    in
    Caqti_request.Infix.(
      Caqti_type.(t3 string string float) ->! Encoding.contract_row)
      query

  let get_top_contracts conn_str start_date end_date limit () =
    let main' (module C : Caqti_lwt.CONNECTION) =
      C.fold
        top_n_contracts
        (fun row acc ->
          {
            contract_address = row.contract_address;
            total_contract_operations = row.total_contract_operations;
          }
          :: acc)
        (start_date, end_date, limit)
        []
    in
    Caqti_lwt_unix.Pool.use main' (mk_pool conn_str)

  (** The main analysis query that categorizes the Tezos operations within a given time frame. *)
  let summary_query =
    let query =
      Printf.sprintf
        {|
      SELECT
        total.transaction_kind,
        count(total.transaction_kind) AS ops_count
      FROM
        (
          -- included subquery on a new line to avoid breaking single line comments
          %s
        ) AS total
      GROUP BY
        total.transaction_kind
    |}
        Sql.get_all_operations_sql
    in
    Caqti_request.Infix.(Caqti_type.(t2 string string) ->! Encoding.summary_row)
      query

  let get_operation_summary conn_str start_date end_date () =
    let main' (module C : Caqti_lwt.CONNECTION) =
      C.fold
        summary_query
        (fun row acc ->
          {
            transaction_kind = row.transaction_kind;
            operation_count = row.operation_count;
          }
          :: acc)
        (start_date, end_date)
        []
    in
    Caqti_lwt_unix.Pool.use main' (mk_pool conn_str)
end

module Json = struct
  (** Calculate the total count operations of the given type. *)
  let get_total_by_type (rows : summary_row list) (ty : transaction_kind) : int
      =
    List.fold_left
      ( + )
      0
      (List.map
         (fun a -> a.operation_count)
         (List.filter (fun a -> a.transaction_kind == ty) rows))

  let list_contract rows =
    List.map (fun a -> (a.contract_address, a.total_contract_operations)) rows

  (** Construct the final JSON object to present the result of the analysis. *)
  let show_summary summary top_contracts =
    let json_output =
      Average_block.
        {
          regular = get_total_by_type summary Regular;
          origination = get_total_by_type summary Origination;
          contract = list_contract top_contracts;
        }
    in
    print_string
      (Data_encoding.Json.to_string
         ~newline:true
         ~minify:false
         (Data_encoding.Json.construct Average_block.encoding json_output))
end

(** Execute the query against the database and formats the result. *)
let query_db start_date end_date contract_min_percentage conn_str =
  let open Lwt_result_syntax in
  let* summary = Db.get_operation_summary conn_str start_date end_date () in
  let* top_contracts =
    Db.get_top_contracts conn_str start_date end_date contract_min_percentage ()
  in
  Json.show_summary summary top_contracts ;
  return_unit

let query_db start_date end_date contract_min_percentage conn_str =
  let open Lwt_syntax in
  let+ r = query_db start_date end_date contract_min_percentage conn_str in
  WithExceptions.Result.to_exn_f
    ~error:(fun e -> Stdlib.Failure (Caqti_error.show e))
    r

let register () =
  Long_test.register
    ~__FILE__
    ~title:Dashboard.Test.estimate_average_block
    ~tags:[Dashboard.Test.estimate_average_block]
    ~timeout:(Long_test.Minutes 60)
    ~executors:Long_test.[x86_executor1]
    (fun () ->
      let connection_string =
        Cli.get_string
          ~default:"postgresql://postgres:postgres@localhost:5432/postgres"
          "connection-string"
      in
      let start_date = Cli.get_string "start-date" in
      let end_date = Cli.get_string "end-date" in
      let contract_min_percentage =
        Cli.get_float ~default:0.1 "contract-min-percentage"
      in
      query_db start_date end_date contract_min_percentage connection_string)
