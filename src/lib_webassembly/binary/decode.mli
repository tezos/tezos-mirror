module Vector = Lazy_vector.LwtInt32Vector

exception Code of Source.region * string

(** Instruction parsing continuations. *)
type instr_block_kont =
  | IKStop of Ast.instr list  (** Final step of a block parsing. *)
  | IKRev of Ast.instr list * Ast.instr list
      (** Reversal of lists of instructions. *)
  | IKNext of Ast.instr list
      (** Tag parsing, containing the accumulation of already parsed values. *)
  | IKBlock of Ast.block_type * int  (** Block parsing step. *)
  | IKLoop of Ast.block_type * int  (** Loop parsing step. *)
  | IKIf1 of Ast.block_type * int  (** If parsing step. *)
  | IKIf2 of Ast.block_type * int * Ast.instr list
      (** If .. else parsing step. *)

(** Vector and size continuations *)

(** Vector accumulator, used in two step: first accumulating the values, then
    reversing them and possibly mapping them, counting the number of values in
    the list. Continuation passing style transformation of {!List.map} also
    returning length. *)
type ('a, 'b) vec_map_kont =
  | Collect of int * 'a list
  | Rev of 'a list * 'b list * int

(** Lazy vector accumulator, with the current offset to write the next value in
    the vector. *)
type 'a lazy_vec_kont = Lazy_vec of {offset : int32; vector : 'a Vector.t}

(** Position of a value on the stream. *)
type pos = private int

(** Size checking version of {!sized} for CPS-style parsing. *)
type size = {size : int; start : pos}

(** Incremental chunked byte vector creation (from implicit input). *)
type byte_vector_kont =
  | VKStart  (** Initial step. *)
  | VKRead of Chunked_byte_vector.Buffer.t * pos * int
      (** Reading step, containing the current position in the string and the
      length, reading byte per byte. *)
  | VKStop of Chunked_byte_vector.Buffer.t  (** Final step, cannot reduce. *)

type name_step =
  | NKStart  (** UTF8 name starting point. *)
  | NKParse of pos * int lazy_vec_kont * int  (** UTF8 char parsing. *)
  | NKStop of Ast.name  (** UTF8 name final step.*)

type func_type_kont =
  | FKStart
  | FKIns of Types.value_type lazy_vec_kont
  | FKOut of Types.value_type Vector.t * Types.value_type lazy_vec_kont
  | FKStop of Types.func_type

type import_kont =
  | ImpKStart  (** Import parsing starting point. *)
  | ImpKModuleName of name_step
      (** Import module name parsing UTF8 char per char step. *)
  | ImpKItemName of Ast.name * name_step
      (** Import item name parsing UTF8 char per char step. *)
  | ImpKStop of Ast.import'  (** Import final step. *)

type export_kont =
  | ExpKStart  (** Export parsing starting point. *)
  | ExpKName of name_step  (** Export name parsing UTF8 char per char step. *)
  | ExpKStop of Ast.export'  (** Export final step. *)

(** Code section parsing. *)
type code_kont =
  | CKStart  (** Starting point of a function parsing. *)
  | CKLocalsParse of {
      left : pos;
      size : size;
      pos : pos;
      vec_kont : (int32 * Types.value_type) lazy_vec_kont;
      locals_size : Int64.t;
    }  (** Parse a local value with its number of occurences. *)
  | CKLocalsAccumulate of {
      left : pos;
      size : size;
      pos : pos;
      type_vec : (int32 * Types.value_type) lazy_vec_kont;
      curr_type : (int32 * Types.value_type) option;
      vec_kont : Types.value_type lazy_vec_kont;
    }  (** Accumulate local values. *)
  | CKBody of {
      left : pos;
      size : size;
      locals : Types.value_type Vector.t;
      const_kont : instr_block_kont list;
    }  (** Parsing step of the body of a function. *)
  | CKStop of Ast.func  (** Final step of a parsed function, irreducible. *)

type index_kind = Indexed | Const

type elem_kont =
  | EKStart  (** Starting point of an element segment parsing. *)
  | EKMode of {
      left : pos;
      index : int32 Source.phrase;
      index_kind : index_kind;
      early_ref_type : Types.ref_type option;
      offset_kont : pos * instr_block_kont list;
    }  (** Element segment mode parsing step. *)
  | EKInitIndexed of {
      mode : Ast.segment_mode;
      ref_type : Types.ref_type;
      einit_vec : Ast.const lazy_vec_kont;
    }
      (** Element segment initialization code parsing step for referenced values. *)
  | EKInitConst of {
      mode : Ast.segment_mode;
      ref_type : Types.ref_type;
      einit_vec : Ast.const lazy_vec_kont;
      einit_kont : pos * instr_block_kont list;
    }
      (** Element segment initialization code parsing step for constant values. *)
  | EKStop of Ast.elem_segment'  (** Final step of a segment parsing. *)

type data_kont =
  | DKStart  (** Starting point of a data segment parsing. *)
  | DKMode of {
      left : pos;
      index : int32 Source.phrase;
      offset_kont : pos * instr_block_kont list;
    }  (** Data segment mode parsing step. *)
  | DKInit of {dmode : Ast.segment_mode; init_kont : byte_vector_kont}
  | DKStop of Ast.data_segment'  (** Final step of a data segment parsing. *)

(** Representation of a section tag. *)
type section_tag =
  [ `CodeSection
  | `CustomSection
  | `DataCountSection
  | `DataSection
  | `ElemSection
  | `ExportSection
  | `FuncSection
  | `GlobalSection
  | `ImportSection
  | `MemorySection
  | `StartSection
  | `TableSection
  | `TypeSection ]

(** Sections representation. *)
type _ field_type =
  | TypeField : Ast.type_ field_type
  | ImportField : Ast.import field_type
  | FuncField : Ast.var field_type
  | TableField : Ast.table field_type
  | MemoryField : Ast.memory field_type
  | GlobalField : Ast.global field_type
  | ExportField : Ast.export field_type
  | StartField : Ast.start field_type
  | ElemField : Ast.elem_segment field_type
  | DataCountField : int32 field_type
  | CodeField : Ast.func field_type
  | DataField : Ast.data_segment field_type

(** Result of a section parsing, being either a single value or a vector. *)
type field =
  | VecField : 'a field_type * 'a Vector.t -> field
  | SingleField : 'a field_type * 'a option -> field

(** Module parsing steps *)
type module_kont =
  | MKStart  (** Initial state of a module parsing *)
  | MKSkipCustom : ('a field_type * section_tag) option -> module_kont
      (** Custom section which are skipped, with the next section to parse. *)
  | MKFieldStart : 'a field_type * section_tag -> module_kont
      (** Starting point of a section, handles parsing generic section header. *)
  | MKField : 'a field_type * size * 'a lazy_vec_kont -> module_kont
      (** Section currently parsed, accumulating each element from the underlying vector. *)
  | MKElaborateFunc :
      Ast.var Vector.t * Ast.func Vector.t * Ast.func lazy_vec_kont * bool
      -> module_kont
      (** Elaboration of functions from the code section with their declared type in
      the func section, and accumulating invariants conditions associated to
      functions. *)
  | MKBuild of Ast.func Vector.t option * bool
      (** Accumulating the parsed sections vectors into a module and checking
      invariants. *)
  | MKStop of Ast.module_' (* TODO (#3120): actually, should be module_ *)
      (** Final step of the parsing, cannot reduce. *)
  | MKTypes of func_type_kont * pos * size * Ast.type_ lazy_vec_kont
      (** Function types section parsing. *)
  | MKImport of import_kont * pos * size * Ast.import lazy_vec_kont
      (** Import section parsing. *)
  | MKExport of export_kont * pos * size * Ast.export lazy_vec_kont
      (** Export section parsing. *)
  | MKGlobal of
      Types.global_type
      * int
      * instr_block_kont list
      * size
      * Ast.global lazy_vec_kont
      (** Globals section parsing, containing the starting position, the
      continuation of the current global block instruction, and the size of the
      section. *)
  | MKElem of elem_kont * int * size * Ast.elem_segment lazy_vec_kont
      (** Element segments section parsing, containing the current element parsing
      continuation, the starting position of the current element, the size of
      the section. *)
  | MKData of data_kont * int * size * Ast.data_segment lazy_vec_kont
      (** Data segments section parsing, containing the current data parsing
      continuation, the starting position of the current data, the size of the
      section. *)
  | MKCode of code_kont * int * size * Ast.func lazy_vec_kont
      (** Code section parsing, containing the current function parsing
      continuation, the starting position of the current function, the size of
      the section. *)

(** Parsed bytes with the current reading position. *)
type stream = {name : string; bytes : string; pos : pos ref}

(** Decoding continuation step. *)
type decode_kont = {
  building_state : field Vector.t;
      (** Accumulated parsed sections, used to build the final module. *)
  module_kont : module_kont;  (** Module continuation. *)
  stream : stream;  (** Parsed stream. *)
}

(** [make_stream filename bytes] returns a new stream to decode. *)
val make_stream : name:string -> bytes:string -> stream

(** [module_step kont] takes one step of parsing from a continuation and returns
   a new continuation. Fails when the contination of the module is [MKStop]
   since it cannot reduce. *)
val module_step : decode_kont -> decode_kont Lwt.t

(** [decode ~name ~bytes] decodes a module [name] from its [bytes] encoding.

    @raise Code on parsing errors. *)
val decode : name:string -> bytes:string -> Ast.module_ Lwt.t

(** [decode ~name ~bytes] decodes a custom section of name [name] from its
    [bytes] encoding.

    @raise Code on parsing errors. *)
val decode_custom : Ast.name -> name:string -> bytes:string -> string list
