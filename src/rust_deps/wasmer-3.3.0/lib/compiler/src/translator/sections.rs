// This file contains code from external sources.
// Attributions: https://github.com/wasmerio/wasmer/blob/master/ATTRIBUTIONS.md

//! Helper functions to gather information for each of the non-function sections of a
//! WebAssembly module.
//!
//! The code of these helper functions is straightforward since they only read metadata
//! about linear memories, tables, globals, etc. and store them for later use.
//!
//! The special case of the initialize expressions for table elements offsets or global variables
//! is handled, according to the semantics of WebAssembly, to only specific expressions that are
//! interpreted on the fly.
use super::environ::ModuleEnvironment;
use super::error::from_binaryreadererror_wasmerror;
use super::state::ModuleTranslationState;
use crate::wasm_unsupported;
use core::convert::TryFrom;
use std::boxed::Box;
use std::vec::Vec;
use wasmer_types::entity::packed_option::ReservedValue;
use wasmer_types::entity::EntityRef;
use wasmer_types::{
    DataIndex, ElemIndex, FunctionIndex, FunctionType, GlobalIndex, GlobalInit, GlobalType,
    MemoryIndex, MemoryType, Pages, SignatureIndex, TableIndex, TableType, Type, V128,
};
use wasmer_types::{WasmError, WasmResult};
use wasmparser::{
    self, Data, DataKind, DataSectionReader, Element, ElementItem, ElementItems, ElementKind,
    ElementSectionReader, Export, ExportSectionReader, ExternalKind, FunctionSectionReader,
    GlobalSectionReader, GlobalType as WPGlobalType, ImportSectionReader, MemorySectionReader,
    MemoryType as WPMemoryType, NameSectionReader, Operator, TableSectionReader, TypeRef,
    TypeSectionReader,
};

/// Helper function translating wasmparser types to Wasm Type.
pub fn wptype_to_type(ty: wasmparser::ValType) -> WasmResult<Type> {
    match ty {
        wasmparser::ValType::I32 => Ok(Type::I32),
        wasmparser::ValType::I64 => Ok(Type::I64),
        wasmparser::ValType::F32 => Ok(Type::F32),
        wasmparser::ValType::F64 => Ok(Type::F64),
        wasmparser::ValType::V128 => Ok(Type::V128),
        wasmparser::ValType::ExternRef => Ok(Type::ExternRef),
        wasmparser::ValType::FuncRef => Ok(Type::FuncRef),
    }
}

/// Parses the Type section of the wasm module.
pub fn parse_type_section(
    types: TypeSectionReader,
    module_translation_state: &mut ModuleTranslationState,
    environ: &mut ModuleEnvironment,
) -> WasmResult<()> {
    let count = types.get_count();
    environ.reserve_signatures(count)?;

    for entry in types {
        if let Ok(wasmparser::Type::Func(functype)) = entry {
            let params = functype.params();
            let returns = functype.results();
            let sig_params: Box<[Type]> = params
                .iter()
                .map(|ty| {
                    wptype_to_type(*ty)
                        .expect("only numeric types are supported in function signatures")
                })
                .collect();
            let sig_returns: Box<[Type]> = returns
                .iter()
                .map(|ty| {
                    wptype_to_type(*ty)
                        .expect("only numeric types are supported in function signatures")
                })
                .collect();
            let sig = FunctionType::new(sig_params, sig_returns);
            environ.declare_signature(sig)?;
            module_translation_state
                .wasm_types
                .push((params.to_vec().into(), returns.to_vec().into()));
        } else {
            unimplemented!("module linking not implemented yet")
        }
    }

    Ok(())
}

/// Parses the Import section of the wasm module.
pub fn parse_import_section<'data>(
    imports: ImportSectionReader<'data>,
    environ: &mut ModuleEnvironment<'data>,
) -> WasmResult<()> {
    environ.reserve_imports(imports.get_count())?;

    for entry in imports {
        let import = entry.map_err(from_binaryreadererror_wasmerror)?;
        let module_name = import.module;
        let field_name = import.name;

        match import.ty {
            TypeRef::Func(sig) => {
                environ.declare_func_import(
                    SignatureIndex::from_u32(sig),
                    module_name,
                    field_name,
                )?;
            }
            TypeRef::Tag(_) => {
                unimplemented!("exception handling not implemented yet")
            }
            TypeRef::Memory(WPMemoryType {
                shared,
                memory64,
                initial,
                maximum,
            }) => {
                if memory64 {
                    unimplemented!("64bit memory not implemented yet");
                }
                environ.declare_memory_import(
                    MemoryType {
                        minimum: Pages(initial as u32),
                        maximum: maximum.map(|p| Pages(p as u32)),
                        shared,
                    },
                    module_name,
                    field_name,
                )?;
            }
            TypeRef::Global(ref ty) => {
                environ.declare_global_import(
                    GlobalType {
                        ty: wptype_to_type(ty.content_type).unwrap(),
                        mutability: ty.mutable.into(),
                    },
                    module_name,
                    field_name,
                )?;
            }
            TypeRef::Table(ref tab) => {
                environ.declare_table_import(
                    TableType {
                        ty: wptype_to_type(tab.element_type).unwrap(),
                        minimum: tab.initial,
                        maximum: tab.maximum,
                    },
                    module_name,
                    field_name,
                )?;
            }
        }
    }

    environ.finish_imports()?;
    Ok(())
}

/// Parses the Function section of the wasm module.
pub fn parse_function_section(
    functions: FunctionSectionReader,
    environ: &mut ModuleEnvironment,
) -> WasmResult<()> {
    let num_functions = functions.get_count();
    if num_functions == std::u32::MAX {
        // We reserve `u32::MAX` for our own use.
        return Err(WasmError::ImplLimitExceeded);
    }

    environ.reserve_func_types(num_functions)?;

    for entry in functions {
        let sigindex = entry.map_err(from_binaryreadererror_wasmerror)?;
        environ.declare_func_type(SignatureIndex::from_u32(sigindex))?;
    }

    Ok(())
}

/// Parses the Table section of the wasm module.
pub fn parse_table_section(
    tables: TableSectionReader,
    environ: &mut ModuleEnvironment,
) -> WasmResult<()> {
    environ.reserve_tables(tables.get_count())?;

    for entry in tables {
        let table = entry.map_err(from_binaryreadererror_wasmerror)?;
        environ.declare_table(TableType {
            ty: wptype_to_type(table.element_type).unwrap(),
            minimum: table.initial,
            maximum: table.maximum,
        })?;
    }

    Ok(())
}

/// Parses the Memory section of the wasm module.
pub fn parse_memory_section(
    memories: MemorySectionReader,
    environ: &mut ModuleEnvironment,
) -> WasmResult<()> {
    environ.reserve_memories(memories.get_count())?;

    for entry in memories {
        let WPMemoryType {
            shared,
            memory64,
            initial,
            maximum,
        } = entry.map_err(from_binaryreadererror_wasmerror)?;
        if memory64 {
            unimplemented!("64bit memory not implemented yet");
        }
        environ.declare_memory(MemoryType {
            minimum: Pages(initial as u32),
            maximum: maximum.map(|p| Pages(p as u32)),
            shared,
        })?;
    }

    Ok(())
}

/// Parses the Global section of the wasm module.
pub fn parse_global_section(
    globals: GlobalSectionReader,
    environ: &mut ModuleEnvironment,
) -> WasmResult<()> {
    environ.reserve_globals(globals.get_count())?;

    for entry in globals {
        let wasmparser::Global {
            ty: WPGlobalType {
                content_type,
                mutable,
            },
            init_expr,
        } = entry.map_err(from_binaryreadererror_wasmerror)?;
        let mut init_expr_reader = init_expr.get_binary_reader();
        let initializer = match init_expr_reader
            .read_operator()
            .map_err(from_binaryreadererror_wasmerror)?
        {
            Operator::I32Const { value } => GlobalInit::I32Const(value),
            Operator::I64Const { value } => GlobalInit::I64Const(value),
            Operator::F32Const { value } => GlobalInit::F32Const(f32::from_bits(value.bits())),
            Operator::F64Const { value } => GlobalInit::F64Const(f64::from_bits(value.bits())),
            Operator::V128Const { value } => GlobalInit::V128Const(V128::from(*value.bytes())),
            Operator::RefNull { ty: _ } => GlobalInit::RefNullConst,
            Operator::RefFunc { function_index } => {
                GlobalInit::RefFunc(FunctionIndex::from_u32(function_index))
            }
            Operator::GlobalGet { global_index } => {
                GlobalInit::GetGlobal(GlobalIndex::from_u32(global_index))
            }
            ref s => {
                return Err(wasm_unsupported!(
                    "unsupported init expr in global section: {:?}",
                    s
                ));
            }
        };
        let global = GlobalType {
            ty: wptype_to_type(content_type).unwrap(),
            mutability: mutable.into(),
        };
        environ.declare_global(global, initializer)?;
    }

    Ok(())
}

/// Parses the Export section of the wasm module.
pub fn parse_export_section<'data>(
    exports: ExportSectionReader<'data>,
    environ: &mut ModuleEnvironment<'data>,
) -> WasmResult<()> {
    environ.reserve_exports(exports.get_count())?;

    for entry in exports {
        let Export {
            name: field,
            ref kind,
            index,
        } = entry.map_err(from_binaryreadererror_wasmerror)?;

        // The input has already been validated, so we should be able to
        // assume valid UTF-8 and use `from_utf8_unchecked` if performance
        // becomes a concern here.
        let index = index as usize;
        match *kind {
            ExternalKind::Func => environ.declare_func_export(FunctionIndex::new(index), field)?,
            ExternalKind::Table => environ.declare_table_export(TableIndex::new(index), field)?,
            ExternalKind::Memory => {
                environ.declare_memory_export(MemoryIndex::new(index), field)?
            }
            ExternalKind::Global => {
                environ.declare_global_export(GlobalIndex::new(index), field)?
            }
            ExternalKind::Tag => {
                unimplemented!("exception handling not implemented yet")
            }
        }
    }

    environ.finish_exports()?;
    Ok(())
}

/// Parses the Start section of the wasm module.
pub fn parse_start_section(index: u32, environ: &mut ModuleEnvironment) -> WasmResult<()> {
    environ.declare_start_function(FunctionIndex::from_u32(index))?;
    Ok(())
}

fn read_elems(items: &ElementItems) -> WasmResult<Box<[FunctionIndex]>> {
    let items_reader = items
        .get_items_reader()
        .map_err(from_binaryreadererror_wasmerror)?;
    let mut elems = Vec::with_capacity(usize::try_from(items_reader.get_count()).unwrap());
    for item in items_reader {
        let elem = match item.map_err(from_binaryreadererror_wasmerror)? {
            ElementItem::Expr(init) => match init
                .get_binary_reader()
                .read_operator()
                .map_err(from_binaryreadererror_wasmerror)?
            {
                Operator::RefNull { .. } => FunctionIndex::reserved_value(),
                Operator::RefFunc { function_index } => FunctionIndex::from_u32(function_index),
                s => {
                    return Err(WasmError::Unsupported(format!(
                        "unsupported init expr in element section: {:?}",
                        s
                    )));
                }
            },
            ElementItem::Func(index) => FunctionIndex::from_u32(index),
        };
        elems.push(elem);
    }
    Ok(elems.into_boxed_slice())
}

/// Parses the Element section of the wasm module.
pub fn parse_element_section<'data>(
    elements: ElementSectionReader<'data>,
    environ: &mut ModuleEnvironment,
) -> WasmResult<()> {
    environ.reserve_table_initializers(elements.get_count())?;

    for (index, entry) in elements.into_iter().enumerate() {
        let Element {
            kind,
            items,
            ty,
            range: _,
        } = entry.map_err(from_binaryreadererror_wasmerror)?;
        if ty != wasmparser::ValType::FuncRef {
            return Err(wasm_unsupported!(
                "unsupported table element type: {:?}",
                ty
            ));
        }
        let segments = read_elems(&items)?;
        match kind {
            ElementKind::Active {
                table_index,
                offset_expr,
            } => {
                let mut init_expr_reader = offset_expr.get_binary_reader();
                let (base, offset) = match init_expr_reader
                    .read_operator()
                    .map_err(from_binaryreadererror_wasmerror)?
                {
                    Operator::I32Const { value } => (None, value as u32 as usize),
                    Operator::GlobalGet { global_index } => {
                        (Some(GlobalIndex::from_u32(global_index)), 0)
                    }
                    ref s => {
                        return Err(wasm_unsupported!(
                            "unsupported init expr in element section: {:?}",
                            s
                        ));
                    }
                };
                environ.declare_table_initializers(
                    TableIndex::from_u32(table_index),
                    base,
                    offset,
                    segments,
                )?
            }
            ElementKind::Passive => {
                let index = ElemIndex::from_u32(index as u32);
                environ.declare_passive_element(index, segments)?;
            }
            ElementKind::Declared => (),
        }
    }
    Ok(())
}

/// Parses the Data section of the wasm module.
pub fn parse_data_section<'data>(
    data: DataSectionReader<'data>,
    environ: &mut ModuleEnvironment<'data>,
) -> WasmResult<()> {
    environ.reserve_data_initializers(data.get_count())?;

    for (index, entry) in data.into_iter().enumerate() {
        let Data {
            kind,
            data,
            range: _,
        } = entry.map_err(from_binaryreadererror_wasmerror)?;
        match kind {
            DataKind::Active {
                memory_index,
                offset_expr,
            } => {
                let mut init_expr_reader = offset_expr.get_binary_reader();
                let (base, offset) = match init_expr_reader
                    .read_operator()
                    .map_err(from_binaryreadererror_wasmerror)?
                {
                    Operator::I32Const { value } => (None, value as u32 as usize),
                    Operator::GlobalGet { global_index } => {
                        (Some(GlobalIndex::from_u32(global_index)), 0)
                    }
                    ref s => {
                        return Err(wasm_unsupported!(
                            "unsupported init expr in data section: {:?}",
                            s
                        ))
                    }
                };
                environ.declare_data_initialization(
                    MemoryIndex::from_u32(memory_index),
                    base,
                    offset,
                    data,
                )?;
            }
            DataKind::Passive => {
                let index = DataIndex::from_u32(index as u32);
                environ.declare_passive_data(index, data)?;
            }
        }
    }

    Ok(())
}

/// Parses the Name section of the wasm module.
pub fn parse_name_section<'data>(
    mut names: NameSectionReader<'data>,
    environ: &mut ModuleEnvironment<'data>,
) -> WasmResult<()> {
    while let Ok(subsection) = names.read() {
        match subsection {
            wasmparser::Name::Function(function_subsection) => {
                for naming in function_subsection.into_iter().flatten() {
                    if naming.index != std::u32::MAX {
                        environ.declare_function_name(
                            FunctionIndex::from_u32(naming.index),
                            naming.name,
                        )?;
                    }
                }
            }
            wasmparser::Name::Module {
                name,
                name_range: _,
            } => {
                environ.declare_module_name(name)?;
            }
            wasmparser::Name::Local(_) => {}
            wasmparser::Name::Label(_)
            | wasmparser::Name::Type(_)
            | wasmparser::Name::Table(_)
            | wasmparser::Name::Memory(_)
            | wasmparser::Name::Global(_)
            | wasmparser::Name::Element(_)
            | wasmparser::Name::Data(_)
            | wasmparser::Name::Unknown { .. } => {}
        };
    }
    Ok(())
}
