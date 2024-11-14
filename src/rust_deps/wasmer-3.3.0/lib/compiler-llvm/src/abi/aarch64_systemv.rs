use crate::abi::Abi;
use crate::translator::intrinsics::{type_to_llvm, Intrinsics};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, FunctionType, StructType},
    values::{BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};
use wasmer_types::CompileError;
use wasmer_types::{FunctionType as FuncSig, Type};
use wasmer_vm::VMOffsets;

use std::convert::TryInto;

/// Implementation of the [`Abi`] trait for the Aarch64 ABI on Linux.
pub struct Aarch64SystemV {}

impl Abi for Aarch64SystemV {
    // Given a function definition, retrieve the parameter that is the vmctx pointer.
    fn get_vmctx_ptr_param<'ctx>(&self, func_value: &FunctionValue<'ctx>) -> PointerValue<'ctx> {
        func_value
            .get_nth_param(u32::from(
                func_value
                    .get_enum_attribute(
                        AttributeLoc::Param(0),
                        Attribute::get_named_enum_kind_id("sret"),
                    )
                    .is_some(),
            ))
            .unwrap()
            .into_pointer_value()
    }

    // Given a wasm function type, produce an llvm function declaration.
    fn func_type_to_llvm<'ctx>(
        &self,
        context: &'ctx Context,
        intrinsics: &Intrinsics<'ctx>,
        offsets: Option<&VMOffsets>,
        sig: &FuncSig,
    ) -> Result<(FunctionType<'ctx>, Vec<(Attribute, AttributeLoc)>), CompileError> {
        let user_param_types = sig.params().iter().map(|&ty| type_to_llvm(intrinsics, ty));

        let param_types =
            std::iter::once(Ok(intrinsics.ctx_ptr_ty.as_basic_type_enum())).chain(user_param_types);

        let vmctx_attributes = |i: u32| {
            vec![
                (
                    context.create_enum_attribute(Attribute::get_named_enum_kind_id("nofree"), 0),
                    AttributeLoc::Param(i),
                ),
                (
                    if let Some(offsets) = offsets {
                        context.create_enum_attribute(
                            Attribute::get_named_enum_kind_id("dereferenceable"),
                            offsets.size_of_vmctx().into(),
                        )
                    } else {
                        context
                            .create_enum_attribute(Attribute::get_named_enum_kind_id("nonnull"), 0)
                    },
                    AttributeLoc::Param(i),
                ),
                (
                    context.create_enum_attribute(
                        Attribute::get_named_enum_kind_id("align"),
                        std::mem::align_of::<wasmer_vm::VMContext>()
                            .try_into()
                            .unwrap(),
                    ),
                    AttributeLoc::Param(i),
                ),
            ]
        };

        Ok(match sig.results() {
            [] => (
                intrinsics.void_ty.fn_type(
                    param_types
                        .map(|v| v.map(Into::into))
                        .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                        .as_slice(),
                    false,
                ),
                vmctx_attributes(0),
            ),
            [_] => {
                let single_value = sig.results()[0];
                (
                    type_to_llvm(intrinsics, single_value)?.fn_type(
                        param_types
                            .map(|v| v.map(Into::into))
                            .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                            .as_slice(),
                        false,
                    ),
                    vmctx_attributes(0),
                )
            }
            [Type::F32, Type::F32] => {
                let f32_ty = intrinsics.f32_ty.as_basic_type_enum();
                (
                    context.struct_type(&[f32_ty, f32_ty], false).fn_type(
                        param_types
                            .map(|v| v.map(Into::into))
                            .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                            .as_slice(),
                        false,
                    ),
                    vmctx_attributes(0),
                )
            }
            [Type::F64, Type::F64] => {
                let f64_ty = intrinsics.f64_ty.as_basic_type_enum();
                (
                    context.struct_type(&[f64_ty, f64_ty], false).fn_type(
                        param_types
                            .map(|v| v.map(Into::into))
                            .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                            .as_slice(),
                        false,
                    ),
                    vmctx_attributes(0),
                )
            }
            [Type::F32, Type::F32, Type::F32] => {
                let f32_ty = intrinsics.f32_ty.as_basic_type_enum();
                (
                    context
                        .struct_type(&[f32_ty, f32_ty, f32_ty], false)
                        .fn_type(
                            param_types
                                .map(|v| v.map(Into::into))
                                .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                                .as_slice(),
                            false,
                        ),
                    vmctx_attributes(0),
                )
            }
            [Type::F32, Type::F32, Type::F32, Type::F32] => {
                let f32_ty = intrinsics.f32_ty.as_basic_type_enum();
                (
                    context
                        .struct_type(&[f32_ty, f32_ty, f32_ty, f32_ty], false)
                        .fn_type(
                            param_types
                                .map(|v| v.map(Into::into))
                                .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                                .as_slice(),
                            false,
                        ),
                    vmctx_attributes(0),
                )
            }
            _ => {
                let sig_returns_bitwidths = sig
                    .results()
                    .iter()
                    .map(|ty| match ty {
                        Type::I32 | Type::F32 => 32,
                        Type::I64 | Type::F64 => 64,
                        Type::V128 => 128,
                        Type::ExternRef | Type::FuncRef => 64, /* pointer */
                    })
                    .collect::<Vec<i32>>();
                match sig_returns_bitwidths.as_slice() {
                    [32, 32] => (
                        intrinsics.i64_ty.fn_type(
                            param_types
                                .map(|v| v.map(Into::into))
                                .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                                .as_slice(),
                            false,
                        ),
                        vmctx_attributes(0),
                    ),
                    [32, 64]
                    | [64, 32]
                    | [64, 64]
                    | [32, 32, 32]
                    | [64, 32, 32]
                    | [32, 32, 64]
                    | [32, 32, 32, 32] => (
                        intrinsics.i64_ty.array_type(2).fn_type(
                            param_types
                                .map(|v| v.map(Into::into))
                                .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                                .as_slice(),
                            false,
                        ),
                        vmctx_attributes(0),
                    ),
                    _ => {
                        let basic_types: Vec<_> = sig
                            .results()
                            .iter()
                            .map(|&ty| type_to_llvm(intrinsics, ty))
                            .collect::<Result<_, _>>()?;

                        let sret = context.struct_type(&basic_types, false);
                        let sret_ptr = sret.ptr_type(AddressSpace::default());

                        let param_types =
                            std::iter::once(Ok(sret_ptr.as_basic_type_enum())).chain(param_types);

                        let mut attributes = vec![(
                            context.create_type_attribute(
                                Attribute::get_named_enum_kind_id("sret"),
                                sret.as_any_type_enum(),
                            ),
                            AttributeLoc::Param(0),
                        )];
                        attributes.append(&mut vmctx_attributes(1));

                        (
                            intrinsics.void_ty.fn_type(
                                param_types
                                    .map(|v| v.map(Into::into))
                                    .collect::<Result<Vec<BasicMetadataTypeEnum>, _>>()?
                                    .as_slice(),
                                false,
                            ),
                            attributes,
                        )
                    }
                }
            }
        })
    }

    // Marshall wasm stack values into function parameters.
    fn args_to_call<'ctx>(
        &self,
        alloca_builder: &Builder<'ctx>,
        func_sig: &FuncSig,
        ctx_ptr: PointerValue<'ctx>,
        llvm_fn_ty: &FunctionType<'ctx>,
        values: &[BasicValueEnum<'ctx>],
    ) -> Vec<BasicValueEnum<'ctx>> {
        // If it's an sret, allocate the return space.
        let sret = if llvm_fn_ty.get_return_type().is_none() && func_sig.results().len() > 1 {
            Some(
                alloca_builder.build_alloca(
                    llvm_fn_ty.get_param_types()[0]
                        .into_pointer_type()
                        .get_element_type()
                        .into_struct_type(),
                    "sret",
                ),
            )
        } else {
            None
        };

        let values = std::iter::once(ctx_ptr.as_basic_value_enum()).chain(values.iter().copied());

        if let Some(sret) = sret {
            std::iter::once(sret.as_basic_value_enum())
                .chain(values)
                .collect()
        } else {
            values.collect()
        }
    }

    // Given a CallSite, extract the returned values and return them in a Vec.
    fn rets_from_call<'ctx>(
        &self,
        builder: &Builder<'ctx>,
        intrinsics: &Intrinsics<'ctx>,
        call_site: CallSiteValue<'ctx>,
        func_sig: &FuncSig,
    ) -> Vec<BasicValueEnum<'ctx>> {
        let split_i64 = |value: IntValue<'ctx>| -> (IntValue<'ctx>, IntValue<'ctx>) {
            assert!(value.get_type() == intrinsics.i64_ty);
            let low = builder.build_int_truncate(value, intrinsics.i32_ty, "");
            let lshr =
                builder.build_right_shift(value, intrinsics.i64_ty.const_int(32, false), false, "");
            let high = builder.build_int_truncate(lshr, intrinsics.i32_ty, "");
            (low, high)
        };

        let casted = |value: BasicValueEnum<'ctx>, ty: Type| -> BasicValueEnum<'ctx> {
            match ty {
                Type::I32 => {
                    assert!(
                        value.get_type() == intrinsics.i32_ty.as_basic_type_enum()
                            || value.get_type() == intrinsics.f32_ty.as_basic_type_enum()
                    );
                    builder.build_bitcast(value, intrinsics.i32_ty, "")
                }
                Type::F32 => {
                    assert!(
                        value.get_type() == intrinsics.i32_ty.as_basic_type_enum()
                            || value.get_type() == intrinsics.f32_ty.as_basic_type_enum()
                    );
                    builder.build_bitcast(value, intrinsics.f32_ty, "")
                }
                Type::I64 => {
                    assert!(
                        value.get_type() == intrinsics.i64_ty.as_basic_type_enum()
                            || value.get_type() == intrinsics.f64_ty.as_basic_type_enum()
                    );
                    builder.build_bitcast(value, intrinsics.i64_ty, "")
                }
                Type::F64 => {
                    assert!(
                        value.get_type() == intrinsics.i64_ty.as_basic_type_enum()
                            || value.get_type() == intrinsics.f64_ty.as_basic_type_enum()
                    );
                    builder.build_bitcast(value, intrinsics.f64_ty, "")
                }
                Type::V128 => {
                    assert!(value.get_type() == intrinsics.i128_ty.as_basic_type_enum());
                    value
                }
                Type::ExternRef | Type::FuncRef => {
                    assert!(value.get_type() == intrinsics.funcref_ty.as_basic_type_enum());
                    value
                }
            }
        };

        if let Some(basic_value) = call_site.try_as_basic_value().left() {
            if func_sig.results().len() > 1 {
                if basic_value.get_type() == intrinsics.i64_ty.as_basic_type_enum() {
                    assert!(func_sig.results().len() == 2);
                    let value = basic_value.into_int_value();
                    let (low, high) = split_i64(value);
                    let low = casted(low.into(), func_sig.results()[0]);
                    let high = casted(high.into(), func_sig.results()[1]);
                    return vec![low, high];
                }
                if basic_value.is_struct_value() {
                    let struct_value = basic_value.into_struct_value();
                    return (0..struct_value.get_type().count_fields())
                        .map(|i| builder.build_extract_value(struct_value, i, "").unwrap())
                        .collect::<Vec<_>>();
                }
                let array_value = basic_value.into_array_value();
                let low = builder
                    .build_extract_value(array_value, 0, "")
                    .unwrap()
                    .into_int_value();
                let high = builder
                    .build_extract_value(array_value, 1, "")
                    .unwrap()
                    .into_int_value();
                let func_sig_returns_bitwidths = func_sig
                    .results()
                    .iter()
                    .map(|ty| match ty {
                        Type::I32 | Type::F32 => 32,
                        Type::I64 | Type::F64 => 64,
                        Type::V128 => 128,
                        Type::ExternRef | Type::FuncRef => 64, /* pointer */
                    })
                    .collect::<Vec<i32>>();

                match func_sig_returns_bitwidths.as_slice() {
                    [32, 64] => {
                        let (low, _) = split_i64(low);
                        let low = casted(low.into(), func_sig.results()[0]);
                        let high = casted(high.into(), func_sig.results()[1]);
                        vec![low, high]
                    }
                    [64, 32] => {
                        let (high, _) = split_i64(high);
                        let low = casted(low.into(), func_sig.results()[0]);
                        let high = casted(high.into(), func_sig.results()[1]);
                        vec![low, high]
                    }
                    [64, 64] => {
                        let low = casted(low.into(), func_sig.results()[0]);
                        let high = casted(high.into(), func_sig.results()[1]);
                        vec![low, high]
                    }
                    [32, 32, 32] => {
                        let (v1, v2) = split_i64(low);
                        let (v3, _) = split_i64(high);
                        let v1 = casted(v1.into(), func_sig.results()[0]);
                        let v2 = casted(v2.into(), func_sig.results()[1]);
                        let v3 = casted(v3.into(), func_sig.results()[2]);
                        vec![v1, v2, v3]
                    }
                    [32, 32, 64] => {
                        let (v1, v2) = split_i64(low);
                        let v1 = casted(v1.into(), func_sig.results()[0]);
                        let v2 = casted(v2.into(), func_sig.results()[1]);
                        let v3 = casted(high.into(), func_sig.results()[2]);
                        vec![v1, v2, v3]
                    }
                    [64, 32, 32] => {
                        let v1 = casted(low.into(), func_sig.results()[0]);
                        let (v2, v3) = split_i64(high);
                        let v2 = casted(v2.into(), func_sig.results()[1]);
                        let v3 = casted(v3.into(), func_sig.results()[2]);
                        vec![v1, v2, v3]
                    }
                    [32, 32, 32, 32] => {
                        let (v1, v2) = split_i64(low);
                        let (v3, v4) = split_i64(high);
                        let v1 = casted(v1.into(), func_sig.results()[0]);
                        let v2 = casted(v2.into(), func_sig.results()[1]);
                        let v3 = casted(v3.into(), func_sig.results()[2]);
                        let v4 = casted(v4.into(), func_sig.results()[3]);
                        vec![v1, v2, v3, v4]
                    }
                    _ => unreachable!("expected an sret for this type"),
                }
            } else {
                assert!(func_sig.results().len() == 1);
                vec![basic_value]
            }
        } else {
            assert!(call_site.count_arguments() > 0); // Either sret or vmctx.
            if call_site
                .get_enum_attribute(
                    AttributeLoc::Param(0),
                    Attribute::get_named_enum_kind_id("sret"),
                )
                .is_some()
            {
                let sret = call_site
                    .try_as_basic_value()
                    .right()
                    .unwrap()
                    .get_operand(0)
                    .unwrap()
                    .left()
                    .unwrap()
                    .into_pointer_value();
                let struct_value = builder.build_load(sret, "").into_struct_value();
                let mut rets: Vec<_> = Vec::new();
                for i in 0..struct_value.get_type().count_fields() {
                    let value = builder.build_extract_value(struct_value, i, "").unwrap();
                    rets.push(value);
                }
                assert!(func_sig.results().len() == rets.len());
                rets
            } else {
                assert!(func_sig.results().is_empty());
                vec![]
            }
        }
    }

    fn is_sret(&self, func_sig: &FuncSig) -> Result<bool, CompileError> {
        let func_sig_returns_bitwidths = func_sig
            .results()
            .iter()
            .map(|ty| match ty {
                Type::I32 | Type::F32 => 32,
                Type::I64 | Type::F64 => 64,
                Type::V128 => 128,
                Type::ExternRef | Type::FuncRef => 64, /* pointer */
            })
            .collect::<Vec<i32>>();

        Ok(!matches!(
            func_sig_returns_bitwidths.as_slice(),
            [] | [_]
                | [32, 32]
                | [32, 64]
                | [64, 32]
                | [64, 64]
                | [32, 32, 32]
                | [32, 32, 64]
                | [64, 32, 32]
                | [32, 32, 32, 32]
        ))
    }

    fn pack_values_for_register_return<'ctx>(
        &self,
        intrinsics: &Intrinsics<'ctx>,
        builder: &Builder<'ctx>,
        values: &[BasicValueEnum<'ctx>],
        func_type: &FunctionType<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, CompileError> {
        let is_32 = |value: BasicValueEnum| {
            (value.is_int_value() && value.into_int_value().get_type() == intrinsics.i32_ty)
                || (value.is_float_value()
                    && value.into_float_value().get_type() == intrinsics.f32_ty)
        };
        let is_64 = |value: BasicValueEnum| {
            (value.is_int_value() && value.into_int_value().get_type() == intrinsics.i64_ty)
                || (value.is_float_value()
                    && value.into_float_value().get_type() == intrinsics.f64_ty)
        };

        let pack_i32s = |low: BasicValueEnum<'ctx>, high: BasicValueEnum<'ctx>| {
            assert!(low.get_type() == intrinsics.i32_ty.as_basic_type_enum());
            assert!(high.get_type() == intrinsics.i32_ty.as_basic_type_enum());
            let (low, high) = (low.into_int_value(), high.into_int_value());
            let low = builder.build_int_z_extend(low, intrinsics.i64_ty, "");
            let high = builder.build_int_z_extend(high, intrinsics.i64_ty, "");
            let high = builder.build_left_shift(high, intrinsics.i64_ty.const_int(32, false), "");
            builder.build_or(low, high, "").as_basic_value_enum()
        };

        let to_i64 = |v: BasicValueEnum<'ctx>| {
            if v.is_float_value() {
                let v = v.into_float_value();
                if v.get_type() == intrinsics.f32_ty {
                    let v = builder
                        .build_bitcast(v, intrinsics.i32_ty, "")
                        .into_int_value();
                    let v = builder.build_int_z_extend(v, intrinsics.i64_ty, "");
                    v.as_basic_value_enum()
                } else {
                    debug_assert!(v.get_type() == intrinsics.f64_ty);
                    let v = builder.build_bitcast(v, intrinsics.i64_ty, "");
                    v.as_basic_value_enum()
                }
            } else {
                let v = v.into_int_value();
                if v.get_type() == intrinsics.i32_ty {
                    let v = builder.build_int_z_extend(v, intrinsics.i64_ty, "");
                    v.as_basic_value_enum()
                } else {
                    debug_assert!(v.get_type() == intrinsics.i64_ty);
                    v.as_basic_value_enum()
                }
            }
        };

        let build_struct = |ty: StructType<'ctx>, values: &[BasicValueEnum<'ctx>]| {
            let mut struct_value = ty.get_undef();
            for (i, v) in values.iter().enumerate() {
                struct_value = builder
                    .build_insert_value(struct_value, *v, i as u32, "")
                    .unwrap()
                    .into_struct_value();
            }
            struct_value.as_basic_value_enum()
        };

        let build_2xi64 = |low: BasicValueEnum<'ctx>, high: BasicValueEnum<'ctx>| {
            let low = to_i64(low);
            let high = to_i64(high);
            let value = intrinsics.i64_ty.array_type(2).get_undef();
            let value = builder.build_insert_value(value, low, 0, "").unwrap();
            let value = builder.build_insert_value(value, high, 1, "").unwrap();
            value.as_basic_value_enum()
        };

        Ok(match *values {
            [one_value] => one_value,
            [v1, v2]
                if v1.is_float_value()
                    && v2.is_float_value()
                    && v1.into_float_value().get_type() == v2.into_float_value().get_type() =>
            {
                build_struct(
                    func_type.get_return_type().unwrap().into_struct_type(),
                    &[v1, v2],
                )
            }
            [v1, v2] if is_32(v1) && is_32(v2) => {
                let v1 = builder.build_bitcast(v1, intrinsics.i32_ty, "");
                let v2 = builder.build_bitcast(v2, intrinsics.i32_ty, "");
                pack_i32s(v1, v2)
            }
            [v1, v2] => build_2xi64(v1, v2),
            [v1, v2, v3]
                if is_32(v1)
                    && is_32(v2)
                    && is_32(v3)
                    && v1.is_float_value()
                    && v2.is_float_value()
                    && v3.is_float_value() =>
            {
                build_struct(
                    func_type.get_return_type().unwrap().into_struct_type(),
                    &[v1, v2, v3],
                )
            }
            [v1, v2, v3] if is_32(v1) && is_32(v2) => {
                let v1 = builder.build_bitcast(v1, intrinsics.i32_ty, "");
                let v2 = builder.build_bitcast(v2, intrinsics.i32_ty, "");
                let v1v2_pack = pack_i32s(v1, v2);
                build_2xi64(v1v2_pack, v3)
            }
            [v1, v2, v3] if is_64(v1) && is_32(v2) && is_32(v3) => {
                let v2 = builder.build_bitcast(v2, intrinsics.i32_ty, "");
                let v3 = builder.build_bitcast(v3, intrinsics.i32_ty, "");
                let v2v3_pack = pack_i32s(v2, v3);
                build_2xi64(v1, v2v3_pack)
            }
            [v1, v2, v3, v4]
                if is_32(v1)
                    && is_32(v2)
                    && is_32(v3)
                    && is_32(v4)
                    && v1.is_float_value()
                    && v2.is_float_value()
                    && v3.is_float_value()
                    && v4.is_float_value() =>
            {
                build_struct(
                    func_type.get_return_type().unwrap().into_struct_type(),
                    &[v1, v2, v3, v4],
                )
            }
            [v1, v2, v3, v4] if is_32(v1) && is_32(v2) && is_32(v3) && is_32(v4) => {
                let v1 = builder.build_bitcast(v1, intrinsics.i32_ty, "");
                let v2 = builder.build_bitcast(v2, intrinsics.i32_ty, "");
                let v1v2_pack = pack_i32s(v1, v2);
                let v3 = builder.build_bitcast(v3, intrinsics.i32_ty, "");
                let v4 = builder.build_bitcast(v4, intrinsics.i32_ty, "");
                let v3v4_pack = pack_i32s(v3, v4);
                build_2xi64(v1v2_pack, v3v4_pack)
            }
            _ => {
                unreachable!("called to perform register return on struct return or void function")
            }
        })
    }
}
