/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

/// Helper macro to define binary arithmetic instances for reference
/// combinations, assuming an instance for the case when both are references
/// exists.
macro_rules! instances {
    ($name:ident, $fn_name:ident, $lhs:ty, $rhs:ty) => {
        impl $name<&$rhs> for $lhs {
            type Output = <&'static $lhs as $name<&'static $rhs>>::Output;
            fn $fn_name(self, rhs: &$rhs) -> Self::Output {
                (&self).$fn_name(rhs)
            }
        }

        impl $name<$rhs> for &$lhs {
            type Output = <&'static $lhs as $name<&'static $rhs>>::Output;
            fn $fn_name(self, rhs: $rhs) -> Self::Output {
                self.$fn_name(&rhs)
            }
        }

        impl $name<$rhs> for $lhs {
            type Output = <&'static $lhs as $name<&'static $rhs>>::Output;
            fn $fn_name(self, rhs: $rhs) -> Self::Output {
                (&self).$fn_name(&rhs)
            }
        }
    };
}

pub(super) use instances;
